
(in-package :eazy-process)

(defclass process ()
  ((#:pid :reader pid :initarg :pid)
   (#:fds :reader %fds :initarg :fds))
  (:documentation
   "A class representing a process."))

(defvar *pid-wait-mark-table* (make-hash-table)
  "Mark a pid if we waited for the process.  The finalizer can then
  skip this step, and we don't get the warning on the trace-output."
  )


(defgeneric mark-process-wait (obj)
  (:documentation "A mark that we have already successfully waited for
   this process.")
  (:method ((obj process))
    (setf (gethash (pid obj) *pid-wait-mark-table*) t))
  )
  
(defmethod print-object ((p process) s)
  (print-unreadable-object (p s :type t)
    (format s "PID: ~A" (pid p))))

(defun fds (process)
  "Returns an fresh array of fds available from lisp process."
  (copy-array (%fds process)))

(defun %make-process (pid fds)
  (let* ((process (make-instance 'process
                                 :pid pid
                                 :fds fds)))
    (trivial-garbage:finalize
     process
     (lambda () (%finalize-process pid 15 fds)))
    process))

(defun %finalize-process (pid sig fds)
  "True finalizer of a process object. However,
This function should not contain reference to the process itself
because it prevents process object from GC-ing."
  (map nil #'%close-fd-safely fds)
  (cond
    ((gethash pid *pid-wait-mark-table*)
     (remhash pid *pid-wait-mark-table*)
     )
    (t
     (handler-case			; in case pid does not exist
	 (when (zerop (waitpid pid iolib.syscalls:WNOHANG))
           (warn "Killing ~a" pid)
           (kill pid sig)
           (when (zerop (waitpid pid iolib.syscalls:WNOHANG))
             (warn "Force killing ~a" pid)
             (kill pid 9)
             (waitpid pid 0)))
       (iolib.syscalls:syscall-error (c)
	 (declare (ignore c))
	 (warn "Process ~a does not exist -- maybe already killed?" pid)
	 nil)))
    )
  )

(defun %close-fd-safely (fd)
  (when fd
    (handler-case
        (isys:close fd)
      (isys:syscall-error (c)
        (unless (= isys:EBADF (isys:code-of c))
          (signal c))))))


(defun finalize-process (process &optional (sig 15))
  "Waitpid the process. If the process is alive, kill it with SIG first,
then with SIGKILL."
  (%finalize-process (pid process) sig (fds process)))

;; Note: without calling waitpid, the child becomes a zombie process.
;; child process should be waited when the process object is GC'ed.

(defun wait (process &optional option)
  "option is one of :nohang, :untraced, :continued.
Returns a value of the following signature:

 (list (boolean ifexited)
       (integer exitstatus)
       (boolean ifsignalled)
       (integer termsig)
       (boolean coredump)
       (boolean ifstopped)
       (integer stopsig)
       (boolean ifcontinued)
       (integer status)).

When the value is inappropriate as defined in man wait(2),
some integer values may return NIL.
When :nohang is specified but no child has changed its state, 
then it returns NIL instead.
`wait(0)', i.e. wait for any children, is not available.
"
  (multiple-value-bind (retval status)
      (waitpid (pid process)
               (case option
                 (:nohang iolib.syscalls:WNOHANG)
                 (:untraced iolib.syscalls:WUNTRACED)
                 (:continued iolib.syscalls:WCONTINUED)
                 (t 0)))
    (when (plusp retval) ;; nohang and some child has changed its state
      (list (iolib.syscalls:WIFEXITED status)
            (when (iolib.syscalls:WIFEXITED status)
              (iolib.syscalls:WEXITSTATUS status))
            (iolib.syscalls:WIFSIGNALED status)
            (when (iolib.syscalls:WIFSIGNALED status)
              (iolib.syscalls:WTERMSIG status))
            (when (iolib.syscalls:WIFSIGNALED status)
              (iolib.syscalls:WCOREDUMP status))
            (iolib.syscalls:WIFSTOPPED status)
            (when (iolib.syscalls:WIFSTOPPED status)
              (iolib.syscalls:WSTOPSIG status))
            (iolib.syscalls:WIFCONTINUED status)
            status))))

(defun fd (process n)
  "Returns the file descriptor of the lisp process.
The returned fd is connected to the n-th fd of the child process through a pipe.
Example:

 (fd P 1) ; --> 5 

This means that the 5'th fd of the lisp process is connected to the 1st fd of the process P."
  (aref (fds process) n))

(defun fd-as-pathname (process fd)
  "Return the pathname for each file descriptor.
Lisp can read/write to each fd by opening this file.

Since the buffer size of file-stream is implementation-dependent, and the
call to cl:read might cache a considerable amount of characters from the
file, we are not able to ensure that the same file can be opened more than once.

Even if you just cl:open the file and just cl:read-char the stream,
it might read the entire characters of the file into the buffer as a side-effect.
We just advise you not to open the file more than once.
"
  (pathname (uiop:ensure-pathname (format nil "/dev/fd/~a" (fd process fd)) :resolve-symlinks t)))


;; The issue with the ccl-bin that I found, isn't with unix but rather
;; seems to be how it's opening the low level file and trying to map
;; LISP permissions to POSIX/Unix permisssions.  There are a lot of
;; lisps, and mapping :if-not-exist. :if-exists ... to O_CREAT, O_RDWR
;; ... sounds hard, so this attempt is to keeps things 'UNIXY' at the
;; lowest levels and to turn things into lisp objects as fast as
;; possible.
(defun fd-output-as-string (obj fd-num)
  "(blocking)Reads all of the output from the file descriptor; like stdout and
stderr, and returns a lisp string."
  (let* ((dup-fd (iolib.syscalls:open (format nil "~a" (eazy-process:fd-as-pathname obj fd-num))
				      iolib.syscalls:o-rdonly))
	 (n 0)
	 (blksz 1024)
	 (buff (cffi:foreign-alloc :char :count blksz))
	 (results '()))
    (unwind-protect
	 (with-output-to-string (out)
	   (loop :do
	      ;; I think syscalls:read handles restarts, we don't need
	      ;; to worry about eintr and the like
		(setf n (iolib.syscalls:read dup-fd buff blksz))
		(cond
		  ((> n 0)
		   (loop :for i :from 0 :below n :do
			(write-char (code-char (cffi:mem-aref buff :char i)) out)))
		  (t
		   (loop-finish)))
		))
      (progn
	(cffi:foreign-free buff)
	(%close-fd-safely dup-fd)	
	))))

(defun fd-input-from-string (obj fd-num strbuff)
  "(blocking)write to the fd of the process on fd-num"
  (let* ((dup-fd (iolib.syscalls:open (format nil "~a" (eazy-process:fd-as-pathname obj fd-num))
					iolib.syscalls:o-wronly))
	   (n (length strbuff))
	   (cbuff (cffi:foreign-alloc :char :count n)))
      (unwind-protect
	   (progn
	     (loop
	      :for idx :from 0
	      :for c :across strbuff :do
		(setf (cffi:mem-aref cbuff :char idx) (char-code c)))
	     (iolib.syscalls:write dup-fd cbuff n))
	(progn
	  (cffi:foreign-free cbuff)
	  (%close-fd-safely dup-fd)		  
	  )
	))
    )
	   
