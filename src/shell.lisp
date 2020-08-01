#|

subshell implemented with fork-execvp

|#

;;; shell

(in-package :eazy-process)

(defun shell2 (argv &key
		      (fdspecs '#.+fdspecs-default+)
		      environments
		      uid
		      gid
		      )
  (let ((fdspecs (mapcar #'canonicalize-fdspec fdspecs (iota (length fdspecs)))))
    (let ((pid (fork)))
      (cond
        ;; ((= -1 pid) ;; this is already handled by iolib, so don't care
        ;;  (%failure command))
        ((zerop pid)
	 (when (numberp gid) (iolib/syscalls:setgid gid))
	 (when (numberp uid) (iolib/syscalls:setuid uid))
         (%in-child fdspecs argv environments t))
        (t
         (%in-parent fdspecs pid)))))
  )

(defun shell (argv &optional
                     (fdspecs '#.+fdspecs-default+)
                     environments
		     (search t)
		     )
  (let ((fdspecs (mapcar #'canonicalize-fdspec fdspecs (iota (length fdspecs)))))
    (let ((pid (fork)))
      (cond
        ;; ((= -1 pid) ;; this is already handled by iolib, so don't care
        ;;  (%failure command))
        ((zerop pid)
         (%in-child fdspecs argv environments search))
        (t
         (%in-parent fdspecs pid))))))

(defun %in-parent (fdspecs pid)
  (%make-process
   pid
   (iter (for i from 0)
         (for (parent . child) in fdspecs)
         (collect (funcall parent i) result-type vector))))

(defun %in-child (fdspecs argv environments search)
  "if the child process died of errno, the exit value is that value"
         (handler-case
             (progn
               (iter (for kind from 0)
                     (for value in-vector *rlimit-resources*)
                     (when (plusp value)
                       (setf (rlimit kind) value)))
               (iter (for i from 0)
                     (for (parent . child) in fdspecs)
                     (funcall child i))
               (%exec argv environments search))
           (isys:syscall-error (c)
             (declare (ignorable c))
             (if (numberp (isys:code-of c))
                 (foreign-funcall "_exit" :int (isys:code-of c))
                 (foreign-funcall "_exit" :int 203)))
           (error (c)
             (declare (ignorable c))
             (foreign-funcall "_exit" :int 202))))

;; Patch ffi-functions-unix.lisp
(in-package :iolib/syscalls)

(cl:defconstant sc-open-max 4)
(export 'sc-open-max)

(defsyscall (sysconf "sysconf") :int
  "Get system configuration info"
  (name :int))

(export 'sysconf)  

(in-package :eazy-process)	    

(defvar *daemonize* nil
  "Special variable to cause daemonization logic to engage between teh fork/exec"
  )

(export '*daemonize*)

(defun daemonize-close-all-files! ()
  (handler-case
      (let ((n (iolib.syscalls::sysconf iolib.syscalls::sc-open-max)))
	(loop :for i :from 0 :upto n :do
	  (ignore-errors
	   (iolib.syscalls:close i))))
    (t (c)
      (format *error-output* "~a~&" c)))
  )

(defun %exec (argv env search)
  (map nil #'%setenv env)
  (alog (format nil "shell.lisp: daemonize=~a~%" *daemonize*))
  (when *daemonize*
    (daemonize-close-all-files!))
  (if search
      (%execvp argv)
      (%execv argv)))

(defun make-c-char* (list-of-string)
  (foreign-alloc :string
                 :initial-contents list-of-string
                 :null-terminated-p t))

(defun %setenv (pair)
  (match pair
    ((or (cons name value)
         (ppcre "([^=]*)=(.*)" name value))
     (isys:setenv name value t))))

(defun %execvp (argv)
  (let (_argv)
    (unwind-protect
         (progn
           (setf _argv (make-c-char* argv))
           (execvp (first argv) _argv)) ; does not return on success
      (when _argv (foreign-free _argv)))))



(defun %execv (argv)
  (let (_argv)
    (unwind-protect
         (progn
           (setf _argv (make-c-char* argv))
           (execv (first argv) _argv)) ; does not return on success
      (when _argv (foreign-free _argv)))))

;; Note:
;; IMPL> (cffi::canonicalize-foreign-type :string)
;; :POINTER

;; Note: allocated memory is automatically freed and get reclaimed by the
;; OS when exec is called successfully, because the data segment = heap is replaced.


;;; documentation

(setf (documentation 'shell 'function)
      "Asynchronously execute `argv' using fork(2) and `execve'
 family of system calls, returns a process structure object.

ARGV is a sequence of strings. Each string is converted to null-terminated
C strings char* ARGV and passed to execvp.  The first element is also
passed to execvp as the pathname of the executable.

FDSPECS is a sequence specifying how the child process should handle its output.
For the documentation of fd-specifier see `+fdspecs-default+'.
Example: (:in :out :out) , (5 :out 8)

Each element in FDSPECS corresponds to each file descriptor of the child process.

When an i-th element of FDSPECS is a symbol, then a new pipe(2) is created for each i.
The i-th fd of the child process p1 is associated with the correct end of the new pipe,
the other end of the pipe is readable/writable from lisp from the process interface.
 (see the documentation of class PROCESS.)

When an i-th element of FDSPECS is an integer j,
then it should be the lisp end of a pipe connected to some other child process p2.
 (not the normal fd of the lisp process, like 0,1,2)
The i-th fd of p1 is connected to this pipe,
then p1 and p2 can exchange the data each other.
When this happens, lisp process should not read from this pipe
 (or the data will not be sent to the destination of the pipe).
For that purpose, add a layer like `tee'.

When ENVIRONMENT is specified, it is passed to execve/execvpe.
It should be a list of strings (\"NAME1=VALUE1\" \"NAME2=VALUE2\")
or an alist ((\"NAME1\" . \"VALUE1\") ...).
When SEARCH is t, it uses execvp/execvpe. (==executable is searched through PATH)

On error during system call in the parent process, iolib.syscalls:syscall-error is signalled.
In the child process, the system call failure result in error status 203.
FIXME: this might not be good.
")

(defun exec (csexp &key
		     input
 		     uid
		     gid)
  "runs a unix cmd (csexp), which is a sequence of strings using the eazy-process
shell function.  This returns the values (output-str error-str
exit-status)"
  (declare (type list csexp))
  (let ((cmd (with-output-to-string (out)
		(loop :for pre = nil :then " "
		      :for str :in csexp :do
		     (if pre (princ pre out))
		     (princ str out)))))
    (let ((p1 (shell2 csexp :uid uid :gid gid)))
      (when input (fd-input-from-string p1 0 input))
      ;;(setf (iolib.syscalls:fd-nonblock-p (eazy-process:fd p1 0)) t)
      #+nil(handler-case
	  (iolib.syscalls:close (eazy-process:fd p1 0))
	(isys:syscall-error (c)
	  (format *error-output* "close error ~a~&" c)))
      (let ((wait-sig (eazy-process:wait p1)))
	(optima:match
	    wait-sig
	  ((list exited? exit-status signalled? termsig coredump? stopped? stopsig continued? status)
	   (let ((out-str (fd-output-as-string p1 1))
		 (err-str (fd-output-as-string p1 2)))
	     (mark-process-wait p1)
	     (finalize-process p1)
	     (tg:cancel-finalization p1)
	     (values out-str err-str exit-status cmd)))
	  (otherwise
	   (error "exec returned an unexpected value ~a~&" wait-sig))))
      )
    )
  )

(defun is-run (str)
  "inferior shell run: takes a string command, use this to make
porting a little easier.  This is not meant to be a replacement for
inferior shell, it will just help in the conversion."
  (exec (ppcre:split "\\s+" str)))
