(in-package :eazy-process.impl)

#|

subshell implemented by fork-exec

|#

(defparameter *interpreter* "sh -c"
  "Command line string which is invoked in order to run the subshell. Default value is \"sh -c\".
The value is then followed by the command specified in shell-command, e.g. ,
when command = 'ls -la', the running command is \"sh -c 'ls -la'\".
It provides the ability to call the interpreters like perl/AWK easily.

The name/path of the interpreter must be separated by spaces from the options like '-c'.
The process is forked, then the child process calls execvp with the name of the interpreter.
Therefore, the actual pathname of the intepreter can be resolved using PATH environment variable.
")

(defun shell (command)
  "Asynchronously execute `command` using an interpreter, returns a process structure object.
The `command' is a valid script in the interpreter specified in `*interpreter*'.

On error during system call, iolib/syscalls:syscall-error is signalled."
  (format t "; ~a '~a'" *interpreter* command)
  (let ((pid (fork)))
    ;; On success, the PID of the child process is returned in the parent,
    ;; and 0 is returned in the child.  On failure, -1 is returned in the
    ;; parent, no child process is created, and errno is set appropriately.
    (cond
      ((zerop pid)
       ;; child
       (%exec command))
      ;; ((= -1 pid) ;; this is 
      ;;  ;; failure
      ;;  (%failure command))
      (t
       ;; parent
       (%make-process pid)))))

(defun %exec (command)
  (print "in child process")
  (let* ((argv (append (split " +" *interpreter*) (list command)))
         _strings)
    (unwind-protect
         (progn
           (setf _strings (foreign-alloc :string
                                         :initial-contents argv
                                         :null-terminated-p t))
           (execvp (first argv) _strings)) ; does not return on success
      (foreign-free _strings)
      (foreign-funcall "_exit" :int -1))))

;; Note:
;; IMPL> (cffi::canonicalize-foreign-type :string)
;; :POINTER

;; Note: allocated memory is automatically freed and get reclaimed by the
;; OS when exec is called successfully, because the data segment = heap is replaced.

