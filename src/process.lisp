
(in-package :eazy-process.impl)

(defclass process ()
  ((#:pid :reader pid :initarg :pid))
  (:documentation
   "A class representing a process.
Properties of a process are accessible through several functions."))

(defun %make-process (pid)
  (let ((process (make-instance 'process :pid pid)))
    (trivial-garbage:finalize
     process
     (lambda ()
       (ignore-errors ; in case pid has already dead
         (iter (for n below 5)
               (kill pid 15)
               (sleep (expt 2 n)))
         (kill pid 9)
         (waitpid pid iolib/syscalls:WNOHANG))))
    process))


;; Note: without calling waitpid, the child becomes a zombie process.
;; child process should be waited when the process object is GC'ed.

(defun wait (process &optional option)
  "option is one of :nohang, :untraced, :continued.
Returns (value (boolean exited-p) (integer exitstatus) (integer waitpid-status)).
For the further investigation of waitpid-status, use iolib/syscalls:WIFSIGNALED etc."
  (let ((status
         (waitpid (pid process)
                  (case option
                    (:nohang iolib/syscalls:WNOHANG)
                    (:untraced iolib/syscalls:WUNTRACED)
                    (:continued iolib/syscalls:WCONTINUED)
                    (t 0)))))
    (values (iolib/syscalls:WIFEXITED status)
            (iolib/syscalls:WEXITSTATUS status))))

;; pwdx

