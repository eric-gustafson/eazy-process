
(in-package :eazy-process)

(defmethod alog ((str string))
  (syslog:log "ezyprc" :user :warning str))

(defstruct pipe
  (read 0 :type fixnum)
  (write 1 :type fixnum))
  
(defun pipe ()
  (multiple-value-bind (read write) (isys:pipe)
    (make-pipe :read read :write write)))

(defun close-pipe (pipe)
  (isys:close (pipe-read pipe))
  (isys:close (pipe-write pipe)))
