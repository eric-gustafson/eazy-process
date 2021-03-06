

(in-package :eazy-process)

#|

limit the process hierarchically

Trying to make the library compatible among unix oses and bsd,
it requires libbsd library

|#

;;; proc

(defun proc (process &rest subdir)
  "Compose a pathname based on the given arguments. Return (values pathname existsp)
where existsp shows if the file or the directory exists.
`process' can be either a process object, :self, or a fixnum of a pid."
  (let ((path (format nil "~a/~(~a~{/~a~}~)"
                      *procfs-root*
                      (ematch process
                        (:self :self)
                        ((type fixnum) process)
                        ((process) (pid process)))
                      subdir)))
    (values path (probe-file path))))

;;; result of ls /proc/self/ in linux 3.13.0-43-generic #72-Ubuntu
;;; attr        cmdline          environ  io         maps        net        oom_score_adj  sched      stat     timers
;;; autogroup   comm             exe      latency    mem         ns         pagemap        schedstat  statm    uid_map
;;; auxv        coredump_filter  fd       limits     mountinfo   numa_maps  personality    sessionid  status   wchan
;;; cgroup      cpuset           fdinfo   loginuid   mounts      oom_adj    projid_map     smaps      syscall
;;; clear_refs  cwd              gid_map  map_files  mountstats  oom_score  root           stack      task

(defun awk (path regex per-line-fn)
  (with-open-file (stream path)
    (iter (for line = (read-line stream nil nil))
          (while line)
          (multiple-value-bind (mstart mend rstarts rends)
              (scan regex line)
            (declare (ignore mend))
            (when mstart
              (apply per-line-fn
                     (iter (for rs in-vector rstarts)
                           (for re in-vector rends)
                           (collect
                               (subseq line rs re)))))))))

(defun io (process &optional field)
  "Returns the corresponding value of the field in /proc/[pid]/io .
If field is not specified, it returns an alist.
Available fields are defined in `+io-keywords+` .
Example: (io :self :rchar) "
  (if field
      (unless (awk (proc process :io)
                   "(.*): +(.*)"
                   (lambda (name var)
                     (when (string-equal field name)
                       (return-from io (parse-integer var)))))
        (error "no field ~a exists!" field))
      (let (acc)
        (awk (proc process :io)
             "(.*): +(.*)"
             (lambda (name var)
               (push (cons (make-keyword (string-upcase name))
                           (parse-integer var)) acc)))
        (nreverse acc))))

(defun statm (process &optional field)
  "Returns the corresponding value of the field in /proc/[pid]/statm .
If field is not specified, it returns an alist.
Available fields are defined in `+statm-keywords+` .
Example: (statm :self :resident) "
  (if field
      (block nil
        (with-open-file (s (proc process :statm))
          (labels ((skip () (unless (char= #\Space (read-char s nil nil)) (skip)))
                   (read-field-or-skip (target)
                     (if (string-equal target field)
                         (return (read s))
                         (skip))))
            (map nil #'read-field-or-skip #.+statm-keywords+)
            (error "invalid field name specified: ~a" field))))
      (with-open-file (s (proc process :statm))
        (map 'list (lambda (field) (cons field (read s))) #.+statm-keywords+))))

(defun stat (process &optional field)
  "Returns the corresponding value of the field in /proc/[pid]/statm .
If field is not specified, it returns an alist.
Available fields are defined in `+stat-keywords+` .
Example: (statm :self :rss) "
  (if field
      (block nil
        (with-open-file (s (proc process :stat))
          (labels ((skip () (unless (char= #\Space (read-char s nil nil)) (skip)))
                   (read-field-or-skip (target)
                     (if (string-equal target field)
                         (return (read s))
                         (skip))))
            (map nil #'read-field-or-skip #.+stat-keywords+)
            (error "invalid field name specified: ~a" field))))
      (with-open-file (s (proc process :stat))
        (map 'list (lambda (field) (cons field (read s))) #.+stat-keywords+))))


