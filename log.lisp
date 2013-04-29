(defpackage log
  (:export disable enable enabledp write-entry)
  (:use common-lisp core))

(in-package log)

(defparameter enabled-tags nil)

(defun disable ()
  (setf enabled-tags nil))

(defun enable (&rest tags)
  (setf enabled-tags tags))

(defun enabledp ()
  enabled-tags)

(defmacro write-entry (tags message &rest args)
  `(when (or (eq (car enabled-tags) t)
             (null (set-difference enabled-tags ',tags)))
     (format t "~&~S ~A~%" ',tags (format nil ,message ,@args))))
