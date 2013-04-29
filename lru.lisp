(defpackage lru
  (:export cached-value make-cache memoize)
  (:use common-lisp core clite queue))

(in-package lru)

(defclass cache ()
  ((capacity :initarg :capacity :initform 0)
   (queue :initform (make-queue))
   (table :initarg :table)
   (test :initarg :test)))

(defun make-cache (&key capacity (test #'eql))
  "Create new LRU cache"
  (make-instance 'cache
                 :capacity capacity
                 :table (make-hash-table :test test)
                 :test test))

(defun cached-value (key cache)
  "Return cached VALUE for KEY"
  (multiple-value-bind (value found) (gethash key (slot-value cache 'table))
    (values (car value) found)))

(defun (setf cached-value) (new-val key cache)
  "Store VALUE with KEY in CACHE and discard least used if > CAPACITY"
  (with-slots (capacity queue table) cache
    (multiple-value-bind (curr-val found) (gethash key table)
      (setf (gethash key table) (cons new-val
                                      (if found
                                          (requeue (cdr curr-val) queue)
                                          (enqueue key queue)))))
    (when (and capacity (> (queue-length queue) capacity))
      (remhash (dequeue queue) table))))

(defun memoize (fn capacity &rest args)
  "Memoize FN using LRU keyed on ARGS"
  (let ((cache (apply #'make-cache capacity :test #'equal args)))
    #'(lambda (&rest args)
        (multiple-value-bind (res found) (cached-value args cache)
          (if found
              res
              (setf (cached-value args cache)
                    (apply fn args)))))))

(test (:lru)
    (let ((c (make-cache :capacity 2 :test #'equal)))
      (testp (null (cached-value "abc" c)))
      (setf (cached-value "abc" c) 123)
      (test= (cached-value "abc" c) 123)
      (setf (cached-value "def" c) 456)
      (setf (cached-value "abc" c) 123)
      (setf (cached-value "ghi" c) 789)
      (test= (cached-value "abc" c) 123)
      (testp (null (cached-value "def" c))
             "Least recently used value is removed when full")
      (test= (cached-value "ghi" c) 789)))
