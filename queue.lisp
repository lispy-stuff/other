(defpackage queue
  (:export dequeue enqueue make-queue queue-length requeue)
  (:use common-lisp clite))

(in-package queue)

(defun dequeue (queue &optional item)
  "Remove ITEM (or last item if not specified) from QUEUE"
  (if item
      (setf (cdr item) (cdr (cdr item)))
      (pop (car queue))))

(defun enqueue (value queue)
  "Put VALUE in QUEUE and return ITEM"
  (if (null (car queue))
      (progn
        (setf (cdr queue) (setf (car queue) (list value)))
        nil)
      (let ((prev (cdr queue)))
        (setf (cdr (cdr queue)) (list value)
              (cdr queue) (cdr (cdr queue)))
        prev)))

(defun make-queue ()
  "Create new QUEUE"
  (cons nil nil))

(defun queue-length (queue)
  "Return length of QUEUE"
  (length (car queue)))

(defun requeue (item queue)
  "Move ITEM to top of QUEUE"
  (let ((value (if item (car (cdr item)) (car (car queue)))))
    (if item
        (dequeue queue item)
        (setf (car queue) (cdr (car queue))))
    (enqueue value queue)))

(test (:queue)
  (let* ((q (make-queue))
        (item (enqueue 1 q)))
    (test-eq (enqueue 2 q) (car q))
    (test-eq (enqueue 3 q) (cdr (car q)))
    (requeue item q)
    (test= (queue-length q) 3)
    (test= (dequeue q) 2)
    (test= (dequeue q) 3)
    (test= (dequeue q) 1)))
