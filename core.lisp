(defpackage core
  (:export assq
           compose
           defconst defmac dolist-reverse doplist do-while do-with do-until
           filter flatten
           kw
           let-if
           mapcars maptree memq mexpand
           plist-remove-if prob
           rassq remove-tree
           str sym sym<)
  (:use common-lisp))

(in-package core)

(defmacro assq (it alst &key key)
  "Return assoc of IT in ALST using #'eq for comparing"
  `(assoc ,it ,alst :key ,key :test #'eq))

(defun compose (&rest fns)
  "Return composed function of FNS"
  (if fns
      (let ((fn1 (car (last fns)))
            (fns (butlast fns)))
        #'(lambda (&rest args)
            (reduce #'funcall fns
                      :from-end t
                      :initial-value (apply fn1 args))))
      #'identity))

(defmacro defconst (name value &optional doc)
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc))))

(defun flatten (x)
  "Turn X into a flat list"
  (labels ((rec (x acc)
             (cond ((null x) acc)
                   ((atom x) (cons x acc))
                   (t (rec (car x) (rec (cdr x) acc))))))
    (rec x nil)))

(defmacro defmac (name args &body body)
  "Define macro with automagic GENSYM of $-symbols"
  (labels ((gensym-p (s)
             (when (symbolp s)
               (let ((n (symbol-name s)))
                 (and (> (length n) 1)
                      (char= (char n 0) #\$))))))
    (let ((syms
           (remove-duplicates (remove-if-not #'gensym-p (flatten body)))))
      `(defmacro ,name ,args
         (let ,(mapcar
                (lambda (s)
                  `(,s (gensym ,(subseq (symbol-name s) 1))))
                syms)
           ,@body)))))

(defmacro dolist-reverse ((var lst) &body body)
  "Perform BODY with VAR bound to items in reversed LST"
  (let (($i (gensym)) ($rev (gensym)))
    `(let ((,$rev (coerce ,lst 'vector)))
       (do ((,$i (1- (array-total-size ,$rev)) (decf ,$i)))
           ((< ,$i 0))
         (let ((,var (aref ,$rev ,$i)))
           ,@body)))))

(defmacro doplist ((key value lst) &body body)
  "Perform BODY with KEY and VALUE bound to keys and values in LST"
  (let (($tail (gensym)))
    `(symbol-macrolet ((,key (car ,$tail)) (,value (cadr ,$tail)))
       (do ((,$tail ,lst (cddr ,$tail)))
           ((null ,$tail))
         ,@body))))

(defmacro do-while (test &body body)
  "Execute body while cond is not NIL"
 `(do () ((not ,test)) ,@body))

(defmacro do-with ((var expr) &body body)
  "Execute body while cond is NIL"
  `(do () ()
     (let ((,var ,expr))
       ,@body)))

(defmacro do-until (test &body body)
  "Execute body while cond is NIL"
  `(do () (,test) ,@body))

(defmacro let-if ((var expr) &body body)
  `(let ((,var ,expr))
     (when ,var ,@body)))

(defun filter (fn lst)
  "Call FN for each item in LST and return results that are not (EQ SKIP)"
  (let ((acc nil))
    (dolist (x lst)
      (let* ((skip (gensym))
             (v (funcall fn x)))
        (unless (eq v skip) (push v acc))))
    (nreverse acc)))

(defun str (&rest args)
  "Generate string from ARGS"
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun kw (&rest args)
  "Generate keyword from ARGS"
  (intern (string-upcase (apply #'str args)) "KEYWORD"))

(defun mapcars (fn &rest lsts)
  "Map FN over all items in all LSTS and return the results"
  (let ((result nil))
    (dolist (l lsts)
      (dolist (x l)
        (push (funcall fn x) result)))
    (nreverse result)))

(defun maptree (fn &rest trees)
  "Map FN over all items in all TREES and return the results"
  (if (some #'atom trees)
      (apply fn trees)
      (apply #'mapcar
               #'(lambda (&rest args)
                   (apply #'maptree fn args))
                 trees)))

(defmacro memq (it lst &key key)
  "Return sublist of LST starting with IT using #'eq for comparing"
  `(member ,it ,lst :key ,key :test #'eq))

(defmacro mexpand (expr)
  "Pretty print macro expansion of expr"
  `(pprint (macroexpand-1 ',expr)))

(defun plist-remove-if (pred plst)
  "Call PRED for each item in PLST and return items for which PRED returns NIL"
  (let (acc)
    (doplist (k v plst)
      (unless (funcall pred k)
        (push k acc)
        (push v acc)))
    (nreverse acc)))

(defun prob (&rest args)
  "Calculate combined probability from ARGS"
  (/ (apply #'* args)
     (+ (apply #'* args)
        (apply #'* (mapcar (lambda (n) (- 1 n)) args)))))

(defmacro rassq (it alst &key key)
  "Return rassoc of IT in ALST using #'eq for comparing"
  `(rassoc ,it ,alst :key ,key :test #'eq))

(defun remove-tree (test tree)
  "Return tree with all values for which TEST returns T removed"
  (labels ((rec (tree acc)
             (cond ((null tree) (nreverse acc))
                   ((consp (car tree))
                    (rec (cdr tree)
                         (cons (rec (car tree) nil) acc)))
                   (t (rec (cdr tree)
                           (if (funcall test (car tree))
                               acc
                               (cons (car tree) acc)))))))
    (rec tree nil)))

(defun sym (&rest args)
  "Generate symbol from ARGS"
  (values (intern (apply #'str args))))

(defun sym< (x y)
  "Return TRUE if X < Y"
  (string< (symbol-name x) (symbol-name y)))
