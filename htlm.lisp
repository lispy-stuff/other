(defpackage htlm
  (:export *stack*)
  (:use clite common-lisp core))

(in-package htlm)

(defparameter *stack* nil)

(defclass node ()
  ())

(defgeneric write-html (node out))

(defclass text-node (node)
  ((content :initarg :content :reader content)))

(defmethod write-html ((node text-node) out)
  (write-string (content node) out))

(defclass elem (node)
   ((child-nodes :initform nil)))

(defgeneric tag-name (elem))

(defgeneric attrs (elem))

(defun add-child-node (elem child)
  (push child (slot-value elem 'child-nodes)))

(defun child-nodes (elem)
  (reverse (slot-value elem 'child-nodes)))

(defmethod write-html ((node elem) out)
  (format out "<~A" (tag-name node))

  (dolist (a (attrs node))
    (let-if (v (rest a))
      (format out " ~A='~A'" (string-downcase (first a)) v)))
  
  (if (child-nodes node)
      (progn
        (write-char #\> out)
        (dolist (c (child-nodes node))
          (write-html c out))
        (format out "</~A>" (tag-name node)))
      (write-string "/>" out)))

(defmac define-elem (tag supers &rest attrs)
  (let ((class-name (sym tag '-elem))
        (attr-names (mapcar (lambda (a) (if (consp a) (first a) a)) attrs)))
    `(progn
       (defclass ,class-name (,@supers)
         (,@(mapcar (lambda (a)
                      (if (consp a) a `(,a :initform nil :accessor ,a)))
                    attrs)))
       
       (defconst ,(sym tag '-attr-names) '(,@attr-names))

       (defmethod tag-name ((elem ,class-name))
         ,(string-downcase tag))
       
       (defmethod attrs ((elem ,class-name))
         (mapcar (lambda (a) (cons (kw a) (slot-value elem a)))
                 '(,@attr-names))))))

(define-elem html (elem)
  manifest)

(define-elem basic (elem)
  accesskey 
  (class :accessor css-class)
  contenteditable
  contextmenu
  dir
  draggable
  dropzone
  hidden
  id
  lang
  onabort
  oncanplay
  oncanplaythrough
  onblur
  onchange
  onclick
  oncontextmenu
  ondblclick
  ondrag
  ondragend
  ondragenter
  ondragleave
  ondragover
  ondragstart
  ondrop
  ondurationchange
  onemptied
  onended
  onerror
  onfocus
  onformchange
  onforminput
  oninput
  oninvalid
  onkeydown
  onkeypress
  onkeyup
  onloadeddata
  onloadedmetadata
  onloadstart
  onmousedown
  onmousemove
  onmouseout
  onmouseover
  onmouseup
  onmousewheel
  onpause
  onplay
  onplaying
  onprogress
  onratechange
  onreadystatechange
  onscroll
  onselect
  onseeked
  onseeking
  onstalled
  onsubmit
  onsuspend
  ontimeupdate
  onvolumechange
  onwaiting
  spellcheck
  style
  tabindex
  title
  translate)

(define-elem a (basic-elem)
  href)

(define-elem body (basic-elem)
  onafterprint
  onbeforeprint
  onbeforeunload
  onhaschange
  onload
  onmessage
  onoffline
  ononline
  onpagehide
  onpageshow
  onpopstate
  onredo
  onresize
  onstorage
  onundo)

(defmac do-elem (elem attr-names &body body)
  `(progn
     (let ((,$elem ,elem))
       (when *stack* (add-child-node (first *stack*) ,$elem))
       (push ,$elem *stack*)
       (with-slots (,@attr-names) ,$elem
         ,@body)
     (pop *stack*))))

(defmac do-a (href &body body)
  `(do-elem (make-instance 'a-elem) ,a-attr-names
       (setf href ,href)
     ,@body))

(defmac do-body (&body body)
  `(do-elem (make-instance 'body-elem) ,body-attr-names
       (macrolet ((a (href &body body)
                    `(do-a ,href ,@body))
                  (text (content)
                    `(add-child-node (first *stack*)
                                     (make-instance 'text-node
                                                    :content ,content))))
         ,@body)))

(defmac do-html (&body body)
  `(do-elem (make-instance 'html-elem) ,html-attr-names
    (macrolet ((body (&body body)
                 `(do-body ,@body)))
      ,@body)))

(test (:htlm)
  (let ((res (with-output-to-string (out)
               (write-html
                (do-html
                  (body (a "abc"
                           (test-string= href "abc")
                           (setf href "def")
                           (text "ghi"))))
                out))))
    (test-string= res "<html><body><a href='def'>ghi</a></body></html>")))