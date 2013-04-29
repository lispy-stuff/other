(defpackage htlm
  (:export *attrs* *out* a attr-names body body-elem define-elem do-a do-html 
           elem)
  (:use common-lisp core clite))

(in-package htlm)

(defparameter *attrs* nil)
(defparameter *out* nil)

(defclass node ()
  ())

(defclass text (node)
  ((content :initarg :content :reader content)))

(defclass basic-elem (node)
  ((attr-names :allocation :class :initform nil :reader attr-names)
   (child-elems :initform nil :accessor child-elems)))

(defmac define-elem (tag supers &rest attrs)
  (let ((first-flag (sym 'first- tag)))
    `(progn
       (defclass ,tag (,@supers)
         (,@(mapcar (lambda (a) (if (consp a) a `(,a :accessor ,a))) attrs)))
       
       (defvar ,first-flag t)
       
       (defmethod initialize-instance :after ((e ,tag) &key)
         (when ,first-flag
           (dolist (a '(,@attrs))
             (let ((n (if (consp a)
                          (first a)
                          a)))
               (push n (slot-value e 'attr-names))))
           (setf ,first-flag nil))))))

(define-elem html-elem (basic-elem)
  manifest)

(define-elem elem (basic-elem)
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

(define-elem a-elem (elem)
  href)

(define-elem body-elem (elem)
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

(defun push-attrs (&rest attrs)
  (doplist (k v attrs)
    (when v
      (push v htlm:*attrs*)
      (push k htlm:*attrs*))))

(defun write-text (cont)
  (write-string cont *out*)) 

(defmac do-basic-elem (tag attrs &body body)
  "Encode element TAG with attrs and body."
  `(let ((,$tag-name (string-downcase ',tag)))
     (format htlm:*out* "<~A" ,$tag-name)
     (let ((,$res 
            (let* ((htlm:*attrs*)
                   (,$res (with-output-to-string (htlm:*out*)
                            (let (,@(mapcar (lambda (a) `(,a)) attrs))
                              (declare (ignorable ,@attrs))
                              ,@body
                              (push-attrs ,@(mapcan (lambda (a)
                                                      `(,(kw a) ,a))
                                                    attrs))))))
              (doplist (k v htlm:*attrs*)
                (format htlm:*out* " ~A='~A'" (string-downcase k) v))
              ,$res)))
       (if (string= ,$res "")
           (write-string "/>" htlm:*out*)
           (format htlm:*out* ">~A</~A>" ,$res ,$tag-name)))))

(defmac do-elem (tag attrs &body body)
  `(do-basic-elem ,tag (accesskey 
                        class
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
                        translate
                        ,@attrs)
     ,@body))


(defmac do-a (href &body body)
  `(do-elem "a" (href)
                 (setf href ,href)
                 ,@body))

(defmac do-body (&body body)
  `(do-elem "body" (onafterprint
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
                 (macrolet ((a (href &body body)
                              `(do-a ,href ,@body))
                            (text (cont)
                              `(write-text ,cont)))
                   ,@body)))


(defmac do-html (&body body)
  `(with-output-to-string (htlm:*out*)
     (do-basic-elem "html" (manifest)
                    (macrolet ((body (&body body)
                                 `(do-body ,@body)))
                      ,@body))))

(test (:htlm)
  (let ((res (do-html
                 (body (a "abc"
                          (test-string= href "abc")
                          (setf href "def")
                          (text "ghi"))))))
    (test-string= res "<html><body><a href='def'>ghi</a></body></html>")))