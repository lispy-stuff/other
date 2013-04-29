(defpackage clarkup
  (:export encode with-decoder)
  (:use common-lisp core clitex iterate))

(in-package clarkup)

(defun decode-attrs (data)
  "Decode attributes in DATA and return ((name1 . value1)...)"
  (let ((res) (start 0))
    (iter (for sep = (position #\= data :start start))
          (unless sep (leave res))
          (let ((end (or (position #\space data :start (1+ sep))
                         (length data))))
            (push (cons (kw (string-left-trim " " (subseq data start sep)))
                        (subseq data (+ sep 2) (1- end)))
                  res)
            (setf start end)))))

(defmac with-decoder (fn start-form end-form &body body)
  "Execute BODY with FN bound to a decoder. Decoder executes START-FORM once for each opened tag with ATTRIBUTES and TAG-NAME bound, END-FORM for each closed tag with ATTRIBUTES, BODY and TAG-NAME bound. STACK is bound to an assoc list of open tags and their attributes in both forms."
  `(let ((,$body)
         (,$stack)
         (,$buffer (make-string-output-stream))
         ,$inside-tag?
         ,$tag)
     (symbol-macrolet ((,(sym "ATTRIBUTES") ,$attributes)
                       (,(sym "BODY") ,$body)
                       (,(sym "STACK") ,$stack)
                       (,(sym "TAG-NAME") ,$tag-name))
       (flet ((,fn (,$data &optional (,$start 0))
                (do-while (< ,$start (length ,$data))
                  (if ,$inside-tag?
                      (let ((,$next (position #\> ,$data :start ,$start)))
                        (if ,$next
                            (progn
                              (when (> ,$next ,$start)
                                (write-string ,$data ,$buffer
                                              :start ,$start
                                              :end ,$next))
                              (setf ,$tag
                                    (string-left-trim
                                     " "
                                     (get-output-stream-string ,$buffer)))
                              (let* ((,$attributes)
                                     (,$i (position #\space ,$tag))
                                     (,$tag-name (if ,$i
                                                     (subseq ,$tag 0 ,$i)
                                                     ,$tag)))
                                (if (char= (char ,$tag-name 0) #\/)
                                    (progn
                                      (setf ,$tag-name (kw (subseq ,$tag-name
                                                                   1)))
                                      ,@end-form)
                                    (progn
                                      (setf ,$tag-name (kw ,$tag-name))
                                      (when ,$i
                                        (setf ,$attributes
                                              (if ,$i
                                                  (decode-attrs (subseq ,$tag
                                                                        ,$i))
                                                  nil)))
                                      (push (cons ,$tag-name ,$attributes)
                                            ,$stack)
                                      ,@start-form)))
                              (setf ,$inside-tag? nil)
                              (setf ,$start (1+ ,$next)))
                            (progn
                              (write-string ,$data ,$buffer
                                            :start ,$start
                                            :end (length ,$data))
                              (setf ,$start (length ,$data)))))
                      (let ((,$next (position #\< ,$data :start ,$start)))
                        (if ,$next
                            (progn
                              (when (> ,$next ,$start)
                                (write-string ,$data ,$buffer
                                              :start ,$start
                                              :end ,$next))
                              (setf ,$body (get-output-stream-string ,$buffer))
                              (when (string= ,$body "") (setf ,$body nil))
                              (setf ,$inside-tag? t)
                              (setf ,$start (1+ ,$next)))
                            (progn
                              (write-string ,$data ,$buffer
                                            :start ,$start
                                            :end (length ,$data))
                              (setf ,$start (length ,$data)))))))))
         ,@body))))

(defmac encode ((tag &rest attrs) &body body)
  "Encode TAG with ATTRS and optional BODY, return T from BODY to create single tag (<TAG/>)."
  `(with-output-to-string (,$out)
     (let ((,$res (progn ,@body))
           (,$tag-name (string-downcase ',tag)))
       (format ,$out "<~A" ,$tag-name)
       ,@(mapcar (lambda (a)
                   `(format ,$out
                            " ~A='~A'"
                            ,(string-downcase (first a))
                            ,(second a)))
                 attrs)
       (cond
         ((eq ,$res t)
          (write-char #\> ,$out))
         ((null ,$res)
          (write-string "/>" ,$out))
         (t (format ,$out ">~A</~A>" ,$res ,$tag-name))))))

(test (:clarkup)
  (test (:encode)
    (test-string=
     (encode (outer (attr1 1) (attr2 2))
             (encode (inner)
                     "abc"))
     "<outer attr1='1' attr2='2'><inner>abc</inner></outer>"))
  (test (:decode)
    (let ((test-tags))
      (with-decoder d
          ((push (cons :start (cons tag-name attributes)) test-tags))
          ((push (cons :end (cons tag-name body)) test-tags))
        (d "<abc def='ghi'><foo")
        (d "><bar>ab")
        (d "c</bar></foo>")
        (test-equal (reverse test-tags)
                    (list (cons :start (cons :abc (list (cons :def "ghi"))))
                          (cons :start (cons :foo nil))
                          (cons :start (cons :bar nil))
                          (cons :end (cons :bar "abc"))
                          (cons :end (cons :foo nil))))))))