;;; The following lines added by ql:add-to-init-file:
#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(require "asdf")
(require "cffi")
(require "iterate")
(require "local-time")

(load #P"~/Lisp/other/core.lisp")
(load #P"~/Lisp/other/log.lisp")
(load #P"~/Lisp/clite/clite.lisp")

(pushnew #P"~/Lisp/asdf/"
         asdf:*central-registry*
         :test #'equal)
(asdf:operate 'asdf:load-op 'clite)
