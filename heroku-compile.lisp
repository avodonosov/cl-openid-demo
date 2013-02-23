(in-package :cl-user)

(print ">>> Building system....")
(require 'asdf)
(asdf:disable-output-translations)
(require-quicklisp) ;; use :version?
(let* ((this-file (load-time-value (or *load-truename* #.*compile-file-pathname*)))
       (this-file-dir (make-pathname :directory (pathname-directory this-file))))
  (push this-file-dir asdf:*central-registry*))
(ql:quickload :openid-demo)
(print ">>> Done building system")
