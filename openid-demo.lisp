(defpackage :openid-demo
  (:use :cl)
  (:export :start))

(in-package :openid-demo)

(hunchentoot:define-easy-handler (send-notification :uri "/hello")
    (who)
  (format nil "hello ~A" who))

(defun start (&key port smtp-password)
  (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port 4242)))