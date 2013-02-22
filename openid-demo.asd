;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8;  -*-
;;; Copyright (C) 2013 Anton Vodonosov (avodonosov@yandex.ru)
;;; See LICENSE for details.

(asdf:defsystem #:openid-demo
  :version "1.0.0"
  :serial t
  :depends-on (#:hunchentoot #:cl-openid)
  :components ((:file "openid-demo")))

