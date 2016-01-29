;;;; package.lisp
(in-package :common-lisp-user)

(defpackage #:r-machine
  (:use #:cl))

(defpackage #:genlin
  (:use #:cl
        #:r-machine
        #:sb-thread))




