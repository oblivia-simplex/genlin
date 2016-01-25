(load "hello.lisp")
(in-package :genetic.hello)
(sb-ext:save-lisp-and-die "hello" :executable t :toplevel #'main)

