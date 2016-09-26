(load "~/quicklisp/setup.lisp")
(ql:quickload :genlin)
(sb-ext:save-lisp-and-die "GENLIN" :executable t :toplevel #'genlin::main)
