(load "genlin.lisp")
(in-package :genlin)
(setf *machine* :r-machine)
(load-other-files)
(sb-ext:save-lisp-and-die "R-GENLIN" :executable t :toplevel #'main)