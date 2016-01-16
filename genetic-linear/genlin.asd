(asdf:defsystem #:genlin
  :depends-on (#:common-lisp)
  :components ((:file "tictactoe")
               (:file "genlin"
                      :depends-on ("tictactoe"))))
               
                 
