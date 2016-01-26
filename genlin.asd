(asdf:defsystem #:genlin
  :depends-on (#:common-lisp)
  :components ((:file "auxiliary")
               (:file "tictactoe")
               (:file "iris")
               (:file "genlin"
                      :depends-on ("tictactoe"))))
               
                 
