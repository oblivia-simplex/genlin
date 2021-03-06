;;;; genlin.asd

(asdf:defsystem #:genlin
  :description "A linear genetic programming engine"
  :author "Olivia Lucca Fraser <lucca.fraser@gmail.com>"
  :license "GNU GPL"
 ;; :depends-on (#:sb-thread)
               
  :serial t
  :components ((:file "package")
               (:file "auxiliary")
               (:file "params")
               (:file "stackmachine")
               (:file "datafiler")
               (:file "genlin")
               (:file "frontend")
               (:file "tictactoe")))
;;               (:file "compile")))

