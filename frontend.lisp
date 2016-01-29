(in-package :genlin)
;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; A pretty front-end
;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

(defparameter *tweakables*
  '(*debug*
    *stat-interval*
    *parallel*
    *dataset*
    *data-path*
    *training-ratio*
    *number-of-islands*
    *population-size*
    *mutation-rate*
    *migration-rate*
    *migration-size*
    *greedy-migration*
    *track-genealogy*
    *min-len*
    *max-len*
    *max-start-len*
    *opcode-bits*
    *source-register-bits*
    *destination-register-bits*))    

(defun print-tweakables ()
  (loop
     for symbol in *tweakables*
     for i from 0 to (length *tweakables*) do
       (format t "[~d] ~S: ~S~%     ~A~%" i symbol (symbol-value symbol)
               (documentation symbol 'variable))))

(defun print-operations ()
  (let ((used #\*)
        (unused #\space))
    (loop
       for op in (coerce *operations* 'list)
       for i from 0 to (length *operations*) do
         (format t "~A ~A~%"
                 (func->string op)
                 (if (< i (expt 2 *opcode-bits*)) used unused)))))

(defun menu ()
  "The front end and user interface of the programme. Allows the user to tweak a number of dynamically scoped, special variables, and then launch setup and evolve."
  (in-package :genlin)
  (flet ((validate (n)      
           (or (eq n :Q) 
               (and (numberp n)
                    (<= 0 n)
                    (< n (length *tweakables*)))))
         (parse-method (m)
           (case m
             ((:t) #'tournement!)
             ((:r) #'roulette!)
             ((:g) #'greedy-roulette!)
             (otherwise nil))))    
    (let ((sel)
          (target)
          (rounds)
          (method))
      (loop do
           (hrule)
           (print-tweakables)
           (hrule)
           (loop do 
                (format t "~%ENTER NUMBER OF PARAMETER TO TWEAK, OR :Q TO PROCEED.~%")
                (princ "~ ")
;                (clear-input)
                (setf sel (read))
                (when (validate sel) (return)))
           (when (eq sel :Q) (return))
           (format t "~%YOU SELECTED ~D: ~A~%CURRENT VALUE: ~S~%     ~A~%~%"
                   sel
                   (elt *tweakables* sel)
                   (symbol-value (elt *tweakables* sel))
                   (documentation (elt *tweakables* sel) 'variable))
           (format t "ENTER NEW VALUE (BE CAREFUL, AND MIND THE SYNTAX)~%~~ ")
           (setf (symbol-value (elt *tweakables* sel)) (read))
           (format t "~A IS NOW SET TO ~A~%"
                   (elt *tweakables* sel)
                   (symbol-value (elt *tweakables* sel)))
           (format t "ENTER :Q TO RUN WITH THE CHOSEN PARAMETERS, OR :C TO CONTINUE TWEAKING.~%~~ ")
;           (clear-input)
           (when (eq (read) :Q) (return)))
      (setf +ISLAND-RING+ '())
      (update-dependent-machine-parameters)
      (setup)
      (format t "THE EVOLUTION WILL RUN UNTIL EITHER A TARGET FITNESS IS~%")
      (format t "REACHED, OR A MAXIMUM NUMBER OF CYCLES HAS BEEN EXCEEDED.~%~%")
      (format t "ENTER TARGET FITNESS (A FLOAT BETWEEN 0 AND 1).~%~~ ")
;      (clear-input)
      (setf target (read))
      (format t "~%CHOOSE A SELECTION METHOD: TOURNEMENT, ROULETTE, OR GREEDY-ROULETTE?~%ENTER :T, :R, or :G.~%~~ ")
;      (clear-input)
      (setf method (parse-method (read)))
      (format t "READ ~A~%" method)
      (when (null method)
        (format t "NO GOOD. USING TOURNEMENT.")
        (setf method #'tournement!))      
      (format t "
ENTER MAXIMUM NUMBER OF CYCLES (A CYCLE HAS TAKEN PLACE WHEN A
BREEDING EVENT HAS ELAPSED ON EACH ISLAND. IN TOURNMENT MODE, THIS
AMOUNTS TO TWO DEATHS, ONE INSTANCE OF SEXUAL REPRODUCTION, AND TWO
BIRTHS. IN ROULETTE MODE, THIS AMOUNTS TO N DEATHS, N/2 INSTANCES OF
SEXUAL REPRODUCTION, AND N BIRTHS, WHERE N = THE TOTAL POPULATION
COUNT).~%~~ ")
;      (clear-input)
      (setf rounds (read))
      (format t "~%COMMENCING EVOLUTIONARY PROCESS. PLEASE STANDBY.~%~%")
      (evolve :target target :rounds rounds :method method))))
               


