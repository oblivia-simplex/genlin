(in-package :genlin)
;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; A pretty front-end
;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

(defun print-operations ()
  (let ((used #\*)
        (unused #\space))
    (loop
       for op in (coerce *operations* 'list)
       for i from 0 to (length *operations*) do
         (format t "~A ~A~%"
                 (func->string op)
                 (if (< i (expt 2 *opcode-bits*)) used unused)))))

(defun string->earmuff (string)
  (let ((muffed (concatenate 'string "*" (string-upcase string) "*")))
    (intern muffed)))

(defun earmuff->string (symbol)
  (let ((string (remove #\* (symbol-name symbol))))
    string))

(defun tweakable->posixopt (symbol)
  (concatenate 'string "--"
               (string-downcase
                (earmuff->string symbol))))

(defun print-tweakables ()
  (loop
     for symbol in *tweakables*
     for i from 0 to (length *tweakables*) do
       (format t "[~d] ~A  ~S~%     ~A~%" i (tweakable->posixopt symbol)
               (symbol-value symbol)
               (documentation symbol 'variable))))


(defun get-opt-arg (list key)
  (let ((anything-there (member key list :test #'equalp)))
    (when anything-there
      (cadr anything-there))))

(defun print-help ()
  (format t "=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=~%")
  (format t "                                 GENLIN~%")
  (format t "=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=~%~%")
  (format t
"Many of the dynamic (global) parameters in GENLIN can be set using
familiar POSIX-style command line argument syntax. Valid options and
the default values of the parameters they modify are printed below.
Note that quotation marks must be escaped for string arguments, but
keywords (prefixed by a colon) need no quotes.~%~%")
  (print-tweakables)
  nil)

(defun parse-command-line-args ()
  (let ((args (cdr sb-ext:*posix-argv*)))
;;    (FORMAT T "ARGV = ~S~%" args)
    (cond ((or (member "--help" args :test #'equalp)
               (member "-h" args :test #'equalp))
           (print-help))
          (t (loop for param in *tweakables* do
                  (let ((key (tweakable->posixopt param)))
                    (when (member key args :test #'equalp)
                      ;; (FORMAT T "FOUND OPT: ~S = ~S~%"
                      ;; key (get-opt-arg args key))
                      (setf (symbol-value param)
                   (read-from-string (get-opt-arg args key)))
                      (format t "Setting ~A to ~A...~%"
                              param (symbol-value param))))) T))))

    ;;           (format t "~S = ~S~%" param (symbol-value param))))))
    
  (defun menu ()
    "The front end and user interface of the programme. Allows the user
to tweak a number of dynamically scoped, special variables, and then
launch setup and evolve."
  (flet ((validate (n)      
           (or (eq n :Q) 
               (and (numberp n)
                    (<= 0 n)
                    (< n (length *tweakables*))))))    
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
                   (symbol-value (elt *tweakables* sel)))))))
           ;;(format t "ENTER :Q TO RUN WITH THE CHOSEN PARAMETERS, OR :C TO CONTINUE TWEAKING.~%~~ ")
;           (clear-input)
      ;;(when (eq (read) :Q) (return)))
      ;; setup was here
;;       (format t "THE EVOLUTION WILL RUN UNTIL EITHER A TARGET FITNESS IS~%")
;;       (format t "REACHED, OR A MAXIMUM NUMBER OF CYCLES HAS BEEN EXCEEDED.~%~%")
;;       (format t "ENTER TARGET FITNESS (A FLOAT BETWEEN 0 AND 1).~%~~ ")
;; ;      (clear-input)
;;       (setf target (read))
;;       (format t "~%CHOOSE A SELECTION METHOD: TOURNEMENT, ROULETTE, OR GREEDY-ROULETTE?~%ENTER :T, :R, or :G.~%~~ ")
;; ;      (clear-input)
;;       (setf *selection-method* (read))
;;       (format t "
;; ENTER MAXIMUM NUMBER OF CYCLES (A CYCLE HAS TAKEN PLACE WHEN A
;; BREEDING EVENT HAS ELAPSED ON EACH ISLAND. IN TOURNMENT MODE, THIS
;; AMOUNTS TO TWO DEATHS, ONE INSTANCE OF SEXUAL REPRODUCTION, AND TWO
;; BIRTHS. IN ROULETTE MODE, THIS AMOUNTS TO N DEATHS, N/2 INSTANCES OF
;; SEXUAL REPRODUCTION, AND N BIRTHS, WHERE N = THE TOTAL POPULATION
;; COUNT).~%~~ ")
;; ;      (clear-input)
;;       (setf *rounds* (read))
;;       (format t "~%COMMENCING EVOLUTIONARY PROCESS. PLEASE STANDBY.~%~%"))))


(defun sanity-check ()
  "A place to prevent a few of the more disasterous parameter clashes
and eventually, sanitize the input."
  (when (or (eql *migration-size* 0) (eql *greedy-migration* 0))
    (setf *greedy-migration* nil))
  (when *debug*
    (setf *parallel* nil)
    (when (eq *selection-method* :lexicase)
      (format t "WARNING: *TRACK-GENEALOGY* CURRENTLY INCOMPATIBLE")
      (format t " WITH LEXICASE SELECTION.~%DISABLING.")
      (setf *track-genealogy* nil))))

;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

(defun setup-population ()
    (setf +ISLAND-RING+
          (init-population *population-size* *max-start-len*
                           :number-of-islands *number-of-islands*)))

(defun main ()
  (format t "~A~%" (timestring))
  (when (parse-command-line-args)
    (when *menu* (menu))
    (setup-data)   ;; load the dataset, build the hashtables
    (update-dependent-machine-parameters)
    (sanity-check) ;; makes things slightly less likely to explode
    (setup-population)
    (print-params)
    (format t "          -oO( COMMENCING EVOLUTIONARY PROCESS, PLEASE STANDBY )Oo-~%")
    (hrule)
    (evolve :target *target* :rounds *rounds*)))
  

;; todo: write a generic csv datafile->hashtable loader, for
;; deployment on arbitrary datasets. 

;; it's not convenient, yet, to select the VM at runtime. this choice needs
;; to be made at compile time, for the time being. 




