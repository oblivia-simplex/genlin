(load #p"~/Projects/genlin/package.lisp")

(in-package :genlin)

;;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;;; Linear Genetic Algorithm
;;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

(defparameter *project-path*
  "~/Projects/genlin/")

(defun loadfile (filename)
  (load (merge-pathnames filename *load-truename*)))

;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

(defparameter *machine* :stackmachine)


;; (defparameter *machine-file*
;;   (case *machine*
;;     ((:stackmachine "stackmachine.lisp"))
;;     ((:slomachine) "slomachine.lisp")
;;     ((:r-machine) "r-machine.lisp")))
;; ;; slomachine is an unstable VM for self-modifying code

(defun load-other-files ()
  (loop for f in `("params.lisp"
                   "auxiliary.lisp"
                   "stackmachine.lisp"
                   "datafiler.lisp"
                   "tictactoe.lisp"
;;                   "iris.lisp"
                   "genlin.lisp") do
       (loadfile (concatenate 'string *project-path* f))))


(load-other-files)

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
    (when (member (tweakable->posixopt '*params-path*) args)
      (format t "READING PARAMETERS FROM ~A~%" *params-path*)
      (read-parameter-file
       (read-from-string
        (get-opt-arg args (tweakable->posixopt '*params-path*)))))
                                                          
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
    (let ((sel))
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


(defun sanity-check ()
  "A place to prevent a few of the more disasterous parameter clashes
and eventually, sanitize the input."
  (when (or (eql *migration-size* 0) (eql *greedy-migration* 0))
    (setf *greedy-migration* nil))
  (unless *sex*
    (setf *mutation-rate* 1))
  (when *debug*
    (setf *parallel* nil)
    (when (eq *selection-method* :lexicase)
      (format t "WARNING: *TRACK-GENEALOGY* CURRENTLY INCOMPATIBLE")
      (format t " WITH LEXICASE SELECTION.~%DISABLING.")
      (setf *track-genealogy* nil)
      (setf *case-storage* t))))

;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

(defun setup-population ()
    (setf +ISLAND-RING+
          (init-population *population-size* *max-start-len*
                           :number-of-islands *number-of-islands*)))

(defun read-parameter-file (param-path)
  (load param-path))

(defun write-parameter-file (param-path)
  "Will append to existing file, allowing old settings to be retrieved if
accidentally clobbered."
  (with-open-file (stream param-path
                          :direction :output
                          :if-exists :append
                          :if-does-not-exist :create)
    (format stream "~%~A~%~%" (timestring))
    (loop for param in *tweakables* do
         (format stream "~S~%"
                 `(setf ,param ,(symbol-value param))))))




(defun setup-data (&key (ratio *training-ratio*)
                     (dataset *dataset*)
                     (selection-method *selection-method*)
                     (pack-selection-method *pack-selection-method*)
                     (fitfunc-name)
                     (filename *data-path*))
  (let ((hashtable)
        (training+testing))

    (setf *method* (case selection-method
                     ((:tournement) #'tournement!)
                     ((:roulette) #'roulette!)
                     ((:greedy-roulette) #'greedy-roulette!)
                     ((:lexicase) #'lexicase!)
                     (otherwise (progn
                                  (format t "WARNING: METHOD NAME NOT RECO")
                                  (format t "GNIZED. USING #'TOURNEMENT!.~%")
                                  #'tournement!))))

    (setf *pack-method* (case pack-selection-method
                     ((:tournement) #'tournement!)
                     ((:roulette) #'roulette!)
                     ((:greedy-roulette) #'greedy-roulette!)
                     ((:lexicase) #'lexicase!)
                     (otherwise (progn
                                  (format t "WARNING: METHOD NAME NOT RECO")
                                  (format t "GNIZED. USING #'TOURNEMENT!.~%")
                                  #'tournement!))))

    (setf *dataset* dataset)
    (reset-records)
    (funcall =label-scanner= 'flush)
    (case dataset   ;; at some point, I'll have to clean up all of this
      ((:tictactoe) ;; ad-hoc spaghetti code. 
       (unless filename (setf filename *tictactoe-path*))
       (unless fitfunc-name (setf fitfunc-name 'binary-1)))
      (otherwise
       (unless filename (setf filename *iris-path*))
       (unless fitfunc-name (setf fitfunc-name 'n-ary))))
    (setf hashtable (datafile->hashtable :filename filename))
    (when *testing-data-path*
      (setf training+testing (cons hashtable
                                   (datafile->hashtable
                                    :filename *testing-data-path*))))
    (unless *split-data*
      (setf training+testing (cons hashtable hashtable)))
    (unless training+testing
      (setf training+testing (partition-data hashtable ratio)))
    (init-fitness-env :training-hashtable (car training+testing)
                      :testing-hashtable  (cdr training+testing)
                      :fitfunc-name fitfunc-name)

    (case *sex*
      ((:1pt) (setf *mating-func* #'shufflefuck-1pt))
      ((:2pt) (setf *mating-func* #'shufflefuck-2pt-constant))
      ((t) (setf *mating-func* #'shufflefuck-2pt-constant))
      (otherwise (setf *mating-func* 'cloning)))
         
    training+testing))


;; todo: write a generic csv datafile->hashtable loader, for
;; deployment on arbitrary datasets. 

;; it's not convenient, yet, to select the VM at runtime. this choice needs
;; to be made at compile time, for the time being. 




;;; 

(defun save-island-ring (island-ring filename)
  (let ((copy (de-ring island-ring)))
    ;; (mapcar #'(lambda (x) (setf (island-lock x) 'mutex-unreadable
    ;;                        (island-logger x) 'logger-unreadable
    ;;                        (island-coverage x) 'coverage-unreadable))
    ;;         copy)
    (with-open-file (stream filename :direction :output :if-exists :overwrite
                            :if-does-not-exist :create)
      (format stream "~A~%~%;; tweakable parameters are:~%'(" (timestring))
      (loop for tweak in *tweakables* do
           (format stream "~S ~S~%" tweak (symbol-value tweak)))
      (format stream ")~%~%;; +ISLAND-RING+ below: ~%~%~S" +ISLAND-RING+))))

;; (defun restore-island-ring (filename)
;;   ;; todo
;;   )

;; Note: it should be simple enough to generalize the ttt data processing
;; technique.
;; - scan the dataset
;; - count the possible values for each field, and if there are an equal
;;   number of possibilities for each field, say n, formalize the key as
;;   an m-digit base-n gray code integer.
;; - this may, in some cases, even work when there is a maximum number
;;   of possibilities per field. or if each field can have any of n
;;   values, when unconstrained by other fields (the mutual constraints,
;;   of course, are an impt aspect of the pattern that the algo's meant
;;   to detect). 


(defun main (&key (run t))
  (format t "~A~%" (timestring))
  (when (parse-command-line-args)
    (when *menu* (menu))
    (setup-data)   ;; load the dataset, build the hashtables
    (update-dependent-machine-parameters)
    (sanity-check) ;; makes things slightly less likely to explode
    (setup-population)
    (print-params)
    (when run
      (format t "          -oO( COMMENCING EVOLUTIONARY PROCESS, PLEASE STANDBY )Oo-~%")
      (hrule)
      (evolve :target *target* :rounds *rounds*)
      (write-parameter-file *last-params-path*)
      (format t "PARAMETERS SAVED IN ~A~%" *last-params-path*)
      (save-island-ring +island-ring+ "ISLAND-RING.SAV")
      (format t "ISLAND-RING SAVED IN ISLAND-RING.SAV~%"))))
