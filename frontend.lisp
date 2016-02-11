(load #p"~/Projects/genlin/package.lisp")

(defpackage :genlin
  (:use :common-lisp))

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
          (t (hrule)
             (loop for param in *tweakables* do
                  (let ((key (tweakable->posixopt param)))
                    (when (member key args :test #'equalp)
                      ;; (FORMAT T "FOUND OPT: ~S = ~S~%"
                      ;; key (get-opt-arg args key))
                      (setf (symbol-value param)
                            (read-from-string (get-opt-arg args key)))
                      (format t "[+] SETTING ~A TO ~A...~%"
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
  ;; (unless *sex*
  ;;   (setf *mutation-rate* 1))
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





(defun setup-data (&key (ratio *training-ratio*)
                     (dataset *dataset*)
                     (selection-method *selection-method*)
                     (pack-selection-method *pack-selection-method*)
                     (fitfunc-name *fitfunc-name*)
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
    
    (setf hashtable (datafile->hashtable :filename filename))
    (when *testing-data-path*
      (setf training+testing (cons hashtable
                                   (datafile->hashtable
                                    :filename *testing-data-path*))))
    (setf *number-of-classes* (funcall =label-scanner= 'count))
    (unless *split-data*
      (setf training+testing (cons hashtable hashtable)))
    (unless training+testing
      (setf training+testing (partition-data hashtable ratio)))

    ;; init-fitness-env sets up the training and testing hashtables,
    ;; the fitness function, and the output registers. 
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

(defun save-all (island-ring)
  (with-open-file (stream "STATISTICS.SAV"
                          :direction :output
                          :if-exists :append
                          :if-does-not-exist :create)
    (format stream "~A~%~%" (timestring))
    (print-statistics island-ring :stream stream))
  (save-parameters)
  (save-island-ring))

(defun save-parameters (&optional (param-path *last-params-path*))
  "Will append to existing file, allowing old settings to be retrieved if
accidentally clobbered."
  (with-open-file (stream param-path
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (format stream "~%~A~%~%" (timestring))
    (loop for param in *tweakables* do
         (format stream "~S~%"
                 `(setf ,param ,(symbol-value param))))))

(defun save-island-ring (&key (filename "ISLAND-RING.SAV")
                           (bury-parents t)
                           (island-ring +ISLAND-RING+))
  (let ((copy (island-ring-writeable-copy island-ring
                                          :bury-parents bury-parents)))
    (with-open-file (stream filename
                            :direction :output
                            :if-exists :supersede 
                            :if-does-not-exist :create)
      (setf *print-circle* nil)
      (format stream
              "~A~%;; +ISLAND-RING+ below: ~%~%~S~%)"
              (timestring)
              copy)
      (setf *print-circle* T))))

(defun island-ring-writeable-copy (island-ring &key (bury-parents t))
  (let ((copy (mapcar #'copy-structure (de-ring island-ring))))
    (loop for isle in copy do
         (when bury-parents
           (mapc #'bury-parents (island-deme isle))
           (mapc #'bury-parents (island-packs isle)))
         (setf (island-method isle) nil)
         (setf (island-lock isle) nil)
         (setf (island-logger isle) nil)
         (setf (island-coverage isle) nil))
    copy))

;; ;; placeholders
;; (defun tournement! ())
;; (defun roulette! ())
;; (defun greedy-roulette! ())
;; (defun lexicase! ())

(defun restore-island-ring (&key (filename "ISLAND-RING.SAV"))
  (let ((copy)
        (method-chooser))
    (format t "[-] RESTORING ISLAND-RING FROM ~A..." filename)
    (with-open-file (stream filename :direction :input
                            :if-does-not-exist nil)
      (and (setf copy (read stream))
           (format t "    ISLAND-RING SUCCESSFULLY RESTORED!")))
    (loop for isle in copy do
         (setf (island-logger isle) (make-logger))
         (setf (island-lock isle) (sb-thread:make-mutex
                                   :name (format nil "isle-~d-lock"
                                                 (island-id isle))))
         (setf (island-era isle) 0) ;; necessary to prevent certain bugs
         ;; but admittedly a bit of a kludge
         (if *case-storage* 
             (setf (island-coverage isle) (init-cases-covered
                                           *training-hashtable*)))
         
         (setf method-chooser
               (if (island-packs isle)
                         *pack-selection-method*
                         *selection-method*))
         
         (setf (island-method isle)
               (case method-chooser                        
                 ((:tournement) #'tournement!)
                 ((:roulette) #'roulette!)
                 ((:greedy-roulette) #'greedy-roulette!)
                 ((:lexicase) #'lexicase!)
                 (otherwise (progn
                              (format t "WARNING: METHOD NAME NOT RECO")
                              (format t "GNIZED. USING #'TOURNEMENT!.~%")
                              #'tournement!)))))

    (setf +island-ring+ (circular copy))))


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
    (if *restore-island-ring*
        (restore-island-ring :filename *restore-island-ring*)
        (setup-population))
    (print-params)
    (when run
      (format t "          -oO( COMMENCING EVOLUTIONARY PROCESS, PLEASE STANDBY )Oo-~%")
      (hrule)
      (evolve :target *target* :rounds *rounds*)
      (save-parameters *last-params-path*)
      (format t "PARAMETERS SAVED IN ~A~%" *last-params-path*)
      (save-island-ring :filename "ISLAND-RING.SAV"
                        :island-ring +ISLAND-RING+)
      (format t "ISLAND-RING SAVED IN ISLAND-RING.SAV~%"))))














