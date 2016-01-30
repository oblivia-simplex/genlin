(in-package :genlin)


(defparameter *tictactoe-path*
  (concatenate 'string *project-path* "datasets/TicTacToe/tic-tac-toe.data"))

(defparameter *iris-path*
  (concatenate 'string *project-path* "datasets/Iris/iris.data"))


(defparameter *STOP* nil)

(defparameter *best* nil)

(defparameter *DEBUG* nil
  "The debugging flag. If set to T, then you will be treated to a very
     verbose, live disassembly of the instructions being excuted in the
     virtual machine, along with a few other pieces of information. Do
     not use in conjunction with *parallel.*")

(defparameter *parallel* t
  "Enables multithreading when set to T, allotting the evolutionary
     process on each island its own thread.")

(defparameter *VERBOSE* nil)

(defparameter *stat-interval* 1000
  "Number of cycles per verbose update on the evolutionary process.")

(defparameter *dataset* :tictactoe
  "Currently accepts only two values: :tictactoe and :iris. This tells
     the programme which dataset to load, and which related data
     processing functions to use.")

 
(defparameter *data-path* (if (eq *dataset* :tictactoe)
                              *tictactoe-path* *iris-path*)
  "Path to the data file. Mismatching *data-path* and *dataset* will
     almost certainly break something, at the moment.")

;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; Genetic Parameters
;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

;; Vars and types:

(defstruct creature fit seq eff gen home parents) 

(defstruct island id of deme best era logger lock)

(defvar +ISLAND-RING+)

(setf +ISLAND-RING+ '())

(defvar *training-ht* (make-hash-table :test 'equalp))

(defvar *testing-ht* (make-hash-table :test 'equalp))

(defvar -print-lock- (sb-thread:make-mutex :name "print lock"))

(defvar -migration-lock- (sb-thread:make-mutex :name "migration lock"))

;; ......................................................................
;; Tweakables
;; ......................................................................

(defparameter *number-of-islands* 12
  "Islands are relatively isolated pockets in the population, linked
     in a ring structure and bridged by occasional migration. Can be set
     to any integer > 0.")

(defparameter *population-size* 1200
  "Remains constant throughout the evolution. Should be > 40.")

(defparameter *specimens* '())

(defparameter *mutation-rate* 15
  "Chance of mutation per spawning event, expressed as percentage.")

(defparameter *migration-size* 10
  "Percentage of population that leaves one deme for the next, 
     per migration event.") ;; percent

(defparameter *migration-rate* 1000
  "Frequency of migrations, measured in main loop cycles.")

(defparameter *greedy-migration* t
  "If set to T, migrants are always the fittest in their deme.
     Otherwise random.")

(defparameter *track-genealogy* t
  "If set to T, then genealogical lineage and statistics are computed
     at runtime. Informative, but increases overhead.")
  
(defparameter *min-len* 2
  "The minimum creature length, measured in instructions.")
;; we want to prevent seqs shrinking to nil

(defparameter *max-len* 256
  "The maximum creature length, measured in instructions.")
;; max instruction length

(defparameter *max-start-len* 25
  "The maximum length of creatures in the initial population.")
;; max initial instruction length

(defparameter *training-ratio* 4/5
  "The ratio of training cases to total cases (the remainder being
     reserved for testing.")

(defparameter *rounds* 10000
  "The evolution will run until either a total number of mating events
     have elapsed on each island, or a pre-established fitness target
     has been reached. This determines the former.")

(defparameter *target* 0.95
  "The evolution will run until either a total number of mating events
     have elapsed on each island, or a pre-established fitness target
     has been reached. This determines the latter.")

(defparameter *method-key* :tournement
  "Determines which selection method will be used to select the parents 
    of each subsequent generation. Accepts as values the keywords: 
    :tournement, :roulette, or :greedy-roulette.")

(defparameter *method* nil);; #'tournement!)

(defparameter *menu* nil
  "Set to t if you would like to see a menu, from which the parameters
     can be set.")

;; Things whose presence here reflects a want of refactoring. 

(defvar .fitfunc. nil)

(defvar .training-hashtable. (make-hash-table :test #'equalp))

(defvar .testing-hashtable.  (make-hash-table :test #'equalp))

(defvar *out-reg* '())

(defparameter =label-scanner=
  ;; Enumerates labels, keeping track of labels it's already seen. 
  (let ((seen '()))
    (lambda (lbl)
      (cond ((eq lbl 'flush) (setf seen '()))
            ((not (member lbl seen :test #'equalp)) (push lbl seen)))         
      (cond ((eq lbl 'get) (pop seen) (reverse seen))
            (t (position lbl (reverse seen) :test #'equalp :from-end t))))))


;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; Virtual Machine Parameters (Generic)
;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;;                           INSTRUCTION FIELDS
;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

;; --- Adjust these 3 parameters to tweak the instruction set. ---
;; --- The rest of the parameters will respond automatically   ---
;; --- but due to use of inlining for optimization, it may     ---
;; --- be necessary to recompile the rest of the programme.    ---

;; see if you can put the entire virtual machine in its own environment

(defparameter *opcode-bits* 3
  "The number of bits in an instruction used to determine the operation. 
     Can be set to 1, 2, or 3.")   ;; size

(defparameter *source-register-bits* 3
  "The number of bits used to calculate the source register in each
     instruction. 2^n readable registers will be allocated where n is the
     value of this parameter." )

(defparameter *destination-register-bits* 2
  "The number of bits used to calculate the destination register. If
     left smaller than *source-register-bits* then there will be 2^(n-m)
     read-only registers, where n is the value of *source-register-bits*
     and m is the value of this parameter." )

;;(defparameter *flag-bits* 2)


;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; Parameters for register configuration.
;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

(defparameter *output-reg* '())

(defparameter *maxval* (expt 2 16)) ;; max val that can be stored in reg

(defparameter *minval* (expt 2 -16)) ;; floor this to 0

;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; Dependent Virtual Machine Vars
;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

(defvar *default-input-reg*)
  
(defvar *default-registers*)

(defvar *pc-idx*)

(defvar *initial-register-state*)

(defvar *input-start-idx*)

(defvar *input-stop-idx*)

(defvar *wordsize*)

(defvar *max-inst*)

(defvar *opbits*)

(defvar *srcbits*)

(defvar *dstbits*)

(defvar *flgbits*)

(defvar *machine-fmt*)



;; where should these functions live? With data? With params?

(defun how-many-input-registers? ()
  (let ((num 0))
    (loop
       for k being the hash-keys in *training-hashtable*
       do
         (setf num (length k))
         (return))
    num))

(defun how-many-output-registers? ()
  ;; a bit hackish, but the idea here is that if an n-ary dataset is used
  ;; then label-scanner will have a list to return; if it's a binary set
  ;; then label-scanner will have a null list
  (let ((s (funcall =label-scanner= 'get)))
    (if s (length (funcall =label-scanner= 'get)) 1)))
;; =label-scanner= lives in data-filer

        
