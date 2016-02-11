(in-package :genlin)


;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; types and structs
;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

(defstruct creature fit cm seq eff gen mut typ home parents pack) 

(defstruct island id of deme packs best era logger lock coverage method sample)

;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

(defparameter *tictactoe-path*
  (concatenate 'string *project-path* "datasets/TicTacToe/tic-tac-toe.data"))

(defparameter *iris-path*
  (concatenate 'string *project-path* "datasets/Iris/iris.data"))

(defvar *number-of-classes*)

(defparameter *STOP* nil)

(defparameter *best* nil)

(defparameter *DEBUG* nil
  "The debugging flag. If set to T, then you will be treated to a very
     verbose, live disassembly of the instructions being excuted in the
     virtual machine, along with a few other pieces of information. Do
     not use in conjunction with *parallel.*")

(defparameter *parallel* nil
  "Enables multithreading when set to T, allotting the evolutionary
     process on each island its own thread.")

(defparameter *VERBOSE* nil)

(defparameter *stat-interval* 500
  "Number of cycles per verbose update on the evolutionary process.")

(defparameter *dataset* :iris
  "Just the name for your dataset. Mostly used for labelling output,
     with the exception of :tictactoe, which tells the programme to use a
     particular data-processing module. More a hook for customizaiton
     than anything else.")

 
(defparameter *data-path* (if (eq *dataset* :tictactoe)
                              *tictactoe-path* *iris-path*)
  "Path to the data file. Mismatching *data-path* and *dataset* will
     almost certainly break something, at the moment.")

;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; Genetic Parameters
;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=



(defvar +ISLAND-RING+)

(defvar +HIVE-RING+)

(setf +ISLAND-RING+ '())

(defvar *training-ht* (make-hash-table :test 'equalp))

(defvar *testing-ht* (make-hash-table :test 'equalp))

(defvar -print-lock- (sb-thread:make-mutex :name "print lock"))

(defvar -migration-lock- (sb-thread:make-mutex :name "migration lock"))

;; ......................................................................
;; Tweakables
;; ......................................................................

(defparameter *number-of-islands* 8
  "Islands are relatively isolated pockets in the population, linked
     in a ring structure and bridged by occasional migration. Can be set
     to any integer > 0.")

(defparameter *population-size* 800
  "Remains constant throughout the evolution. Should be > 40.")

(defparameter *specimens* '())

(defparameter *migration-size* 1/10
  "Fraction of population that leaves one deme for the next, per
     migration event.") ;; percent

(defparameter *migration-rate* 200
  "Frequency of migrations, measured in generations.")

(defparameter *migration-method* :free
  "Can be :FREE or :CYCLIC.") ;; stub, explain

(defparameter *greedy-migration* 1
  "If set to 1, migrants are always the fittest in their deme. If set to a
     fraction lesser than 1, then migrants will be the top *migration-size*
     percent of a randomly-selected *greedy-migration* fraction of the
     deme.")

(defparameter *track-genealogy* nil
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

(defparameter *selection-method* :lexicase
  "Determines which selection method will be used to select the parents 
    of each subsequent generation. Accepts as values the keywords: 
    :tournement, :roulette, :greedy-roulette, or :lexicase.")

(defparameter *method* nil);; #'tournement!)

(defparameter *menu* nil
  "Set to t if you would like to see a menu, from which the parameters
     can be set.")

;; Things whose presence here reflects a want of refactoring. 

(defvar *fitfunc* nil)

(defvar *training-hashtable* (make-hash-table :test #'equalp))

(defvar *testing-hashtable*  (make-hash-table :test #'equalp))

(defvar *out-reg* '())


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

(defparameter *opcode-bits* 4
  "The number of bits in an instruction used to determine the operation. 
     Can be set to 1, 2, 3, or 4.")   ;; size

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

        
(defvar *ops* '(:DIV :MUL :SUB :ADD :PMD :JLE :LOD :STO)
  "The user may supply a list of opcode mnemonics if to reconfigure
     the virtual machine's primitive instruction set.")

(defvar *opstring* nil
  "The user may supply a list of opcode mnemonics to reconfigure the
     virtual machine's primitive instruction set. These operations
     should be entered as keywords, separated by commas, with no
     spaces. (UNSTABLE.)")

(defvar *testing-data-path* nil
  "Specify an independent testing dataset. If left as nil, then the
     main dataset will be partitioned according to --training-ratio.")

(defparameter *mutation-rate* 3/10
  "Chance of mutation per spawning event, expressed as percentage.")

(defparameter *metamutation-rate* 1/10
  "If over 0, then the rate of mutation is localized for each creature, and is     itself susceptible to mutation.")

(defparameter *metamutation-step* 1/50)

(defparameter *split-data* t
  "Set to nil to use the same dataset for both training and testing. I
     won't judge. *TRAINING-RATIO* is ignored if this is to set to nil.")

(defparameter *case-storage* nil
  "For efficiency at the cost of memory allocation, set to T and have
     creatures store hash-tables of the testing cases they are able to
     correctly classify. Principally for use with the Lexicase selection
     method.")

;; NOTE: adjustable lexicase population pool. easy to implement. may speed up
;; lexicase selection for large populations

;; could also do so with datasets, but benefits unclear

;; lexicase is doing quite badly right now.

(defparameter *sex* nil ;;:1pt
  "One-point crossover when set to :1pt, two-point/fixed-length when
     set to :2pt. Asexual reproduction (cloning, with certain
     mutation) when set to nil.")

(defparameter *mating-func* nil)

(defparameter *max-pack-size* 8)

(defparameter *lexicase-combatant-ratio* 1)

(defparameter *params-path* nil
  "An parameter file can be supplied, if desired. It should consist of
     a series of Lisp S-expressions of the form 
     (setf *PARAMETER-NAME* parameter-value)")

(defparameter *last-params-path* "LAST-PARAMS.SAV"
  "File in which to store the parameters used on the last run.")

(defparameter *lexicase-pool-ratio* 1)

(defparameter *scale-data* nil
  "Apply scaling function to keys in the hashtable, when using numeric
     data. Accepts a keyword value of: :linear-transform,
     or :mean-variance. Data used as-is if set to nil.")

(defparameter *verbose-report* t
  "Print a thorough report of GP results on the testing set. You might
     want to disable this for very large datasets.")

(defparameter *monitor-coverage* t)

(defparameter *pack-count* 100
  "The number of packs to establish on an island, once pack-formation
     begins.")

(defparameter *pack-thresh-by-era* 2000
  "If *PACKS* is T, then pack-formation will begin on an island once
    it surpasses this era.")

(defparameter *pack-thresh-by-difficulty* 100
  "If *PACKS* and *CASE-STORAGE* are both set to T, then pack
     formation will begin on an island when it has witnessed
     *PACK-THRESH-BY-DIFFICULTY* successful classifications of its most
     difficult (least-often-correctly-classified) case.")

(defparameter *pack-thresh-by-fitness* .9 
  "If *PACKS* is set to T, then pack formation will begin on an island
     when its best fitness score surpasses *PACK-THRESH-BY-FITNESS*.")

(defparameter *pack-thresh-by-plateau* 1000
  "If *PACKS* is T, then pack formation will begin once an island hits
     a fitness plateau, understood as a span of n consecutive eras without
     an increase it its best fitness.")

(defparameter *packs* nil)

(defvar *pack-method*)

(defparameter *pack-selection-method* :lexicase)

(defparameter *mingle-rate* 2/3)

(defparameter *lexicase-elitism* 3/4)

(defparameter *remove-introns* nil)

(defparameter *records* '())

(defvar *ttl* *max-len*
  "How many instructions can be executed in a sequence before the
     execution halts? Typically set to a multiple of *max-len* ((*
     *max-len* 1), for example).)")

(defvar *gc* nil)

(defvar *max-cal-depth* 1
  "Determines how many nested function calls are permitted, though CAL
     and BIN, in the virtual machine. What is gained in expressive
     power is paid for in time and island desynchronization.")

(defvar *save-every* 1000
  "Save the island ring every *SAVE-EVERY* rounds. Not a bad idea, so
     long as memory fault bugs persist.")

(defparameter *restore-island-ring* nil
  "Loads a saved island ring from supplied path name.")

(defparameter *sampling-policy*
  :div-by-class-num-shuffle
  "Currently only implemented for lexicase.
     Accepts: :div-by-class-num-shuffle, :full-shuffle [STUB]")

;; (defparameter *fitness-by-detection-rate* t
;;   "If set to T, then fitness will be measured in terms of detection
;;      rate instead of accuracy. Currently only implemented for lexicase
;;      selection.")

(defparameter *fitfunc-name* :n-ary-prop-vote
  "Accepted values: :n-ary-prop-vote, :detection-rate, :accuracy,
     :avg-acc-dr.")

(defparameter *sampling-ratio* 1/8)

(defparameter *tweakables*
  '(*menu*
    *debug*
    *gc*
    *stat-interval*
    *parallel*
    *dataset*
    *data-path*
    *testing-data-path*
    *split-data*
    *scale-data*
    *fitfunc-name*
    *sampling-policy*
    *training-ratio*
    *selection-method*
    *lexicase-combatant-ratio*
    *lexicase-elitism*
    *case-storage* ;;; case storage is buggy right now
    *number-of-islands*
    *population-size*
    *packs*
    *pack-count*
    *pack-thresh-by-fitness*
    *pack-thresh-by-era*
    *pack-thresh-by-difficulty*
    *pack-selection-method*
    *sex*
    *mutation-rate*
    *mingle-rate*
    *metamutation-rate*
    *migration-rate*
    *migration-size*
    *greedy-migration*
    *max-pack-size*
    *track-genealogy*
    *min-len*
    *max-len*
    *ttl* 
    *max-start-len*
    *remove-introns*
    *maxval*
    *opstring*
    *opcode-bits*
    *operations*
    *source-register-bits*
    *destination-register-bits*
    *max-cal-depth*
    *rounds*
    *target*
    *verbose-report*
    *save-every*
    *restore-island-ring*
    *sampling-ratio*
    *params-path*
    *last-params-path*))    


;;; IDEA: replace registers with stacks
;;; always hold at least one "anchor" element
;;; register instruction set operates on cars or pops
;;; new instruction: execstack, which executes a register

;;; advantages: intron detection?










