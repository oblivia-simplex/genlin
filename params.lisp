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


