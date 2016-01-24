;;#! /usr/bin/sbcl --script

;;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;;; Linear Genetic Algorithm
;;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;;; TODO: create user interface. NCurses?


(load "~/.sbclrc")

(ql:quickload :bordeaux-threads)

(defpackage :genetic.linear
  (:use :common-lisp
        :bordeaux-threads))

(in-package :genetic.linear)

(defparameter *project-path*
  "/home/oblivia/Projects/genetic-exercises/genetic-linear/")

(defparameter *tictactoe-path*
  (concatenate 'string *project-path* "datasets/TicTacToe/tic-tac-toe-balanced.data"))

(defparameter *iris-path*
  (concatenate 'string *project-path* "datasets/Iris/iris.data"))

(defun loadfile (filename)
  (load (merge-pathnames filename *load-truename*)))

(loop for f in '("auxilary.lisp"
                 "tictactoe.lisp"
                 "iris.lisp") do
     (loadfile (concatenate 'string *project-path* f)))

(defparameter *STOP* nil)

(defparameter *DEBUG* nil)

  
(defun dbg (&optional on-off)
  (case on-off
    ((on)  (setf *debug* t))
    ((off) (setf *debug* nil))
    (otherwise (setf *debug* (not *debug*)))))

(defparameter *parallel* nil)

(defparameter -print-lock- (make-lock "PRINT LOCK"))

(defparameter -migration-lock- (make-lock "migration lock"))

(defparameter *VERBOSE* nil)

(defparameter *ht* (make-hash-table :test 'equalp))

(defparameter *training-ht* (make-hash-table :test 'equalp))

(defparameter *testing-ht* (make-hash-table :test 'equalp))

(defparameter *dataset* 'iris)

;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; Genetic Parameters
;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
(defstruct creature fit seq eff gen home parents) ;; home tags the creature w its native deme

(defstruct island id of deme best age logger lock)

(defparameter *best* (make-creature :fit 0))

(defparameter *population* '())

(defparameter +ISLAND-RING+ '()) ;; make var later

(defparameter *number-of-islands* 4)

(defparameter *population-size* 800)

(defparameter *specimens* '())

(defparameter *mutation-rate* 15)

(defparameter *migration-size* 10) ;; percent

(defparameter *migration-rate* 777)

(defparameter *greedy-migration* t)

(defparameter *track-genealogy* t)

(defparameter *genealogical-fitness-statistics* t)

(defparameter *records* '())

(defun reset-records ()
  (setf *records*
        '(:same-eff-as-parents 0
          :fitter-than-parents 0
          :less-fit-than-parents 0
          :as-fit-as-parents 0)))
  

;; a migration is triggered every n loops through #'run-breeder

(defparameter *min-len* 2) ;; we want to prevent seqs shrinking to nil

(defparameter *max-len* 256) ;; max instruction length

(defparameter *max-start-len* 25) ;; max initial instruction length

;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; Logging
;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

(defun make-logger ()
  (let ((log '()))
    (lambda (&optional (entry nil))
      (cond ((eq entry 'CLEAR) (setf log '()))
            (entry (push entry log))
            (t log)))))

;;(defparameter =logger= ;; global -- will replace with local per island
;;  ;; Just an excuse to play with the let-over-lambda design pattern...
 ;; (make-logger))

;;(defun log-best-fit (iteration)
;;  (funcall =logger= (cons iteration (creature-fit *best*))))



;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;;                            Virtual machine
;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;;                           INSTRUCTION FIELDS
;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

;; --- Adjust these 3 parameters to tweak the instruction set. ---
;; --- The rest of the parameters will respond automatically   ---
;; --- but due to use of inlining for optimization, it may     ---
;; --- be necessary to recompile the rest of the programme.    ---

;; see if you can put the entire virtual machine in its own environment

(defparameter *opf* 3)   ;; size of opcode field, in bits

(defparameter *srcf* 3)  ;; size of source register field, in bits

(defparameter *dstf* 2)  ;; size of destination register field, in bits

;; --- Do not adjust the following five parameters manually ---

(defparameter *wordsize* (+ *opf* *srcf* *dstf*))

(defparameter *max-inst* (expt 2 *wordsize*)) ;; upper bound on inst size

(defparameter *opbits* (byte *opf* 0))

(defparameter *srcbits* (byte *srcf* *opf*))

(defparameter *dstbits* (byte *dstf* (+ *srcf* *opf*)))


(defun inst-schema-match (&rest instructions)
  "Returns a Holland-style schema that matches all of the instructions
passed in as arguments. This will be a bitvector (an integer) with a 1
for every bit that represents a match, and a 0 for every bit that
represents a clash."
  (ldb (byte *wordsize* 0) (apply #'logeqv instructions)))

(defun seq-schema-match (seq1 seq2)
  (map 'list #'inst-schema-match seq1 seq2))

(defun allonesp (inst)
  (= inst (ldb (byte *wordsize* 0) (lognot 0))))

(defun boolean-seq-schema-match (seq1 seq2)
  (mapcar #'allonesp (seq-schema-match seq1 seq2)))

(defun likeness (seq1 seq2)
  (div (length (remove-if #'null (boolean-seq-schema-match seq1 seq2)))
       (max (length seq1) (length seq2))))

;; --- Operations: these can be tweaked independently of the 
;; --- fields above, so long as their are at least (expt 2 *opf*)
;; --- elements in the *opcodes* vector. 

(declaim (inline MOV DIV MUL XOR CNJ DIS PMD ADD SUB MUL JLE)) 


(defun MOV (&rest args)
  "Copy the contents in SRC to register DST."
  (car args))

(defun DIV (&rest args)
  "A divide-by-zero-proof division operator."
  (if (some #'zerop args) 0
      (handler-case (/ (car args) (cadr args))
        (error () 0))))

(defun XOR (&rest args) ;; xor integer parts
  (logxor (floor (car args)) (floor (cadr args))))

(defun CNJ (&rest args)
  (logand (floor (car args)) (floor (cadr args))))

(defun DIS (&rest args)
  ;;x  (declare (type (cons fixnum)) args)
  (lognot (logand  (lognot (floor (car args)))
                   (lognot (floor (cadr args))))))

(defun PMD (&rest args)
  "Protected MOD."
  (if (some #'zerop args) (car args)
      (mod (car args) (cadr args))))

(defun ADD (&rest args)
  (+ (car args) (cadr args)))

(defun SUB (&rest args)
  (- (car args) (cadr args)))

(defun MUL (&rest args)
  (* (car args) (cadr args)))

(defun JLE (&rest args) ;; CONDITIONAL JUMP OPERATOR
  (if (<= (car args) (cadr args)) (1+ (caddr args))
      (caddr args)))

(defparameter *opcodes*
  (vector  #'DIV #'MUL #'SUB #'ADD   ;; basic operations    (2bit opcode)
           #'XOR #'PMD #'CNJ #'MOV)) ;; extended operations (3bit opcode)

;; adding the extended opcodes seems to result in an immense boost in the
;; population's fitness -- 0.905 is now achieved in the time it took to
;; reach 0.64 with the basic operation set. (For tic-tac-toe.)


;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; Parameters for register configuration.
;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

(defparameter *output-reg* '())

(defparameter *maxval* (expt 2 16)) ;; max val that can be stored in reg

(defparameter *minval* (expt 2 -16)) ;; floor this to 0

(defparameter *default-input-reg*
  (concatenate 'vector
               (loop for i from 1 to (- (expt 2 *srcf*) (expt 2 *dstf*))
                  collect (expt -1 i))))

(defparameter *default-registers*
  (concatenate 'vector #(0) (loop for i from 2 to (expt 2 *dstf*)
                               collect (expt -1 i))))

(defparameter *pc-idx*
  (+ (length *default-registers*) (length *default-input-reg*)))

(defparameter *initial-register-state*
  (concatenate 'vector
               *default-registers*
               *default-input-reg*
               #(0))) ;; PROGRAMME COUNTER

(defparameter *input-start-idx* (length *default-registers*))

(defparameter *input-stop-idx*
  (+ *input-start-idx* (length *default-input-reg*)))

;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; Functions for extracting fields from the instructions
;; and other low-level machine code operations.
;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

(declaim (inline src? dst? op?))

(defun src? (inst)
  (declare (type fixnum inst))
  (declare (type cons *srcbits*))
  (ldb *srcbits* inst))

(defun dst? (inst)
  (declare (type fixnum inst))
  (declare (type cons *dstbits*))
  (ldb *dstbits* inst))

(defun op? (inst)
  (declare (type fixnum inst))
  (declare (type cons *opbits*))
  (declare (type (simple-array function) *opcodes*))
  (aref *opcodes* (ldb *opbits* inst)))

(defun jmp? (inst) ;; ad-hoc-ish...
  (equalp (op? inst) #'JLE))

(defun enter-input (registers input)
  (let ((copy (copy-seq registers)))
    (setf (subseq copy *input-start-idx*
                  (+ *input-start-idx* (length input)))
          input)
    copy))

;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; debugging functions
;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

(defun print-registers (registers)
  (loop for i from 0 to (1- (length registers)) do
       (format t "R~d: ~6f ~c" i (aref registers i) #\TAB)
       (if (= 0 (mod (1+ i) 4)) (format t "~%")))
  (format t "~%"))

(defun inst->string (inst)
  (concatenate 'string
               (format nil "[~a  R~d, R~d]"
                       (func->string (op? inst))
                       (src? inst) (dst? inst))))

(defun disassemble-sequence (seq &key (registers *initial-register-state*)
                                   (input *default-input-reg*)
                                   (static nil))
  (hrule)
  (if static (disassemble-history :static seq)
      (execute-sequence seq :debug t :input input :registers registers))
  (hrule))

;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; Execution procedure
;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

;; encapsulate in its own let environment?

;; MESSY, and TOO SLOW. Try to delegate debugging output elsewhere. 


;; Almost certainly not threadsafe. 
(defparameter =history=
  (let ((hist '()))
    (lambda (&optional (entry nil))
      (cond ((eq entry 'CLEAR) (setf hist '()))
            (entry (push entry hist))
            (t hist)))))

;; is it my imagination, or did avg fitness drop after making these edits?

(defun history-push-test (seq reg)
  (funcall =history= (cons seq reg)))

(defun history-flush ()
  (funcall =history= 'CLEAR))

(defun history-print (&optional len)
  (let ((h (funcall =history=)))
    (print (subseq h 0 len))))

;;; IS THE REGISTER STATE PERSISTING BETWEEN SEQUENCES?
(defun disassemble-history (&key (len 0) (all t) (static nil))
  
  "If passed a sequence in the static field, disassemble it without
any reference to register states. If not, disassemble the last len
entries in the history stack, tracking changes to the registers."
  (let* ((history (funcall =history=))
         (len (if all (length history) len))
         (story (if static
                    (coerce static 'list)
                    (reverse (subseq history 0 len)))))
    (loop for couplet in story do
         (let* ((registers (if static
                               (copy-seq *initial-register-state*)
                               (cdr couplet)))
                (inst (if static couplet (car couplet))))
             (format t "~8,'0b  ~a  "
                     inst
                     (inst->string inst))
             (if (not static) (format t ";; now R~d = ~f ;; PC = ~d~%"
                                      (dst? inst)
                                      (aref registers (dst? inst))
                                      (aref registers *pc-idx*))
                 (format t "~%")))))
  (if static (hrule)))

(defun execute-sequence (seq &key (registers *initial-register-state* )
                               (input *default-input-reg*) (output nil)
                               (debug nil))
  "Takes a sequence of instructions, seq, and an initial
register state vector, registers, and then runs the virtual machine,
returning the resulting value in R0."
  (declare (type fixnum *input-start-idx*))
  (declare (inline src? dst? op?))
  (flet ((save-state (inst regs)
           (funcall =history= (cons inst regs))))
    (let ((regs (copy-seq registers))
          (seqlen (length seq))
          (debugger (or debug *debug*)))
      ;; the input values will be stored in read-only regs
      (setf (subseq regs *input-start-idx*
                    (+ *input-start-idx* (length input))) input)
      (unless (zerop seqlen)
        (loop do
           ;; Fetch the next instruction
             (let* ((inst (aref seq (aref regs *pc-idx*)))
                    ;; Determine the target register
                    (D (if (jmp? inst) *pc-idx* (dst? inst))))
               ;; Increment the programme counter
               (incf (aref regs *pc-idx*))
               ;; Perform the operation and store the result in [dst]
               (setf (aref regs D)
                     (max (min (apply (op? inst)
                                      (list (aref regs (src? inst))
                                            (aref regs (dst? inst))
                                            (aref regs *pc-idx*))) *maxval*)
                          *minval*))
               ;; Save history for debugger
               (and debugger (save-state inst regs)
                    (disassemble-history :len 1 :all nil))
               
               (and (>= (aref regs *pc-idx*) seqlen)
                    (return)))))
      (mapcar #'(lambda (i) (aref regs i)) output))))

;; ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;; Intron Removal
;; ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

(defun remove-introns (seq &key (output '(0)))
  "Filters out non-effective instructions from a sequence -- instructions
that have no bearing on the final value(s) contained in the output 
register(s)."
  (let ((efr output)
        (efseq '()))
    (loop for i from (1- (length seq)) downto 0 do
         (let ((inst (aref seq i)))
           (when (member (dst? inst) efr)
             (push (src? inst) efr)
             (push inst efseq)
             (when (and (not (zerop i)) (jmp? (aref seq (1- i))))
               ;; a jump immediately before an effective instruction
               ;; is also an effective instruction. 
               (let ((prevjmp (aref seq (1- i))))
                 (push (src? prevjmp) efr)
                 (push (dst? prevjmp) efr))))))
    (coerce efseq 'vector)))

;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; Functions related to fitness measurement
;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

(defparameter .fitfunc. nil)

(defparameter .training-hashtable. nil)

(defparameter .testing-hashtable. nil)

(defparameter .out-reg. nil)

(defun get-fitfunc ()
  .fitfunc.)

(defun get-out-reg ()
  .out-reg.)

(defun peek-fitness-environment ()
  (format t ".fitfunc. = ~a~%.training-hashtable. = ~a~%.testing-hashtable. = ~a~%.out-reg. = ~a~%" .fitfunc. .training-hashtable. .testing-hashtable. .out-reg.))

(defun set-fitfunc (name)
  (case name
    ((binary-1) (setf .fitfunc. #'fitness-binary-classifier-1)
     (setf .out-reg. '(0)))
    ((binary-2) (setf .fitfunc. #'fitness-binary-classifier-2)
     (setf .out-reg. '(0)))
    ((binary-3) (setf .fitfunc. #'fitness-binary-classifier-3)
     (setf .out-reg. '(0 1)))
    ((ternary-1) (setf .fitfunc. #'fitness-ternary-classifier-1)
     (setf .out-reg. '(0 1 2)))
    ((ternary-2) (setf .fitfunc. #'fitness-ternary-classifier-2)
     (setf .out-reg. '(0 1 2)))   
    (otherwise (error "FITFUNC NICKNAME NOT RECOGNIZED. MUST BE ONE OF THE FOLLOWING: BINARY-1, BINARY-2, BINARY-3, TERNARY-1."))))

(defun init-fitness-env (&key fitfunc-name training-hashtable testing-hashtable)
  "Works sort of like a constructor, to initialize the fitness 
environment."
  (set-fitfunc fitfunc-name)
  (setf .training-hashtable. training-hashtable)
  (setf .testing-hashtable. testing-hashtable))

;; DO NOT TINKER WITH THIS ANY MORE. 
(defun sigmoid-error(raw goal)
  ;; goal is a boolean value (t or nil)
  ;; raw is, typically, the return value in r0
  (let ((divisor (/ *maxval* 10000)))
    (flet ((sigmoid (x)
             (tanh (/ x divisor))))
      ;; if raw is < 0, then (sigmoid raw) will be between
      ;; 0 and -1. If the goal is -1, then the final result
      ;; will be (-1 + p)/2 -- a value between -0.5 and -1,
      ;; but taken absolutely as a val between 0.5 and 1.
      ;; likewise when raw is > 0.
      (/ (abs (+ (sigmoid raw) goal)) 2))))

;; design a meta-gp to evolve the sigmoid function?
(defun fitness-binary-classifier-1 (crt &key (ht .training-hashtable.))
    "Fitness is gauged as the output of a sigmoid function over the
output in register 0 times the labelled value of the input (+1 for
positive, -1 for negative."
  (if (null .out-reg.) (setf .out-reg. '(0)))
  (unless (> 0 (length (creature-eff crt)))
    (setf (creature-eff crt) ;; assuming eff is not set, if fit is not.
          (remove-introns (creature-seq crt) :output '(0))))
  (let ((seq (creature-eff crt)))
    (let ((results
           (loop for pattern
              being the hash-keys in ht collect
                (sigmoid-error
                 (car (execute-sequence seq
                               ;;         :registers *initial-register-state*
                                        :input pattern
                                        :output '(0) )) ;; raw
                 (gethash pattern ht))))) ;; goal
      (and *debug* *verbose*
           (format t "SEQUENCE:~a~%RESULTS:~%~a~%" seq results))
      (/ (reduce #'+ results) (length results)))))

(defun fitness-binary-classifier-2 (crt &key (ht .training-hashtable.))
  (if (null .out-reg.) (setf .out-reg. '(0)))
  (unless (> 0 (length (creature-eff crt)))
    (setf (creature-eff crt) ;; assuming eff is not set, if fit is not.
          (remove-introns (creature-seq crt) :output .out-reg.)))
  (let ((hit 0)
        (miss 0)
        (seq (creature-eff crt)))
    (loop for pattern being the hash-keys in ht
       using (hash-value v) do
         (let ((f ;; shooting in the dark, here. 
                (tanh (/ (car (execute-sequence seq :input pattern
                                                :output .out-reg.)) 300))))
           (if (> (* f v) .5) (incf hit) (incf miss))))
    (if (zerop miss) 1
        (/ hit (+ hit miss)))))

(defun fitness-binary-classifier-3 (crt &key (ht .training-hashtable.))
  "Measures fitness as the proportion of the absolute value
contained in the 'correct' register to the sum of the values in
registers 0 and 1. Setting R0 for negative (-1) and R1 for
positive (+1)."
  (if (null .out-reg.) (setf .out-reg. '(0 1)))
  (unless (> 0 (length (creature-eff crt)))
    (setf (creature-eff crt) ;; assuming eff is not set, if fit is not.
          (remove-introns (creature-seq crt) :output '(0 1 2))))
  (labels ((deneg (n)
             (abs n))
           (to-idx (x) ;; translates hash value to register index
             (if (< 0 x) 0 1))
           (check (out v-idx)
             (if (> (deneg (elt out v-idx))
                    (deneg (elt out (ldb (byte 1 0) (lognot v-idx)))))
               1
               0)))
    (let ((acc1 0)
          (acc2 0)
          (w1 0.4)
          (w2 0.6)
          (seq (creature-eff crt)))
      
      (loop for pattern being the hash-keys in ht
         using (hash-value val) do
           (let ((output (execute-sequence seq
                                           :input pattern
                                           :output '(0 1))))
             ;; measure proportion of correct vote to total votes
             ;; compare r0 to sum if val < 0
             ;; compare r1 to sum if val >= 0
             (incf acc1 (DIV (deneg (nth (to-idx val) output))
                             (reduce #'+ (mapcar #'deneg output))))
             (incf acc2 (check output (to-idx val)))))

      (setf acc1 (float (/ acc1 (hash-table-count ht))))
      (setf acc2 (float (/ acc2 (hash-table-count ht))))
      (+ (* acc1 w1) (* acc2 w2)))))

;; In Linear Genetic Programming, the author suggests using the
;; sum of two fitness measures as the fitness: 

;; This classifier needs a little bit of tweaking. 
(defun fitness-ternary-classifier-1 (crt &key (ht .training-hashtable.))
  "Where n is the target register, measures fitness as the ratio of
Rn to the sum of all output registers R0-R2 (wrt absolute value)."
  (if (null .out-reg.) (setf .out-reg. '(0 1 2)))
  (unless (> 0 (length (creature-eff crt)))
    (setf (creature-eff crt) ;; assuming eff is not set, if fit is not.
          (remove-introns (creature-seq crt) :output '(0 1 2))))
  (let ((acc 0)
        (seq (creature-eff crt)))
    (loop for pattern being the hash-keys in ht
       using (hash-value i) do
         (let ((output (execute-sequence seq
                                         :input pattern
                                         :output .out-reg.)))
           (incf acc (DIV (abs (nth i output))
                          (reduce #'+ (mapcar #'abs output))))))
    (/ acc (hash-table-count .training-hashtable.))))

(defun fitness-ternary-classifier-2 (crt &key (ht .training-hashtable.))
  "Where n is the target register, measures fitness as the ratio of
Rn to the sum of all output registers R0-R2 (wrt absolute value)."
  (if (null .out-reg.) (setf .out-reg. '(0 1 2)))
  (unless (> 0 (length (creature-eff crt)))
    (setf (creature-eff crt) ;; assuming eff is not set, if fit is not.
          (remove-introns (creature-seq crt) :output '(0 1 2))))
  (let ((acc1 0)
        (acc2 0)
        (w1 0.4)
        (w2 0.6)
        (seq (creature-eff crt)))
    (loop for pattern being the hash-keys in ht
       using (hash-value i) do
         (let ((output (execute-sequence seq
                                         :input pattern
                                         :output .out-reg.)))
           (incf acc1 (DIV (abs (nth i output))
                           (reduce #'+ (mapcar #'abs output))))
           (incf acc2 (if (> (* (abs (nth i output)) 3) (reduce #'+ output))
                          1 0))))
    (+ (* w1 (/ acc1 (hash-table-count ht)))
       (* w2 (/ acc2 (hash-table-count ht))))))

(defun fitness (crt &key (ht .training-hashtable.))
  "Measures the fitness of a specimen, according to a specified
fitness function."
  (unless (creature-fit crt)  
    (setf (creature-fit crt)
          (funcall .fitfunc. crt :ht ht)))
  (creature-fit crt))

;; there needs to be an option to run the fitness functions with the testing hashtable. 

;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; Genetic operations (variation)
;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

(defun imutate-flip (inst)
  "Flips a random bit in the instruction."
  (declare (type fixnum inst))
  (logxor inst (ash 1 (random *wordsize*))))

;; Some of these mutations are destructive, some are not, so we'll
;; rig each of them to return the mutated sequence, and we'll treat
;; them as if they were purely functional, and combine them with an
;; setf to mutate their target. 

(defun smutate-swap (seq)
  "Exchanges the position of two instructions in a sequence."
  (declare (type simple-array seq))
  (and *debug* (print "smutate-swap"))
  (let* ((len (length seq))
         (i (random len))
         (j (random len))
         (tmp (elt seq i)))
    (setf (elt seq i) (elt seq i))
    (setf (elt seq j) tmp))
  seq)

(defun smutate-grow (seq)
  "Adds another (random) instruction to the end of the sequence."
  (declare (type simple-array seq))
  (and *debug* (print "smutate-append"))
  (setf seq (concatenate 'vector seq `(,(random #x100))))
  seq)

(defun smutate-imutate (seq)
  "Applies imutate-flip to a random instruction in the sequence."
  (declare (type simple-array seq))
  (and *debug* (print "smutate-imutate"))
  (let ((idx (random (length seq))))
    (setf (elt seq idx) (imutate-flip (elt seq idx))))
  seq)

(defparameter *mutations*
  (vector #'smutate-grow #'smutate-imutate #'smutate-swap))

(defun random-mutation (seq)
  (declare (type simple-array seq))
  (apply (aref *mutations* (random (length *mutations*))) `(,seq)))

(defun maybe-mutate (seq)
  (declare (type simple-array seq))
  (if (< (random 100) *mutation-rate*)
      (random-mutation seq)
      seq))

(defun crossover (p0 p1)
  ;;  (declare (type cons p0 p1))
  ;;  (declare (optimize (speed 2)))
  (let* ((p00 (creature-seq p0))
         (p01 (creature-seq p1))
         (parents (sort (list p00 p01) #'(lambda (x y) (< (length x) (length y)))))
         (father (car parents))  ;; we trim off the car, which holds fitness
         (mother (cadr parents)) ;; let the father be the shorter of the two
         (r-align (random 2)) ;; 1 or 0
         (offset (* r-align (- (length mother) (length father))))
         (daughter (copy-seq mother))
         (son (copy-seq father))
         (idx0 (random (length father)))
         (idx1 (random (length father)))
         (minidx (min idx0 idx1))
         (maxidx (max idx0 idx1))
         (children))
    ;;    (declare (type cons mother father daughter son))
    ;;    (declare (type fixnum idx0 idx1 minidx maxidx offset r-align))
                                        ;(format t "minidx: ~d  maxidx: ~d  offset: ~d~%" minidx maxidx offset)
    (setf (subseq daughter (+ offset minidx) (+ offset maxidx))
          (subseq father minidx maxidx))
    (setf (subseq son minidx maxidx)
          (subseq mother (+ offset minidx) (+ offset maxidx)))         
    ;;    (format t "mother: ~a~%father: ~a~%daughter: ~a~%son: ~a~%"
    ;;            mother father daughter son)
    (setf children (list (make-creature :seq son)
                         (make-creature :seq daughter)))))

(defun mate (p0 p1 &key (genealogy *track-genealogy*)
                               (output-registers .out-reg.))
  (let ((children (crossover p0 p1)))
    ;; mutation
    (loop for child in children do
         (setf (creature-seq child) (maybe-mutate (creature-seq child)))
         (setf (creature-eff child) (remove-introns
                                     (creature-seq child)
                                     :output output-registers))
         (loop for parent in (list p0 p1) do
              (if (equalp (creature-eff child) (creature-eff parent))
                  (setf (creature-fit child) (creature-fit parent)
                        (getf *records* :same-eff-as-parents)
                        (1+ (getf *records* :same-eff-as-parents)))))
         (when *genealogical-fitness-statistics* 
           (setf (creature-fit child) (fitness child))
           (loop for parent in (list p0 p1) do
                (cond ((> (creature-fit child) (creature-fit parent))
                       (incf (getf *records* :fitter-than-parents)))
                      ((< (creature-fit child) (creature-fit parent))
                       (incf (getf *records* :less-fit-than-parents)))
                      (t (incf (getf *records* :as-fit-as-parents)))))))
    ;; could test fitness from here, to easily measure to what extent the
    ;; variation operators tend to produce more or less fit offspring.
    
    (when genealogy
      (mapcar #'(lambda (x) (setf (creature-gen x)
                             (1+ (max (creature-gen p0) (creature-gen p1)))
                             (creature-parents x)
                             (list p0 p1))) children))
    children))

;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; Selection functions
;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

(defun update-best-if-better (crt island &key (verbose t))
  (let ((verbosity (if *parallel* nil verbose)))
  (when (> (creature-fit crt) (creature-fit (island-best island)))
    (setf (island-best island) crt)
    ;; log updates in the island's own logger
    (funcall (island-logger island) `(,(island-age island) .
                                       ,(creature-fit crt)))
    (when verbosity 
      (print-new-best-update island)))))

(defun tournement! (island &key (genealogy *track-genealogy*))
  "Tournement selction function: selects four combatants at random
from the population provide, and lets the two fittest reproduce. Their
children replace the two least fit of the four. Disable genealogy to
facilitate garbage-collection of the dead, at the cost of losing a
genealogical record."
  (let ((population)
        (combatants))
    (setf (island-deme island) (shuffle (island-deme island))
          population (cddddr (island-deme island))
          combatants (subseq (island-deme island) 0 4 ))
    (and *debug* (format t "COMBATANTS BEFORE: ~a~%" combatants))
    (loop for combatant in combatants do
         (unless (creature-fit combatant)
           (fitness combatant)))
    (and *debug* (format t "COMBATANTS AFTER: ~a~%" combatants))
    (let* ((ranked (sort combatants
                         #'(lambda (x y) (< (creature-fit x)
                                       (creature-fit y)))))
           (best-in-show  (car (last ranked)))
           (parents (cddr ranked))
           (children (apply #'(lambda (x y) (mate x y
                                                  :genealogy genealogy))
                            parents)))
          ;; (the-dead (subseq ranked 0 2)))
;;      (FORMAT T "RANKED: ~A~%BEST-IN-SHOW: ~A~%" ranked best-in-show)
      (incf (island-age island))
      (update-best-if-better best-in-show island)
      ;; Now replace the dead with the children of the winners
      (mapcar #'(lambda (x) (setf (creature-home x) (island-id island))) children)
      (loop for creature in (concatenate 'list parents children) do
           (push creature population))
      (setf (island-deme island) population)
      ;;  (mapcar #'(lambda (x y) (setf (elt population (cdr y)) x)) children the-dead)
      ;;(and t (format t "CHILDREN: ~A~%" children))
      (island-best island))))

(defun spin-wheel (wheel top)
  "A helper function for f-roulette."
  (let ((ball (if (> top 0)
                  (random (float top))
                  (progn (print "*** ALERT: ZERO TOP IN SPIN-WHEEL ***")
                         0))) ;; hopefully this doesn't happen often.
        (ptr (car wheel)))
    (loop named spinning for slot in wheel do
         (when (< ball (car slot))
           (return-from spinning))
         (setf ptr slot)) ;; each slot is a cons pair of a float and creature
    (cdr ptr))) ;; extract the creature from the slot the ptr lands on

(defun f-roulette (island &key (genealogy t))
  "Generational selection function: assigns each individual a
probability of mating that is proportionate to its fitness, and then
chooses n/2 mating pairs, where n is the size of the population, and
returns a list of the resulting offspring. Turn off genealogy to save
on memory use, as it will prevent the dead from being
garbage-collected."
  (let* ((population (island-deme island))
         (tally 0)
         (popsize (length population))
         (wheel (loop for creature in population
                   collect (progn
                             (let ((f (float (fitness creature))))
                               (incf (island-age island))
                               (update-best-if-better creature island)
                               (incf tally f)
                               (cons tally creature)))))
         ;; the roulette wheel is now built
         (breeders (loop for i from 1 to popsize
                      collect (spin-wheel wheel tally)))
         (half (/ popsize 2)) ;; what to do when island population is odd?
         (children)
         (mothers (subseq breeders 0 half))
         (fathers (subseq breeders half popsize))
         (broods (mapcar #'(lambda (x y) (mate x y :genealogy genealogy))
                         mothers fathers)))
    (setf children (apply #'concatenate 'list broods))
    (mapcar #'(lambda (x) (setf (creature-home x) (island-id island))) children)
    children))

(defun greedy-roulette! (island)
  "New and old generations compete for survival. Instead of having the
new generation replace the old, the population is replaced with the
fittest n members of the union of the old population and the new (as
resulting from f-roulette). Changes the population in place. Takes
twice as long as #'roulette!"
  (let ((popsize (length (island-deme island))))
    (setf (island-deme island)
          (subseq (sort (concatenate 'list (island-deme island)
                                     (f-roulette island))
                        #'(lambda (x y) (> (fitness x)
                                      (fitness y))))
                  0 popsize))))

(defun roulette! (island)
  "Replaces the given island's population with the one generated by
f-roulette."
  (setf (island-deme island)
        (f-roulette island)))

;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; Population control
;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

(defun de-ring (island-ring)
  (subseq island-ring 0 (island-of (car island-ring))))

(defun extract-seqs-from-island-ring (island-ring)
  "Returns lists of instruction-vectors representing each creature on
each island. Creatures dwelling on the same island share a list."
  (mapcar #'(lambda (x) (mapcar #'(lambda (y) (creature-seq y)) (island-deme x)))
          (de-ring island-ring)))

(defun fitsort-deme (deme)
  (flet ((nil0 (n)
           (if (null n) 0 n)))
    (let ((population (copy-seq deme)))
      (sort population
            #'(lambda (x y) (< (nil0 (creature-fit x))
                          (nil0 (creature-fit y))))))))

;; this needs to be mutex protected
(defun reorder-demes (island-ring &key (greedy t))
  "Shuffles the demes of each island in the ring."
  (loop for isle in (de-ring island-ring) do
       (acquire-lock (island-lock isle))
       (setf (island-deme isle)
             (if greedy
                 (fitsort-deme (island-deme isle))
                 (shuffle (island-deme isle))))
       (release-lock (island-lock isle))))
  
;; now chalk-full of locks. 
(defun migrate (island-ring &key (emigrant-percent 10)
                              (greedy *greedy-migration*))
  "Migrates a randomly-populated percentage of each island to the
preceding island in the island-ring. The demes of each island are
shuffled in the process."
    (let* ((island-pop (length (island-deme (car island-ring))))
           (emigrant-count (floor (* island-pop (/ emigrant-percent 100))))
           (emigrant-idx (- island-pop emigrant-count))
           (buffer))
      (reorder-demes island-ring :greedy greedy)
      ;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; (format t "~%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%~%EMIGRANT FITNESS:~%~A~%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%~%"
      ;;         (mapcar #'(lambda (z) (mapcar #'(lambda (y) (creature-fit y)) z)) 
      ;;                 (mapcar #'(lambda (x) (subseq x emigrant-idx))
      ;;                         (mapcar #'island-deme
      ;;                                 (de-ring +island-ring+)))))
                    ;;;;;;;;;;;;;;;;;;;;;;;;
      (setf buffer (subseq (island-deme (car island-ring)) emigrant-idx))
      (loop
         for (isle-1 isle-2) on island-ring
         for i from 1 to (1- (island-of (car island-ring))) do
           (acquire-lock (island-lock isle-1))
           (acquire-lock (island-lock isle-2))
           (setf (subseq (island-deme isle-1) emigrant-idx)
                 (subseq (island-deme isle-2) emigrant-idx))
           (release-lock (island-lock isle-1))
           (release-lock (island-lock isle-2)))
      (acquire-lock (island-lock
                     (elt island-ring (1- (island-of (car island-ring))))))
      (setf (subseq (island-deme
                     (elt island-ring (1- (island-of (car island-ring)))))
                    0 emigrant-count) buffer)
      (release-lock (island-lock
                     (elt island-ring (1- (island-of (car island-ring))))))
      island-ring))    

(defun island-log (isle entry)
  (funcall (island-logger isle) entry))

(defun island-by-id (island-ring id)
  (let ((limit (island-of (car island-ring))))
    (find id island-ring :key #'island-id :end limit)))
  
(defun population->islands (population number-of-islands)
  "Takes a population list and returns a circular list of 'islands' --
structs that contain portions of the initial population in 'demes',
along with a handful of other attributes. Note that this list is
circular, and so it must be cut with the de-ring function before
applying, say, mapcar or length to it, in most cases."
  (let ((i 0)
        (islands (loop for i from 0 to (1- number-of-islands)
                    collect (make-island :id i
                                         :of number-of-islands
                                         :age 0
                                         ;; stave off <,> errors w null crt
                                         :best (make-creature :fit 0)
                                         :logger (make-logger)
                                         :lock (make-lock
                                                (format nil "isle-~d-lock"
                                                        i))))))
    (loop for creature in population do ;; allocate pop to the islands
         (let ((home-isle (mod i number-of-islands)))
           (setf (creature-home creature) home-isle)
           (push creature (island-deme (elt islands home-isle)))
           (incf i)))
    (circular islands))) ;; circular is defined in auxiliary.lisp
;; it forms the list (here, islands) into a circular linked list, by
;; setting the cdr of its last element as a pointer to its car.
             
(defun islands->population (island-ring)
  (apply #'concatenate 'list (mapcar #'(lambda (x) (island-deme x))
                                     (de-ring island-ring))))
  
(defun spawn-sequence (len)
  (concatenate 'vector (loop repeat len collect (random *max-inst*))))

(defun spawn-creature (len)
  (make-creature :seq (spawn-sequence len)
                 :gen 0))

(defun init-population (popsize slen &key (number-of-islands 4))
  (let ((adjusted-popsize (+ popsize (mod popsize (* 2 number-of-islands)))))
    ;; we need to adjust the population size so that there exists an integer
    ;; number of mating pairs on each island -- so long as we want roulette to
    ;; work without a hitch. 
    (population->islands (loop for i from 0 to (1- adjusted-popsize) collect
                            (spawn-creature (+ *min-len*
                                               (random slen))))
                       number-of-islands)))

(defun partition-data (hashtable ratio)
  (flet ((shuffle (l)
           (sort l #'(lambda (x y) (= 0 (random 2))))))
    (let* ((size (hash-table-count hashtable))
           (training-size (floor (* ratio size)))
           (keys (loop for k being the hash-keys in hashtable collect k))
           (shuffled (shuffle keys))
           (training (make-hash-table :test 'equalp))
           (testing (make-hash-table :test 'equalp)))
      (loop for i from 1 to size do
           (let ((k (pop shuffled))
                 (dst-ht (if (< i training-size) training testing)))
             (setf (gethash k dst-ht) (gethash k hashtable))))
      (cons training testing))))

;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; User interface functions
;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

(defun setup (&key (ratio 4/5)
                (dataset *dataset*)
                (fitfunc-name)
                (popsize *population-size*)
                (number-of-islands *number-of-islands*)
                (migration-rate *migration-rate*)
                (migration-size *migration-size*)
                (greedy-migration *greedy-migration*)
                (file))
  (let ((filename)
        (hashtable)
        (training+testing))
    (setf *number-of-islands* number-of-islands
          *population-size* popsize
          *dataset* dataset
          *migration-rate* migration-rate
          *migration-size* migration-size
          *greedy-migration* greedy-migration)
    (reset-records)
    (case dataset
      ((tictactoe)
       (unless file (setf filename *tictactoe-path*))
       (unless fitfunc-name (setf fitfunc-name 'binary-3))
       (setf hashtable (ttt-datafile->hashtable
                        :filename filename :int t :gray t)))
      ((iris)
       (unless file (setf filename *iris-path*))
       (unless fitfunc-name (setf fitfunc-name 'ternary-1))
       (setf hashtable (iris-datafile->hashtable :filename filename)))
      (otherwise (error "DATASET UNKNOWN (IN SETUP)")))
    (setf training+testing (partition-data hashtable ratio))
    (init-fitness-env :training-hashtable (car training+testing)
                      :testing-hashtable  (cdr training+testing)
                      :fitfunc-name fitfunc-name)
    ;; (setf *best* (make-creature :fit 0))
    (setf +ISLAND-RING+ (init-population popsize *max-start-len*
                                   :number-of-islands number-of-islands))
    (format t "~%")
    (hrule)
    (format t "[!] DATA READ AND PARTITIONED INTO TRAINING AND TESTING TABLES~%[!] POPULATION OF ~d INITIALIZED~%[!] POPULATION SPLIT INTO ~d DEMES AND ISLANDS POPULATED~%[!]  FITNESS ENVIRONMENT INITIALIZED:~%" popsize number-of-islands)
    (hrule)
    (peek-fitness-environment)
    (hrule)
    (print-params)
    hashtable))

;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; Runners
;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

(defun best-of-all (island-ring)
  (let ((best-so-far (make-creature :fit 0)))
    (loop for isle in (de-ring island-ring) do
         (if (> (creature-fit (island-best isle)) (creature-fit best-so-far))
             (setf best-so-far (island-best isle))))
    ;; you could insert a validation routine here
    (setf *best* best-so-far)
    best-so-far))

(defun release-all-locks (island-ring)
  "Mostly for running from the REPL after aborting the programme."
  (release-lock -migration-lock-)
  (loop for island in (de-ring island-ring) do
       (release-lock (island-lock island))))

(defun evolve (&key (method #'tournement!) (dataset *dataset*)
                 (rounds 10000) (target 0.97)
                 (stat-interval 500) (island-ring +island-ring+) 
                 (migration-rate *migration-rate*)
                 (migration-size *migration-size*)
                 (parallelize t))
  ;; adjustments needed to add fitfunc param here. 
  (setf *STOP* nil)
  (setf *parallel* parallelize)
  ;;; Just putting this here temporarily:
  (time (block evolver
          (loop for i from 1 to rounds do
               (let ((isle (pop island-ring)))
                 ;; island-ring is circular, so pop
                 ;; will cycle & not exhaust it
                 (labels ((dispatcher ()
                            ;; we don't want more than one thread per island.
                            (acquire-lock (island-lock isle))
                            (funcall method isle)
                            (release-lock (island-lock isle)))
                          (dispatch ()
                            (if parallelize
                                (make-thread #'dispatcher)
                                (dispatcher))))
                   (dispatch)
                   (when (and (> migration-rate 0) (= 0 (mod i migration-rate)))
                     (acquire-lock -migration-lock-)
                     (princ ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>")
                     (princ " MIGRATION EVENT ")
                     (format t "<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<~%")
                     (migrate island-ring :emigrant-percent migration-size)
                     (release-lock -migration-lock-))
                   (when (= 0 (mod i stat-interval))
                     (print-statistics island-ring))
                   (when (or (> (creature-fit (island-best isle)) target)
                             *STOP*)
                     (format t "~%TARGET OF ~f REACHED AFTER ~d ROUNDS~%"
                             target i)
                     (return-from evolver)))))))
  (format t "BEST:~%")
  (print-creature (best-of-all island-ring))
  (classification-report *best* dataset)
  (loop for isle in (de-ring island-ring) do
       (plot-fitness isle))
  (when *genealogical-fitness-statistics* (genealogical-fitness-stats)))

(defun run-with-defaults ()
  (setup)
  (evolve))

;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; Debugging functions and information output
;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

(defun print-params ()
  "Prints major global parameters, and some statistics, too."
  (hrule)
  (format t "[+] INSTRUCTION SIZE:     ~d bits~%" *wordsize*)
  (format t "[+] # of RW REGISTERS:    ~d~%" (expt 2 *dstf*))
  (format t "[+] # of RO REGISTERS:    ~d~%" (expt 2 *srcf*))
  (format t "[+] PRIMITIVE OPERATIONS: ")
  (loop for i from 0 to (1- (expt 2 *opf*)) do
       (format t "~a " (func->string (aref *opcodes* i))))
  (format t "~%[+] POPULATION SIZE:      ~d~%"
          *population-size*)
  (format t "[+] NUMBER OF ISLANDS:    ~d~%" (island-of (car +island-ring+)))
  (format t "[+] MIGRATION RATE:       ONCE EVERY ~D CYCLES~%" *migration-rate*)
  (format t "[+] MIGRATION SIZE:       ~D%~%" *migration-size*)
  (format t "[+] MUTATION RATE:        ~d%~%" *mutation-rate*)
  (format t "[+] MAX SEQUENCE LENGTH:  ~d~%" *max-len*)
  (format t "[+] MAX STARTING LENGTH:  ~d~%" *max-start-len*)
  (format t "[+] # of TRAINING CASES:  ~d~%"
          (hash-table-count *training-ht*))
  (format t "[+] # of TEST CASES:      ~d~%" (hash-table-count *testing-ht*))
  (format t "[+] FITNESS FUNCTION:     ~s~%" (get-fitfunc))
  (hrule))

(defun likeness-to-specimen (population specimen)
  "A weak, but often informative, likeness gauge. Assumes gene alignment,
for the sake of a quick algorithm that can be dispatched at runtime
without incurring delays."
  (float (div (reduce #'+
                      (mapcar #'(lambda (x) (likeness (creature-eff specimen)
                                                 (creature-eff x)))
                              (remove-if
                               #'(lambda (x) (equalp #() (creature-eff x)))
                               population)))
              (length population))))

(defun print-fitness-by-gen (logger)
  (flet ((.*. (x y)
           (if (numberp y)
               (* x y)
               NIL)))
  (let ((l (funcall logger)))
    (loop for (e1 e2) on l by #'cddr do
         (format t "~c~d:~c~5,4f %~c~c~d:~c~5,4f %~%" #\tab
                 (car e1) #\tab (.*. 100 (cdr e1)) #\tab #\tab
                 (car e2) #\tab (.*. 100 (cdr e2)))))))

(defun opcode-census (population)
  (let* ((buckets (make-array (expt 2 *opf*)))
         (instructions (reduce #'(lambda (x y) (concatenate 'list x y))
                               (mapcar #'creature-eff population)))
         (sum (length instructions)))
    (loop for inst in instructions do
         (incf (aref buckets (ldb (byte *opf* 0) inst))))
    (loop repeat (/ (length buckets) 2)
       for x = 0 then (+ x 2)
       for y = 1 then (+ y 2) do
         (format t "~C~A: ~4D~C(~5,2f%)~C" #\tab
                 (func->string (aref *opcodes* x))
                 (aref buckets x)
                 #\tab
                 (* 100 (div (aref buckets x) sum)) #\tab)
         (format t "~C~A: ~4D~c(~5,2f%)~%" #\tab
                 (func->string (aref *opcodes* y))
                 (aref buckets y)
                 #\tab
                 (* 100 (div (aref buckets y) sum))))))

(defun percent-effective (crt &key (out *output-reg*))
  (when (equalp #() (creature-eff crt))
    (setf (creature-eff crt) (remove-introns (creature-seq crt)
                                             :output out)))
  (float (/ (length (creature-eff crt)) (length (creature-seq crt)))))

(defun average-effective (population)
  (/ (reduce #'+ (mapcar #'percent-effective population))
     (length population)))


(defun print-statistics (island-ring)
  (mapcar #'print-statistics-for-island
          (subseq island-ring 0 (island-of (car island-ring))))
  (hrule))

(defun print-statistics-for-island (island)
  ;; eventually, we should change *best* to list of bests, per deme.
  ;; same goes for logger. 
  (hrule)
  (format t "              *** STATISTICS FOR ISLAND #~d AT AGE ~d ***~%"
          (island-id island)
          (island-age island))
  (hrule)
  (format t "[*] BEST FITNESS SCORE ACHIEVED ON ISLAND: ~5,4f %~%"
          (* 100 (creature-fit (island-best island))))
  (format t "[*] AVERAGE FITNESS ON ISLAND: ~5,2f %~%"
          (* 100 (/ (reduce #'+
                            (remove-if #'null (mapcar #'creature-fit (island-deme island))))
                    (length (island-deme island)))))
  (format t "[*] BEST FITNESS BY GENERATION:  ~%")
  (print-fitness-by-gen (island-logger island))
  (format t "[*] AVERAGE SIMILARITY TO BEST:  ~5,2f %~%"
          (* 100 (likeness-to-specimen (island-deme island) (island-best island))))
  (format t "[*] STRUCTURAL INTRON FREQUENCY: ~5,2f %~%"
          (* 100 (- 1 (average-effective (island-deme island)))))

  (format t "[*] AVERAGE LENGTH: ~5,2f instructions (~5,2f effective)~%"
          (/ (reduce #'+ (mapcar #'(lambda (x) (length (creature-seq x)))
                                 (island-deme island)))
             (length (island-deme island)))
          (/ (reduce #'+ (mapcar #'(lambda (x) (length (creature-eff x)))
                                 (island-deme island)))
             (length (remove-if #'(lambda (x) (equalp #() (creature-eff x)))
                                (island-deme island)))))
  (format t "[*] EFFECTIVE OPCODE CENSUS:~%")
  (opcode-census (island-deme island)))
                  

(defun plot-fitness (island)
  (hrule)
  (if (< 0 (island-id island))
      (format t "           PLOT OF BEST FITNESS OVER GENERATIONS FOR ISLAND ~@R~%"
              (island-id island))
      (format t "           PLOT OF BEST FITNESS OVER GENERATIONS FOR ISLAND ZERO~%"))
             
  (hrule)
  (let* ((fitlog (funcall (island-logger island)))
         (lastgen (caar fitlog))
         (row #\newline)
         (divisor 35)
         (interval (max 1 (floor (div lastgen divisor))))
         (scale 128)
         (bar-char #\X))
;;         (end-char #\x))
    (dotimes (i (+ lastgen interval))
      (when (assoc i fitlog)
        (setf row (format nil "~v@{~c~:*~}"
                          (ceiling (* (- (cdr (assoc i fitlog)) .5)
                                      scale)) bar-char)))
      (when (= (mod i interval) 0)
        (format t "~5d || ~a~%" i row)))
    (hrule)
    (format t "        ")
    (let ((x-axis 50))
      (dotimes (i (floor (div scale 2)))
        (if (= (pmd i (floor (div scale 11))) 0)
            (progn 
              (format t "~d" x-axis)
              (when (>= x-axis 100) (return))
              (incf x-axis 10))
            (format t " "))))
    (format t "~%")
    (hrule)))

(defun print-creature (crt)
  (flet ((nil0 (n)
           (if (null n) 0 n)))
    (format t "FIT: ~F~%SEQ: ~A~%EFF: ~A~%HOME: ISLAND #~D~%GEN: ~D~%"
            (nil0 (creature-fit crt))
            (creature-seq crt)
            (creature-eff crt)
            (nil0 (creature-home crt))
            (nil0 (creature-gen crt)))
    (when (creature-parents crt)
      (format t "FITNESS OF PARENTS: ~F, ~F~%"
              (creature-fit (car (creature-parents crt)))
              (creature-fit (cadr (creature-parents crt))))
      (format t "HOMES OF PARENTS: ISLAND ~D, ISLAND ~D~%"
              (creature-home (car (creature-parents crt)))
              (creature-home (car (creature-parents crt))))))
  
  (hrule)
  (format t "DISASSEMBLY OF EFFECTIVE CODE:~%")
  (disassemble-sequence (creature-eff crt) :static t))
  

(defun genealogical-fitness-stats ()
  (let ((sum 0))
    (mapcar #'(lambda (x) (if (numberp x) (incf sum x))) *records*)
    (loop for (entry stat) on *records* by #'cddr do
         (format t "~A: ~5,2F%~%"
                 (substitute #\space #\- (symbol-name entry))
                 (* 100 (/ stat sum))))))

(defun print-new-best-update (island)
;;  (hrule)
  (format t "****************** NEW BEST ON ISLAND #~d AT GENERATION ~d ******************~%"
          (island-id island) (island-age island))
  (print-creature (island-best island)))
  ;;(format t "*****************************************************************************~%"))
          
(defun classification-report (crt dataset &key (testing t) (ht .testing-hashtable.))
  (if testing
      (setf ht .testing-hashtable.)
      (setf ht .training-hashtable.))
  (case dataset
    (tictactoe (ttt-classification-report :crt crt :ht ht :out .out-reg.))
    (iris (iris-classification-report :crt crt :ht ht :out .out-reg.))
    (otherwise (error "Unknown dataset."))))


