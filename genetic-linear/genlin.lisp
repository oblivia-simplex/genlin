;;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;;; Linear Genetic Algorithm
;;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;;; TODO:
;;; - split datasets into testing and training hashtables
;;; - write function to plot genetic programme as directed graph (dot)
;;; - organize code into files and annotate w comments


;; Weirdly, the evolution seems to run fine, but is severely retarded,
;; if auxiliary.lisp isn't loaded *explicitly* before C-L loading this
;; file. Not sure of details, but I'm screwing up somewhere w src files. 

(defpackage :genetic.linear
  (:use :common-lisp))

(in-package :genetic.linear)

(defparameter *project-path*
  "/home/oblivia/Projects/genetic-exercises/genetic-linear/")

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



(defparameter *VERBOSE* nil)

(defparameter *ht* (make-hash-table :test 'equalp))

(defparameter *training-ht* (make-hash-table :test 'equalp))

(defparameter *testing-ht* (make-hash-table :test 'equalp))

(defparameter *TASK* 'binary)

;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; Genetic Parameters
;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
(defstruct creature fit seq eff idx)

(defparameter *best* (make-creature :fit 0))

(defparameter *population* '())

(defparameter *population-size* 500)

(defparameter *specimens* '())

(defparameter *mutation-rate* 15)

(defparameter *min-len* 2) ;; we want to prevent seqs shrinking to nil

(defparameter *max-len* 256) ;; max instruction length

(defparameter *max-start-len* 25) ;; max initial instruction length

;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; Logging
;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

(defparameter =logger=
  ;; Just an excuse to play with the let-over-lambda design pattern...
  (let ((log '()))
    (lambda (&optional (entry nil))
      (cond ((eq entry 'CLEAR) (setf log '()))
            (entry (push entry log))
            (t log)))))

(defun log-best-fit (iteration)
  (funcall =logger= (cons iteration (creature-fit *best*))))



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

;;(let ((history '()))
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
;;;        (print regs) ;;;;;;;;;;;;;;;;;;;;;;;;;;DEBUGGING
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
;; Intron Removal and Statistics
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
             (push inst efseq))))
         ;;    (when (and (not (zerop i)) (jmp? (aref seq (1- i))))
         ;;      (let ((prevjmp (aref seq (1- i))))
         ;;        (push (src? prevjmp) efr)
         ;;        (push (dst? prevjmp) efr))))))
    (coerce efseq 'vector)))
  ;; (let ((efr output)
  ;;       (efseq '()))
  ;;   (loop for i from (1- (length seq)) downto 0 do
  ;;        (let ((inst (aref seq i)))
  ;;          (when (member (dst? inst) efr)
  ;;            (push (src? inst) efr)
  ;;            (push inst efseq))))
  ;;   (coerce efseq 'vector)))


  
(defun percent-effective (crt &key (out *output-reg*))
  (when (equalp #() (creature-eff crt))
    (setf (creature-eff crt) (remove-introns (creature-seq crt)
                                             :output out)))
  (float (/ (length (creature-eff crt)) (length (creature-seq crt)))))

(defun average-effective (population)
  (/ (reduce #'+ (mapcar #'percent-effective population))
     (length population)))

;; ......................................................................


;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; Functions related to fitness measurement
;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

;; Convention for fitness functions: let 1 be maximum value. no lower bound -- may
;; go to arbitrarily small fractions < 1

;; (defparameter =fitness-parameters=
;;   (let* ((.fitfunc. nil)
;;          (.training-hashtable. nil)
;;          (.testing-hashtable. nil)
;;          (.out-reg. nil))
;;     (lambda (&key (get nil))
;;       (case get
;;         (('fitfunc) .fitfunc.)
;;         (('training-hashtable) .training-hashtable.)
;;         (('testing-hashtable) .testing-hashtable.)
;;         (('out-reg) .out-reg.)
;;         (otherwise `(,.fitfunc.
;;                      ,.training-hashtable.
;;                      ,.testing-hashtable.
;;                      ,.out-reg.))))))
 

;; these parameters could be wrapped in a let, encapsulating the fitness
;; related functions that follow in a "regional" environment.

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
(defun fitness-binary-classifier-1 (crt)
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
              being the hash-keys in .training-hashtable. collect
                (sigmoid-error
                 (car (execute-sequence seq
                               ;;         :registers *initial-register-state*
                                        :input pattern
                                        :output '(0) )) ;; raw
                 (gethash pattern .training-hashtable.))))) ;; goal
      (and *debug* *verbose*
           (format t "SEQUENCE:~a~%RESULTS:~%~a~%" seq results))
      (/ (reduce #'+ results) (length results)))))

(defun fitness-binary-classifier-2 (crt)
  (if (null .out-reg.) (setf .out-reg. '(0)))
  (unless (> 0 (length (creature-eff crt)))
    (setf (creature-eff crt) ;; assuming eff is not set, if fit is not.
          (remove-introns (creature-seq crt) :output .out-reg.)))
  (let ((hit 0)
        (miss 0)
        (seq (creature-eff crt)))
    (loop for pattern being the hash-keys in .training-hashtable.
       using (hash-value v) do
         (let ((f ;; shooting in the dark, here. 
                (tanh (/ (car (execute-sequence seq :input pattern
                                                :output .out-reg.)) 300))))
           (if (> (* f v) .5) (incf hit) (incf miss))))
    (if (zerop miss) 1
        (/ hit (+ hit miss)))))


  
(defun fitness-binary-classifier-3 (crt)
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
    (let ((acc1)
          (acc2)
          (acc3)
          (weight 1)
          (seq (creature-eff crt)))
      (setf acc1 0 acc2 0 acc3 0)
      
      (loop for pattern being the hash-keys in .training-hashtable.
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

      (setf acc1 (float (/ acc1 (hash-table-count .training-hashtable.))))
      (setf acc2 (float (/ acc2 (hash-table-count .training-hashtable.))))
      (setf acc3 (/ (+ acc1 (* weight acc2)) 2))
;;c      (format t "acc1: ~5,4f     acc2: ~5,4f  ==> avg = ~4,2f~%" acc1 acc2 acc3)
      acc3)))


;; In Linear Genetic Programming, the author suggests using the
;; sum of two fitness measures as the fitness: 

(defun fitness-ternary-classifier-1 (crt)
  "Where n is the target register, measures fitness as the ratio of
Rn to the sum of all output registers R0-R2 (wrt absolute value)."
  (if (null .out-reg.) (setf .out-reg. '(0 1 2)))
  (unless (> 0 (length (creature-eff crt)))
    (setf (creature-eff crt) ;; assuming eff is not set, if fit is not.
          (remove-introns (creature-seq crt) :output '(0 1 2))))
  (let ((acc 0)
        (seq (creature-eff crt)))
    (loop for pattern being the hash-keys in .training-hashtable.
       using (hash-value i) do
         (let ((output (execute-sequence seq
                                         :input pattern
                                         :output .out-reg.)))
           (incf acc (DIV (abs (nth i output))
                          (reduce #'+ (mapcar #'abs output))))))
    (/ acc (hash-table-count .training-hashtable.))))

(defun fitness (crt)
  "Measures the fitness of a specimen, according to a specified
fitness function."
  (unless (creature-fit crt)  
    (setf (creature-fit crt)
          (funcall .fitfunc. crt))
    (when (or (null (creature-fit *best*))
              (> (creature-fit crt) (creature-fit *best*)))
      (setf *best* (copy-structure crt))
      (and *debug* (format t "FITNESS: ~f~%BEST:    ~f~%"
                           (creature-fit crt) (creature-fit *best*)))))
  (creature-fit crt))



(defun classification-report (crt dataset &key (testing t) (ht .testing-hashtable.))
  (if testing
      (setf ht .testing-hashtable.)
      (setf ht .training-hashtable.))
  (case dataset
    (tictactoe (ttt-classification-report :crt crt :ht ht :out .out-reg.))
    (iris (iris-classification-report :crt crt :ht ht :out .out-reg.))
    (otherwise (error "Unknown dataset.")))) 

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
         (maxidx (max idx0 idx1)))
    ;;    (declare (type cons mother father daughter son))
    ;;    (declare (type fixnum idx0 idx1 minidx maxidx offset r-align))
                                        ;(format t "minidx: ~d  maxidx: ~d  offset: ~d~%" minidx maxidx offset)
    (setf (subseq daughter (+ offset minidx) (+ offset maxidx))
          (subseq father minidx maxidx))
    (setf (subseq son minidx maxidx)
          (subseq mother (+ offset minidx) (+ offset maxidx)))         
    ;;    (format t "mother: ~a~%father: ~a~%daughter: ~a~%son: ~a~%"
    ;;            mother father daughter son)
    (list (make-creature :seq (maybe-mutate son))
          (make-creature :seq (maybe-mutate daughter)))))


;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; Selection functions
;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=



(defun tournement! (population)
  (let* ((lots (n-rnd 0 (length *population*)))
         (combatants (mapcar #'(lambda (i) (nth i population)) lots)))
    (and *debug* (format t "COMBATANTS BEFORE: ~a~%" combatants))
    (loop for combatant in combatants do
         (unless (creature-fit combatant)
           (fitness combatant)))
    (and *debug* (format t "COMBATANTS AFTER: ~a~%" combatants))
    (let* ((ranked (sort combatants
                         #'(lambda (x y) (< (creature-fit x) (creature-fit y)))))
           (parents (cddr ranked))
           (children (apply #'crossover parents))
           (the-dead (subseq ranked 0 2)))
      (map 'list #'(lambda (i j) (setf (creature-idx i) (creature-idx j)))
           children the-dead)
      (mapcar #'(lambda (x) (setf (nth (creature-idx x) population) x))
              children) 
      (and *debug* (format t "RANKED: ~a~%" ranked))
      *best*)))

(defun spin-wheel (wheel top)
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

(defun f-roulette (population)
  (let* ((tally 0)
         (popsize (length population))
         
         (wheel (loop for creature in population
                   collect (progn
                             (let ((f (float (fitness creature))))
                               (incf tally f)
                               (cons tally creature)))))
         ;; the roulette wheel is now built
         (breeders (loop for i from 1 to popsize
                      collect (spin-wheel wheel tally)))
         (half (/ popsize 2))
         (mothers (subseq breeders 0 half))
         (fathers (subseq breeders half popsize)))
    (apply #'concatenate 'list (map 'list #'crossover mothers fathers))))

(defun greedy-roulette! (population)
  "New and old generations compete for survival. Takes twice as long
as #'roulette!"
  (let ((popsize (length population)))
    (setf (subseq population 0 popsize)
          (subseq (sort (concatenate 'list population
                                     (f-roulette population))
                        #'(lambda (x y) (> (fitness x)
                                      (fitness y))))
                  0 popsize))))

(defun roulette! (population)
  (setf (subseq population 0 (length population))
        (f-roulette population)))


(defun spawn-sequence (len)
  (concatenate 'vector (loop repeat len collect (random *max-inst*))))

(defun spawn-creature (len &key idx)
  (make-creature :seq (spawn-sequence len) :idx idx))

(defun init-population (popsize slen)
  (loop for i from 0 to (1- popsize) collect
       (spawn-creature (+ *min-len* (random slen)) :idx i)))


;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; Initialization functions
;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

;; --- Prepare the population ---


;; -- Prepare the data (general) ---

;; (defun obsolete-partition-data (hashtable ratio)
;;   (flet ((shuffle (l)
;;            (sort l #'(lambda (x y) (= 0 (random 2))))))
;;     (let* ((size (hash-table-count hashtable))
;;            (training-size (floor (* ratio size)))
;;            (keys (loop for k being the hash-keys in hashtable collect k))
;;            (shuffled (shuffle keys)))
;;       (setf *training-ht* (make-hash-table :test 'equalp))
;;       (setf *testing-ht* (make-hash-table :test 'equalp))
;;       (loop for i from 1 to size do
;;            (let ((k (pop shuffled))
;;                  (dst-ht (if (< i training-size) *training-ht* *testing-ht*)))
;;              (setf (gethash k dst-ht) (gethash k hashtable)))))))


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

;; --- Prepare the data (domain-specific) ---

(defun setup-tictactoe (&key (int t) (gray t) (ratio 4/5)
                          (fitfunc-name 'binary-1)
                          (file "/home/oblivia/Projects/genetic-exercises/genetic-linear/datasets/TicTacToe/tic-tac-toe-balanced.data"))
  (let* ((filename file)
         (hashtable (ttt-datafile->hashtable :filename filename
                                             :int int :gray gray))
         (tht-tht (partition-data hashtable ratio)))
    (format t "at line 614, hashtable = ~a~%" hashtable)
    (init-fitness-env :training-hashtable (car tht-tht)
                      :testing-hashtable  (cdr tht-tht)
                      :fitfunc-name fitfunc-name)
    (setf *best* (make-creature :fit 0))
    (setf *population* (init-population *population-size* *max-start-len*))
    (format t "~%")
    (hrule)
    (format t "[!] DATA READ AND PARTITIONED INTO TRAINING AND TESTING TABLES~%[!] POPULATION INITIALIZED, STORED IN *POPULATION*~%[!] FITNESS ENVIRONMENT INITIALIZED:~%")
    (hrule)
    (peek-fitness-environment)
    (hrule)
    hashtable))

(defun setup-iris (&key (ratio 4/5) (fitfunc #'fitness-ternary-classifier-1))
  (let* ((filename "/home/oblivia/Projects/genetic-exercises/genetic-linear/datasets/Iris/iris.data")
         (hashtable (iris-datafile->hashtable filename))
         (tht-tht (partition-data hashtable ratio)))
    (init-fitness-env :training-hashtable (car tht-tht)
                      :testing-hashtable  (cdr tht-tht)
                      :fitfunc fitfunc
                      :out-reg nil) ;; fitfunc will init this (bad hack)
    (setf *best* (make-creature :fit 0))
    (setf *population* (init-population *population-size* *max-start-len*))
    (print "population initialized in *population*; data read; hashtable in *ht*")
    (peek-fitness-environment)
    hashtable))

;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; User interface functions
;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=


;; need a better naming scheme for these UI & helper funcs!


(defun print-params ()
  "Prints major global parameters, and some statistics, too."
  (hrule)
  (format t "[+] INSTRUCTION SIZE:     ~d bits~%" *wordsize*)
  (format t "[+] # of RW REGISTERS:    ~d~%" (expt 2 *dstf*))
  (format t "[+] # of RO REGISTERS:    ~d~%" (expt 2 *srcf*))
  (format t "[+] PRIMITIVE OPERATIONS: ")
  (loop for i from 0 to (1- (expt 2 *opf*)) do
       (format t "~a " (func->string (aref *opcodes* i))))
  (format t "~%[+] POPULATION SIZE:      ~d of ~d~%"
          (length *population*) *population-size*)
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


(defun print-fitness-by-gen ()
  (flet ((.*. (x y)
           (if (numberp y)
               (* x y)
               NIL)))
  (let ((l (funcall =logger=)))
    (loop for (e1 e2) on l by #'cddr do
         (format t "~c~d:~c~5,4f %~c~c~d:~c~5,4f %~%" #\tab
                 (car e1) #\tab (.*. 100 (cdr e1)) #\tab #\tab
                 (car e2) #\tab (.*. 100 (cdr e2)))))))

(defun print-statistics ()
  (hrule)
  (format t "[*] BEST FITNESS SCORE ACHIEVED: ~5,4f %~%" (* 100 (creature-fit *best*)))
  (format t "[*] AVERAGE FITNESS: ~5,2f %~%"
          (* 100 (/ (reduce #'+
                     (remove-if #'null (mapcar #'creature-fit *population*)))
             (length *population*))))
  (format t "[*] BEST FITNESS BY GENERATION:  ~%")
  (print-fitness-by-gen)
  (format t "[*] AVERAGE SIMILARITY TO BEST:  ~5,2f %~%"
          (* 100 (likeness-to-specimen *population* *best*)))
  (format t "[*] STRUCTURAL INTRON FREQUENCY: ~5,2f %~%"
          (* 100 (- 1 (average-effective *population*))))

  (format t "[*] AVERAGE LENGTH: ~5,2f instructions (~5,2f effective)~%"
          (/ (reduce #'+ (mapcar #'(lambda (x) (length (creature-seq x)))
                                 *population*))
             (length *population*))
          (/ (reduce #'+ (mapcar #'(lambda (x) (length (creature-eff x)))
                                 *population*))
             (length (remove-if #'(lambda (x) (equalp #() (creature-eff x)))
                                *population*))))
  (format t "[*] EFFECTIVE OPCODE CENSUS:~%")
  (opcode-census *population*)
  (hrule))
                  

(defun plot-fitness ()
  (hrule)
  (format t "                    PLOT OF BEST FITNESS OVER GENERATIONS~%")
  (hrule)
  (let* ((fitlog (funcall =logger=))
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
        (format t "~5d | ~a~%" i row)))
    (hrule)
    (format t "        ")
    (let ((x-axis 50))
      (dotimes (i (floor (div scale 2)))
        (if (= (pmd i (floor (div scale 10.6))) 0)
            (progn 
              (format t "~d" x-axis)
              (when (>= x-axis 100) (return))
              (incf x-axis 10))
            (format t " "))))
    (format t "~%")
    (hrule)))

;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; Runners
;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

(defun run-breeder (&key (method #'tournement!) (dataset 'unknown)
                      (rounds 10000) (target 0.97) (stat-interval 500))
                      ;; adjustments needed to add fitfunc param here. 
  (setf *STOP* nil)
  (let ((oldbest *best*))
    (funcall =logger= 'clear)
    (time (block evolver
            (dotimes (i rounds)
              (funcall method *population*)
              (when (= 0 (mod i stat-interval))
                (hrule)
                (format t "[ITERATION ~d]~%" i)
                (print-statistics))
              (when (or (null (creature-fit *best*))
                        (> (creature-fit *best*) (creature-fit oldbest)))
                (setf oldbest *best*)
                (log-best-fit i)
                (format t "DATASET: ~s; METHOD: ~s~%" dataset method)
                (format t "NEW BEST AT ROUND ~d: ~a~%" i *best*)
                (disassemble-sequence (creature-eff *best*)
                                      :static t ))
              (when (or (> (creature-fit *best*) target) *STOP*)
                (format t "~%TARGET OF ~f REACHED AFTER ~d ROUNDS~%" target i)
                (return-from evolver)))))
    (push *best* *specimens*)
    (format t "BEST: ~f~%" (creature-fit *best*))))


(defun evolve (&key (method 'tournement!) (rounds 50000) (target 0.97)
                 (dataset 'tictactoe) (ratio 8/10) (fitfunc-name 'binary-3))
  "Do everything: initialize a population and hashtable, then run the
breeder function, with specified method (roulette or tournmenent,
e.g.) for a specified number of rounds or until a given fitness target
is reached, whichever comes first."
  (cond ((eq dataset 'tictactoe) (setup-tictactoe
                                  :int t :gray t
                                  :fitfunc-name fitfunc-name
                                  :ratio ratio))
        ((eq dataset 'iris) (setup-iris
                             :ratio ratio))
        (t (error "DATASET UNKNOWN")))
  (hrule)
  ;; set default fitness functions
  (format t "INITIAL POPULATION~%")
  (hrule)
 ;; (print *population*)
  (format t "~%")
  (hrule)
  (print-params)
  (print-statistics)
  (partition-data *ht* ratio)
  (run-breeder :dataset dataset :method method
               :rounds rounds :target target)
  (format t "~%FINAL POPULATION~%")
  ;;(print *population*)
  (format t "~%")
  (hrule)
  (format t "~%TRAINING COMPLETE. TESTING BEST SPECIMEN.~%" )
  (hrule)
  (classification-report *best* dataset)
  (print-params)
  (print-statistics)
  (plot-fitness))

(defun omnibus (&key (mutation-rate *mutation-rate*)
                  (population-size *population-size*)
                  (tournement-rounds 10000)
                  (roulette-rounds 500))
  (setf *population-size* population-size)
  (setf *mutation-rate* mutation-rate)
  (let* ((t-rounds tournement-rounds)
         (r-rounds roulette-rounds)
         (targ 1)
         (params `((:method tournement! :dataset tictactoe
                            :rounds ,t-rounds :target ,targ)
                   (:method roulette! :dataset tictactoe
                            :rounds ,r-rounds :target ,targ)
                   (:method greedy-roulette! :dataset tictactoe
                            :rounds ,r-rounds :target ,targ)
                   (:method tournement! :dataset iris
                            :rounds ,t-rounds :target ,targ)
                   (:method roulette! :dataset iris
                            :rounds ,r-rounds :target ,targ)
                   (:method greedy-roulette! :dataset iris
                            :rounds ,r-rounds :target ,targ))))
    (hrule)
    (loop for param-list in params do
         (format t "METHOD: ~S; DATASET: ~S~%"
                 (getf param-list :method)
                 (getf param-list :dataset))              
         (hrule)
         (apply #'evolve param-list))))



;; TODO:
;; replace the rest of the frequently-passed RO variables with
;; something more like "static" class variables, in let-over-defuns.
;; resist the temptation to do this with mutable variables, like
;; population, until it's clear that this won't interfere with hyperthreading.
(setf *STOP* nil)
(print-params)



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




(print-params)
