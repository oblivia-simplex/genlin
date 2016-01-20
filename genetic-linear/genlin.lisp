;;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;;; Linear Genetic Algorithm
;;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;;; TODO:
;;; - split datasets into testing and training hashtables
;;; - write function to plot genetic programme as directed graph (dot)
;;; - organize code into files and annotate w comments


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

(defparameter *DEBUG* nil)

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

(defparameter *opf* 3)   ;; size of opcode field, in bits

(defparameter *srcf* 3)  ;; size of source register field, in bits

(defparameter *dstf* 2)  ;; size of destination register field, in bits

;; --- Do not adjust the following five parameters manually ---

(defparameter *wordsize* (+ *opf* *srcf* *dstf*))

(defparameter *max-inst* (expt 2 *wordsize*)) ;; upper bound on inst size

(defparameter *opbits* (byte *opf* 0))

(defparameter *srcbits* (byte *srcf* *opf*))

(defparameter *dstbits* (byte *dstf* (+ *srcf* *opf*)))

;; --- Operations: these can be tweaked independently of the 
;; --- fields above, so long as their are at least (expt 2 *opf*)
;; --- elements in the *opcodes* vector. 

(declaim (inline DIV MUL XOR CNJ DIS PMD ADD SUB MUL JLE)) 

(defun DIV (&rest args)
  "A divide-by-zero-proof division operator."
  (if (some #'zerop args) 0
      (/ (car args) (cadr args))))

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
      (handler-case (mod (car args) (cadr args))
        (error () 0))))

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
           #'XOR #'PMD #'CNJ #'JLE)) ;; extended operations (3bit opcode)

;; adding the extended opcodes seems to result in an immense boost in the
;; population's fitness -- 0.905 is now achieved in the time it took to
;; reach 0.64 with the basic operation set. (For tic-tac-toe.)


;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; Parameters for register configuration.
;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

(defparameter *output-reg* '())

(defparameter *maxval* (expt 2 16)) ;; max val that can be stored in reg

(defparameter *default-input-reg*
  (concatenate 'vector (loop for i from 1 to (- (expt 2 *srcf*) (expt 2 *dstf*))
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

            ;;   (sieve-of-eratosthenes 18))) ;; some primes for fun

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

(defun func->string (func)
  (let* ((fm (format nil "~a" func))
         (i (mismatch fm "#<FUNCTION __"))
         (o (subseq fm i (1- (length fm)))))
    #+sbcl o
    #+clisp (subseq o 0 (position #\space o))))

(defun inst->string (inst &key (registers *initial-register-state*)
                            (static nil))
  (concatenate 'string
               (format nil "[~a  R~d, R~d]"
                       (func->string (op? inst))
                       (src? inst) (dst? inst))
               (unless static (format nil " ;; (~f, ~f)"
                                      (aref registers (src? inst))
                                      (aref registers (dst? inst))))))

(defun hrule ()
  (format t "-----------------------------------------------------------------------------~%"))

(defun disassemble-sequence (seq &key (registers *initial-register-state*)
                                   (input *default-input-reg*)
                                   (static nil))
  (let ((od *debug*)
        (regs (copy-seq registers)))
    (enter-input regs input)
    (unless static
      (hrule)
      (print-registers regs))
    (hrule)
    (setf *debug* 1)
    (execute-sequence seq :input input :registers regs :static static)
    (setf *debug* od)
    (unless static 
      (print-registers regs)
      (hrule))))
    
(defun dbg (&optional on-off)
  (case on-off
    ((on)  (setf *debug* t))
    ((off) (setf *debug* nil))
    (otherwise (setf *debug* (not *debug*)))))

;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; Intron Removal and Statistics
;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

(defun remove-introns (seq &key (out '(0)))
  "Filters out non-effective instructions from a sequence -- instructions
that have no bearing on the final value(s) contained in the output 
register(s)."
  (let ((efr out)
        (efseq '()))
    (loop for i from (1- (length seq)) downto 0 do
         (let ((inst (aref seq i)))
           (when (member (dst? inst) efr)
             (push (src? inst) efr)
             (push inst efseq)
             (when (and (not (zerop i)) (jmp? (aref seq (1- i))))
               (let ((prevjmp (aref seq (1- i))))
                 (push (src? prevjmp) efr)
                 (push (dst? prevjmp) efr))))))
    (coerce efseq 'vector)))

(defun percent-effective (crt &key (out *output-reg*))
  (unless (creature-eff crt)
    (setf (creature-eff crt) (remove-introns (creature-seq crt) :out out)))
  (float (/ (length (creature-eff crt)) (length (creature-seq crt)))))

(defun average-effective (population)
  (/ (reduce #'+ (mapcar #'percent-effective population))
     (length population)))

;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; Execution procedure
;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=


;; MESSY, and TOO SLOW. Try to delegate debugging output elsewhere. 
(defun execute-sequence (seq &key (registers *initial-register-state* )
                               (input *default-input-reg*)
                               (static nil))
  "Takes a sequence of instructions, seq, and an initial register 
state vector, registers, and then runs the virtual machine, returning the
resulting value in R0."
  ;; (declare (optimize (speed 1)))
;  (declare (type (simple-array fixnum (*)) ;registers
;                 input *default-input-reg* *initial-register-state*))
  (declare (type fixnum *input-start-idx*))
  (declare (inline src? dst? op?))
  
  (let ((regs (copy-seq registers))
        (seqlen (length seq)))
    ;; the input values will be stored in read-only regs
    (setf (subseq regs *input-start-idx*
                  (+ *input-start-idx* (length input))) input)
    ;;      (format t "input: ~a~%regs: ~a~%" inp regs)
    (unless (zerop seqlen)
      (loop do
           (let* ((inst (aref seq (aref regs *pc-idx*)))
                (D (if (jmp? inst) *pc-idx* (dst? inst))))
             (and *debug* (format t "~8,'0b  ~a~c" inst
                                  (inst->string inst :registers regs
                                                :static static)
                                  (if static #\newline #\space)))
             (incf (aref regs *pc-idx*))
             (unless static
               (setf (aref regs D)
                     (rem (apply (op? inst)
                                 (list (aref regs (src? inst))
                                       (aref regs (dst? inst))
                                       (aref regs *pc-idx*))) *maxval*))
               (and *debug* (format t ";; now R~d = ~f; PC = ~d~%"
                                  (dst? inst) (aref regs (dst? inst))
                                  (aref regs *pc-idx*))))
             (and (>= (aref regs *pc-idx*) seqlen) (return)))))
    (and *debug* (hrule))
    (mapcar #'(lambda (i) (aref regs i)) *output-reg*)))

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
;; we still need to make this modification to roulette, to accommodate the
;; fitness-storing cons cell in the car of each individual 

;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; Functions related to fitness measurement
;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

;; Convention for fitness functions: let 1 be maximum value. no lower bound -- may
;; go to arbitrarily small fractions < 1

(let ((n 1))
  
  
  (declaim (inline binary-error-measure))
  
  (defun binary-error-measure (raw goal)
    ;; goal is a boolean value (t or nil)
    ;; raw is, typically, the return value in r0
  (let ((div (/ *maxval* 10000))) ;;  (let ((div (/ *maxval* 10000)))
    (flet ((sigmoid (x)
             (tanh (/ x div))))
      ;; if raw is < 0, then (sigmoid raw) will be between
      ;; 0 and -1. If the goal is -1, then the final result
      ;; will be (-1 + p)/2 -- a value between -0.5 and -1,
      ;; but taken absolutely as a val between 0.5 and 1.
      ;; likewise when raw is > 0. 
      (/ (abs (+ (sigmoid raw) goal)) 2))))

  (defun fitness-binary-classifier-1 (seq hashtable)
    ;; positive = t; negative = nil
    ;; (declare (type hash-table hashtable))
    ;; (declare (type cons seq))
    ;; (declare (optimize speed))
    (let ((results (loop for pattern being the hash-keys in hashtable collect
                        (binary-error-measure
                         (car (execute-sequence seq :input pattern))
                         (gethash pattern hashtable)))))
      ;; (declare (type (cons rational) results))
      (and *debug* *verbose*
           (format t "SEQUENCE:~a~%RESULTS:~%~a~%" seq results))
      (/ (apply #'+ results) (length results)))) ;; average

  (defun fitness-binary-classifier-2 (seq hashtable)
    ;; (declare (type hash-table hashtable))
    ;; (declare (type (cons rational) seq))
    ;; (declare (optimize speed))
    (let ((correct 0)
          (incorrect 0))
      ;; (declare (type integer correct incorrect))
      (loop for pattern being the hash-keys in hashtable using (hash-value v)
         do
           (let ((f (car (execute-sequence seq :input pattern))))
             ;; (declare (type rational f v))
             (if (> (* f v) 0) (incf correct) (incf incorrect))))
      ;;    (format t "SEQ: ~a~%CORRECT: ~d    INCORRECT ~d~%~%" seq correct incorrect)
      (if (zerop incorrect) 1
          (/ correct (+ correct incorrect)))))

  (defun fitness-ternary-classifier-1 (seq hashtable)
    "Where n is the target register, measures fitness as the ratio of Rn to
the sum of all output registers R0-R2 (wrt absolute value)."
    (let ((acc 0))
      (loop for pattern being the hash-keys in hashtable using (hash-value i)
         do
           (let ((output (execute-sequence seq :input pattern)))
             (incf acc (DIV (abs (nth i output))
                            (reduce #'+ (mapcar #'abs output))))))
      (/ acc (hash-table-count hashtable))))


  (defun fitness (&key (crt nil) (lookup *ht*) (out *output-reg*)
                    (fitfunc #'fitness-binary-classifier-1)) 
    "Measures the fitness of a specimen, according to a specified fitness
function."
    (unless (creature-fit crt)  
      (setf (creature-eff crt) ;; assuming eff is not set, if fit is not.
            (remove-introns (creature-seq crt) :out out))
      (setf (creature-fit crt)
            (funcall fitfunc (creature-eff crt) lookup))
      (when (or (null (creature-fit *best*))
                (> (creature-fit crt) (creature-fit *best*)))
        (setf *best* (copy-structure crt))
        (and *debug* (format t "FITNESS: ~f~%BEST:    ~f~%"
                             (creature-fit crt) (creature-fit *best*)))))
    (creature-fit crt))
)
;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; Selection functions
;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

(let ((%population% '()))

  (defun tournement! (population &key (lookup nil)
                                   (fitfunc #'fitness-binary-classifier-1))
    (let* ((lots (n-rnd 0 (length *population*)))
           (combatants (mapcar #'(lambda (i) (nth i population)) lots)))
      (and *debug* (format t "COMBATANTS BEFORE: ~a~%" combatants))
      (loop for combatant in combatants do
           (unless (creature-fit combatant)
             (setf (creature-fit combatant)
                   (fitness :crt combatant :lookup lookup
                            :fitfunc fitfunc))))
      (and *debug* (format t "COMBATANTS AFTER: ~a~%" combatants))
      (let* ((ranked (sort combatants #'(lambda (x y) (< (creature-fit x) (creature-fit y)))))
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
    (let ((ball (random (float top)))
          (ptr (car wheel)))
      (loop named spinning for slot in wheel do
           (when (< ball (car slot))
             (return-from spinning))
           (setf ptr slot))
      (cdr ptr)))
  
  (defun f-roulette (population &key (lookup *ht*)
                                  (fitfunc #'fitness-binary-classifier-1))
    (let* ((tally 0)
           (popsize (length population))
           
           (wheel (loop for creature in population
                     collect (progn
                               (let ((f (float (fitness :crt creature
                                                        :fitfunc fitfunc
                                                        :lookup lookup))))
                                 (incf tally f)
                                 (cons tally creature)))))
           ;; the roulette wheel is now built
           (breeders (loop for i from 1 to popsize
                        collect (spin-wheel wheel tally)))
           (half (/ popsize 2))
           (mothers (subseq breeders 0 half))
           (fathers (subseq breeders half popsize)))
      (apply #'concatenate 'list (map 'list #'crossover mothers fathers))))

  (defun greedy-roulette! (population &key (lookup *ht*)
                                        (fitfunc #'fitness-binary-classifier-1))
    "New and old generations compete for survival. Takes twice as long
as #'roulette!"
    (let ((popsize (length population)))
      (setf (subseq population 0 popsize)
            (subseq (sort (concatenate 'list population
                                       (f-roulette population :lookup lookup
                                                   :fitfunc fitfunc))
                          #'(lambda (x y) (> (fitness :crt x :lookup lookup
                                                 :fitfunc fitfunc)
                                        (fitness :crt y :lookup lookup
                                                 :fitfunc fitfunc))))
                    0 popsize))))

  (defun roulette! (population &key (lookup *ht*)
                                 (fitfunc #'fitness-binary-classifier-1))
    (setf (subseq population 0 (length population))
          (f-roulette population :lookup lookup :fitfunc fitfunc)))
)
;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; Initialization functions
;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

;; --- Prepare the population ---

(defun spawn-sequence (len)
  (concatenate 'vector (loop repeat len collect (random *max-inst*))))

(defun spawn-creature (len &key idx)
  (make-creature :seq (spawn-sequence len) :idx idx))

(defun init-population (popsize slen)
  (loop for i from 0 to (1- popsize) collect
       (spawn-creature (+ *min-len* (random slen)) :idx i)))

;; -- Prepare the data (general) ---

(defun partition-data (hashtable ratio)
  (flet ((shuffle (l)
           (sort l #'(lambda (x y) (= 0 (random 2))))))
    (let* ((size (hash-table-count hashtable))
           (training-size (floor (* ratio size)))
           (keys (loop for k being the hash-keys in hashtable collect k))
           (shuffled (shuffle keys)))
      (setf *training-ht* (make-hash-table :test 'equalp))
      (setf *testing-ht* (make-hash-table :test 'equalp))
       (loop for i from 1 to size do
           (let ((k (pop shuffled))
                 (dst-ht (if (< i training-size) *training-ht* *testing-ht*)))
             (setf (gethash k dst-ht) (gethash k hashtable)))))))

;; --- Prepare the data (domain-specific) ---

(defun setup-tictactoe (&key (int t) (gray t))
  (let* ((filename "/home/oblivia/Projects/genetic-exercises/genetic-linear/datasets/TicTacToe/tic-tac-toe-balanced.data")
         (hashtable (ttt-datafile->hashtable filename :int int :gray gray)))
    (setf *output-reg* '(0))
    (setf *best* (make-creature :fit 0))
    (setf *population* (init-population 500 *max-start-len*))
    (print "population initialized in *population*; data read; hashtable in *ht*")
    (setf *TASK* 'binary)
    (setf *ht* hashtable)
    hashtable))

(defun setup-iris ()
  (let* ((filename "/home/oblivia/Projects/genetic-exercises/genetic-linear/datasets/Iris/iris.data")
         (hashtable (iris-datafile->hashtable filename)))
    (setf *output-reg* '(0 1 2))
    (setf *best* (make-creature :fit 0))
    (setf *population* (init-population 500 *max-start-len*))
    (print "population initialized in *population*; data read; hashtable in *ht*")
    (setf *TASK* 'ternary)
    (setf *ht* hashtable)
    hashtable))

;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; User interface functions
;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=


;; need a better naming scheme for these UI & helper funcs!


(defun print-params ()
  "Prints major global parameters, and some statistics, too."
  (hrule)
  (format t "[+] INSTRUCTION SIZE:     ~d bits~%" *wordsize*)
  (format t "[+] # of RW REGISTERS:   ~d~%" (expt 2 *dstf*))
  (format t "[+] # of RO REGISTERS:    ~d~%" (expt 2 *srcf*))
  (format t "[+] PRIMITIVE OPERATIONS: ")
  (loop for i from 0 to (1- (expt 2 *opf*)) do
       (format t "~a " (func->string (aref *opcodes* i))))
  (format t "~%[+] POPULATION SIZE:      ~d~%" (length *population*))
  (format t "[+] MUTATION RATE:        ~d%~%" *mutation-rate*)
  (format t "[+] MAX SEQUENCE LENGTH:  ~d~%" *max-len*)
  (format t "[+] MAX STARTING LENGTH:  ~d~%" *max-start-len*)
  (format t "[+] # of TRAINING CASES:  ~d~%"
          (hash-table-count *training-ht*))
  (format t "[+] # of TEST CASES:      ~d~%" (hash-table-count *testing-ht*))
  (format t "[+] FITNESS FUNCTION:     ~s~%" *task*)
  (hrule))

(defun print-statistics ()
  (hrule)
  (format t "[*] STRUCTURAL INTRON FREQUENCY: ~d%~%"
          (* 100 (- 1 (average-effective *population*))))
  (format t "[*] BEST FITNESS SCORE ACHIEVED: ~d%~%" (* 100 (creature-fit *best*)))
  (format t "[*] BEST FITNESS BY GENERATION:  ~a~%" (funcall =logger=)))
  

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
         (bar-char #\X)
         (end-char #\x))
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

(defun run-breeder (&key (method 'tournement!) (dataset 'unknown)
                      (rounds 10000) (target 0.97) (ht *ht*)
                      (fitfunc #'fitness-binary-classifier-1))
  (let ((oldbest *best*))
    (funcall =logger= 'clear)
    (time (block evolver
            (dotimes (i rounds)
              (funcall (symbol-function method) *population*
                       :lookup ht :fitfunc fitfunc)
              (when (or (null (creature-fit *best*))
                        (> (creature-fit *best*) (creature-fit oldbest)))
                (setf oldbest *best*)
                (log-best-fit i)
                (format t "DATASET: ~s; METHOD: ~s~%" dataset method)
                (format t "NEW BEST AT ROUND ~d: ~a~%" i *best*)
                (disassemble-sequence (creature-eff *best*)
                                      :static t ))
              (when (> (creature-fit *best*) target)
                (format t "~%TARGET OF ~f REACHED AFTER ~d ROUNDS~%" target i)
                (return-from evolver)))))
    (push *best* *specimens*)
    (format t "BEST: ~f~%" (creature-fit *best*))))


(defun evolve (&key (method 'tournement!) (rounds 50000) (target 0.97)
                 (dataset 'tictactoe) (ratio 8/10)
                 (fitfunc #'fitness-binary-classifier-1))
  "Do everything: initialize a population and hashtable, then run the breeder function, with specified method (roulette or tournmenent, e.g.) for a specified number of rounds or until a given fitness target is reached, whichever comes first." 
  (setf *ht* (make-hash-table :test 'equalp))
  (cond ((eq dataset 'tictactoe) (setup-tictactoe :int t :gray t))
        ((eq dataset 'iris) (setup-iris))
        (t (error "DATASET UNKNOWN")))
  (hrule)
  (format t "INITIAL POPULATION~%")
  (hrule)
  (print *population*)
  (format t "~%")
  (hrule)
  (print-params)
  (print-statistics)
  (partition-data *ht* ratio)
  (run-breeder :dataset dataset :method method :rounds rounds
               :target target :ht *training-ht* :fitfunc fitfunc)
  (format t "~%FINAL POPULATION~%")
  (print *population*)
  (format t "~%")
  (hrule)
  (format t "~%TRAINING COMPLETE. TESTING BEST SPECIMEN.~%" )
  (hrule)
  (case dataset
    ((tictactoe) (ttt-classification-report *best* *testing-ht*))
    ((iris) (iris-classification-report *best* *testing-ht*)))
  (print-params)
  (print-statistics)
  (plot-fitness))
  
(defun omnibus ()
  (let* ((t-rounds 10000)
         (r-rounds 500)
         (targ 1)
         (params `((:method tournement! :dataset tictactoe
                            :fitfunc fitness-binary-classifier-1
                            :rounds ,t-rounds :target ,targ)
                   (:method roulette! :dataset tictactoe
                            :fitfunc fitness-binary-classifier-1
                            :rounds ,r-rounds :target ,targ)
                   (:method greedy-roulette! :dataset tictactoe
                            :fitfunc fitness-binary-classifier-1
                            :rounds ,r-rounds :target ,targ)
                   (:method tournement! :dataset iris
                            :fitfunc fitness-ternary-classifier-1
                            :rounds ,t-rounds :target ,targ)
                   (:method roulette! :dataset iris
                            :fitfunc fitness-ternary-classifier-1
                            :rounds ,r-rounds :target ,targ)
                   (:method greedy-roulette! :dataset iris
                            :fitfunc fitness-ternary-classifier-1
                            :rounds ,r-rounds :target ,targ))))
    (hrule)
    (loop for param-list in params do
         (format t "METHOD: ~S; DATASET: ~S~%FITNESS FUNCTION: ~A~%"
                 (getf param-list :method)
                 (getf param-list :dataset)
                 (getf param-list :fitfunc))
         (hrule)
         (apply #'evolve param-list))))


(defun compactify (population)
  ) ;; define new fitfunc to reward the shortest sequence that still satisfies the old fitfunc 
