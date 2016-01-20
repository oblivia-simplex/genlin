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

(defun inst->string (inst &key (registers *initial-register-state*)
                            (static nil))
  (concatenate 'string
               (format nil "[~a  R~d, R~d]"
                       (func->string (op? inst))
                       (src? inst) (dst? inst))
               (unless static (format nil " ;; (~f, ~f)"
                                      (aref registers (src? inst))
                                      (aref registers (dst? inst))))))

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
;; Execution procedure
;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

;; encapsulate in its own let environment?

;; MESSY, and TOO SLOW. Try to delegate debugging output elsewhere. 
(defun execute-sequence (seq &key (registers *initial-register-state* )
                               (input *default-input-reg*)
                               (static nil) (output nil))
  "Takes a sequence of instructions, seq, and an initial register 
state vector, registers, and then runs the virtual machine, returning the
resulting value in R0."

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
             (and *debug* (format t "~8,'0b  ~a~c" inst               ;;
                                  (inst->string inst :registers regs  ;;
                                                :static static)       ;;
                                  (if static #\newline #\space)))     ;;
             (incf (aref regs *pc-idx*))
             (unless static                                           ;;-
               (setf (aref regs D)
                     (rem (apply (op? inst)
                                 (list (aref regs (src? inst))
                                       (aref regs (dst? inst))o
                                       (aref regs *pc-idx*))) *maxval*))
               (and *debug* (format t ";; now R~d = ~f; PC = ~d~%"       ;;
                                    (dst? inst) (aref regs (dst? inst))  ;;
                                    (aref regs *pc-idx*))))              ;;
             (and (>= (aref regs *pc-idx*) seqlen) (return)))))
    (and *debug* (hrule))                                                ;;
    (mapcar #'(lambda (i) (aref regs i)) output)))


;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; Functions related to fitness measurement
;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

;; Convention for fitness functions: let 1 be maximum value. no lower bound -- may
;; go to arbitrarily small fractions < 1
(let ((.fitfunc. nil)
      (.hashtable. nil)
      (.testing-hashtable. nil)
      (.out-reg. nil))

  (defun init-fitness-env (&key fitfunc training-hashtable testing-hashtable out-reg)
    "Works sort of like a constructor, to initialize the fitness 
environment."
    (setf .fitfunc. fitfunc)
    (setf .hashtable. training-hashtable)
    (setf .testing-hashtable. testing-hashtable)
    (setf .out-reg. (copy-seq out-reg)))

  ;; ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  ;; Intron Removal and Statistics
  ;; ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

  (defun remove-introns (seq &key (out .out-reg.))
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
  
  ;; ......................................................................
  
  (defun fitness-binary-classifier-1 (seq)
    (flet ((error-gauge (raw goal)
             (/ (abs (+ (tanh (/ raw (/ *maxval* 10000))) goal)) 2)))
      (let ((results))
        (setf results (loop for pattern
                         being the hash-keys in .hashtable. collect
                           (error-gauge
                            (car (execute-sequence seq
                                                   :registers *initial-register-state*
                                                   :input pattern
                                                   :output .out-reg. ))
                            (gethash pattern .hashtable.))))
        (and *debug* *verbose*
             (format t "SEQUENCE:~a~%RESULTS:~%~a~%" seq results))
        (/ (apply #'+ results) (length results)))))
  
  (defun fitness-binary-classifier-2 (seq)
    (let ((correct 0)
          (incorrect 0))
      (loop for pattern being the hash-keys in .hashtable.
         using (hash-value v) do
           (let ((f (car (execute-sequence seq :input pattern))))
             (if (> (* f v) 0) (incf correct) (incf incorrect))))
      (if (zerop incorrect) 1
          (/ correct (+ correct incorrect)))))
  
  (defun fitness-ternary-classifier-1 (seq)
    "Where n is the target register, measures fitness as the ratio of
Rn to the sum of all output registers R0-R2 (wrt absolute value)."
    (let ((acc 0)) 
      (loop for pattern being the hash-keys in .hashtable.
         using (hash-value i) do
           (let ((output (execute-sequence seq :input pattern)))
             (incf acc (DIV (abs (nth i output))
                            (reduce #'+ (mapcar #'abs output))))))
      (/ acc (hash-table-count .hashtable.))))
  
  (defun fitness (crt)
    "Measures the fitness of a specimen, according to a specified
fitness function."
    (unless (creature-fit crt)  
      (setf (creature-eff crt) ;; assuming eff is not set, if fit is not.
            (remove-introns (creature-seq crt) :out .out-reg.))
      (setf (creature-fit crt)
            (funcall .fitfunc. (creature-eff crt)))
      (when (or (null (creature-fit *best*))
                (> (creature-fit crt) (creature-fit *best*)))
        (setf *best* (copy-structure crt))
        (and *debug* (format t "FITNESS: ~f~%BEST:    ~f~%"
                             (creature-fit crt) (creature-fit *best*)))))
    (creature-fit crt))

  (defun classification-report (crt dataset &key (testing t) (ht .testing-hashtable.))
    (if testing
        (setf ht .testing-hashtable.)
        (setf ht .hashtable.))
    (case dataset
      (tictactoe (ttt-classification-report crt ht))
      (iris (iris-classification-report crt ht))
      (otherwise (error "Unknown dataset."))))

  
  ) ;; end fitness environment

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
           (setf (creature-fit combatant)
                 (fitness combatant))))
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
    (format t "at line 614, hashtable = ~a~%" hashtable)
    (init-fitness-env :training-hashtable hashtable
                      :fitfunc #'fitness-binary-classifier-1
                      :out-reg '(0))
    (setf *best* (make-creature :fit 0))
    (setf *population* (init-population 500 *max-start-len*))
    (print "population initialized in *population*; data read")
    hashtable))

(defun setup-iris ()
  (let* ((filename "/home/oblivia/Projects/genetic-exercises/genetic-linear/datasets/Iris/iris.data")
         (hashtable (iris-datafile->hashtable filename)))
    (init-fitness-env :training-hashtable hashtable
                      :fitfunc fitness-ternary-classifier-1
                      :out-reg '(0 1 2))
    (setf *best* (make-creature :fit 0))
    (setf *population* (init-population 500 *max-start-len*))
    (print "population initialized in *population*; data read; hashtable in *ht*")
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

(defun run-breeder (&key (method #'tournement!) (dataset 'unknown)
                      (rounds 10000) (target 0.97))
  (let ((oldbest *best*))
    (funcall =logger= 'clear)
    (time (block evolver
            (dotimes (i rounds)
              (funcall method *population*)
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
                 (dataset 'tictactoe) (ratio 8/10))
  "Do everything: initialize a population and hashtable, then run the
breeder function, with specified method (roulette or tournmenent,
e.g.) for a specified number of rounds or until a given fitness target
is reached, whichever comes first."
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
  (run-breeder :dataset dataset :method method
               :rounds rounds :target target)
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


(defun compactify (population)
  ) ;; define new fitfunc to reward the shortest sequence that still satisfies the old fitfunc 

;; TODO:
;; replace the rest of the frequently-passed RO variables with
;; something more like "static" class variables, in let-over-defuns.
;; resist the temptation to do this with mutable variables, like
;; population, until it's clear that this won't interfere with hyperthreading.
