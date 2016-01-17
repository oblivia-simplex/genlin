;;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;;; Linear Genetic Algorithm
;;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

(defpackage :genetic.linear
  (:use :common-lisp))

(in-package :genetic.linear)

(defun loadfile (filename)
  (load (merge-pathnames filename *load-truename*)))

(loadfile "/home/oblivia/Projects/genetic-exercises/genetic-linear/tictactoe.lisp")

(defparameter *tictactoe-path* "/home/oblivia/Projects/genetic-exercises/genetic-linear/datasets/TicTacToe/tic-tac-toe-balanced.data")
(defparameter *DEBUG* nil)
(defparameter *VERBOSE* nil)
(defparameter *ht* (make-hash-table :test 'equal))


;;; TODO:



;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; Virtual machine
;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
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

(defparameter *wordsize* (+ opf srcf dstf))

(defparameter *max-inst* (expt 2 *wordsize*)) ;; upper bound on inst size

(defparameter *opbits* (byte *opf* 0))

(defparameter *srcbits* (byte *srcf* *opf*))

(defparameter *dstbits* (byte *dstf* (+ *srcf* *opf*)))

;; --- Operations: these can be tweaked independently of the 
;; --- fields above, so long as their are at least (expt 2 *opf*)
;; --- elements in the *opcodes* vector. 

(defun % (&rest args)
  "A divide-by-zero-proof division operator."
  (if (some #'zerop args) 0
      (reduce #'/ args)))

(defun ^ (&rest args) ;; xor integer parts
  (reduce #'(lambda (x y) (logxor (floor x) (floor y))) args))

(defun & (&rest args)
  (reduce #'(lambda (x y) (logand (floor x) (floor y))) args))

(defun v (&rest args)
  (reduce #'(lambda (x y) (lognot (logand  (lognot (floor x))
                                      (lognot (floor y))))) args))
(defun m (&rest args)
  (if (some #'zerop args) (car args)
      (mod (car args) (cadr args))))

(defparameter *opcodes*
  (vector  #'% #'* #'- #'+   ;; basic operations    (2bit opcode)
           #'^ #'v #'& #'m)) ;; extended operations (3bit opcode)

;; adding the extended opcodes seems to result in an immense boost in the
;; population's fitness -- 0.905 is now achieved in the time it took to
;; reach 0.64 with the basic operation set. (For tic-tac-toe.)


;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; Parameters for register configuration.
;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=




(defun sieve-of-eratosthenes (maximum) "sieve for odd numbers"
       ;; taken from Rosetta Code. 
       (cons 2
             (let ((maxi (ash (1- maximum) -1))
                   (stop (ash (isqrt maximum) -1)))
               (let ((sieve (make-array (1+ maxi)
                                        :element-type 'bit
                                        :initial-element 0)))
                 (loop for i from 1 to maxi
                    when (zerop (sbit sieve i))
                    collect (1+ (ash i 1))
                    and when (<= i stop) do
                      (loop for j from
                           (ash (* i (1+ i)) 1) to maxi by (1+ (ash i 1))
                         do (setf (sbit sieve j) 1)))))))


(defparameter *minlen* 2) ;; we want to prevent seqs shrinking to nil!

(defparameter *default-input-reg*
  #(0 0 0 0 0 0 0 0 0)) ;; need 9 for tic-tac-toe

(defparameter *default-registers*
  #(0 0 1 -1))

(defparameter *initial-register-state*
  (concatenate 'vector
               *default-registers*
               *default-input-reg*
               (sieve-of-eratosthenes 18))) ;; some primes for fun

(defparameter *input-start-idx* (length *default-registers*))

(defparameter *input-stop-idx*
  (+ *input-start-idx* (length *default-input-reg*)))



;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

(defparameter *maxval* (expt 2 16)) ;; max val that can be stored in reg


(defparameter *max-len* 256) ;; max instruction length
(defparameter *max-startlen* 25) ;; max initial instruction length


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


(defun enter-input (registers input)
  (let ((copy (copy-seq registers)))
    (setf (subseq copy *input-start-idx* (+ *input-start-idx* (length input)))
          input)
    copy))

;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; debugging functions

(defun print-registers (registers)
  (loop for i from 0 to (1- (length registers)) do
       (format t "R~d: ~6f ~c" i (aref registers i) #\TAB)
       (if (= 0 (mod (1+ i) 5)) (format t "~%")))
  (format t "~%"))

(defun func->string (func)
  (let* ((fm (format nil "~a" func))
         (i (mismatch fm "#<FUNCTION __"))
         (o (subseq fm i (1- (length fm)))))
    o))

(defun inst->string (inst &optional (registers *initial-register-state*))
  (format nil "[~a R~d, R~d] ;; (~f, ~f)"
          (func->string (op? inst)) (src? inst) (dst? inst)
          (aref registers (src? inst)) (aref registers (dst? inst))))
(defun hrule ()
  (format t "-----------------------------------------------------------------------------~%"))



(defun disassemble-sequence (seq &optional input)
  (let ((registers (enter-input *initial-register-state* input)))
    (hrule)
    (format t "~%")
    (print-registers registers)
    (hrule)
    (loop for inst in seq do
         
         (format t "~a" (inst->string inst registers))
         (setf (aref registers (dst? inst))
               (apply (op? inst)
                      (list (aref registers (src? inst))
                            (aref registers (dst? inst)))))
         (format t " ;; now (R~d) = ~f~%" (dst? inst) (aref registers (dst? inst))))
    (hrule)
    (format t "~%")
    (print-registers registers)
    (hrule)))
    
(defun dbg ()
  (setf *debug* (not *debug*)))

;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

(defun execute-sequence (sequence &key (init-state *initial-register-state* )
                                    (input *default-input-reg*)
                                    (output 0))
  "Takes a sequence of instructions, seq, and an initial register 
state vector, init-state, and then runs the virtual machine, returning the
resulting value in R0."
  (declare (optimize (speed 1)))
  (declare (type (simple-array rational (*)) init-state input *default-input-reg*
                *initial-register-state*))
  (declare (type fixnum output *input-start-idx*))
  (declare (inline src? dst? op?))
  (let ((registers (copy-seq init-state))
        (seq (final-dst sequence output))) ;; remove introns
    ;; the input values will be head in read-only registers
    (setf (subseq registers *input-start-idx*
                  (+ *input-start-idx* (length input))) input)
    ;;      (format t "input: ~a~%registers: ~a~%" inp registers)
    (loop for inst in seq do
         (setf (aref registers (dst? inst))
               (rem (apply (op? inst) (list (aref registers (src? inst))
                                            (aref registers (dst? inst))))
                    *maxval*)) ;; keep register vals from getting too big
         (and *debug* *verbose* (disassemble-inst inst)
              (print-registers registers)))
    (aref registers output)))


;; THESE EXECUTE FUNCS ARE VERY SLOW
;; *********************************


(defun final-dst (seq reg)
  "Gives the subsequence of instructions that terminates in the last 
occurrence of reg in the DST position. Returns NIL if DST doesn't occur."
  (declare (type cons seq))
  (declare (type fixnum reg))
  (subseq seq 0 (1+ (or (position reg seq :key #'dst? :from-end t) -1))))
  

;; (defun tooshort (seq)
;;   (< (length seq) *minlen*))

;; (defun regfilter (popul r)
;;   (flet ((f (x) (final-dst x r)))
;;     (let* ((plist (concatenate 'list popul))
;;            (pfilt (remove-if #'null (mapcar #'f plist))) ; alt: #'tooshort
;;            (pvec (concatenate 'vector pfilt)))
;;       pvec)))

;; Alternate:
;;
;; (defun fdr (s r)
;;   (loop
;;      (when (= (dst? (car s)) r)
;;        (return s))
;;      (pop s)))

(defun seek-reg (seq reg)
  (let* ((rseq (reverse seq))
         (srcs (remove-if-not #'(lambda (x) (= reg (src? x))) rseq))
         (dsts (remove-if-not #'(lambda (x) (= reg (dst? x))) rseq)))
    (print srcs)
    (print dsts)))

;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; Genetic components
;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

(defparameter *best* '())
(defparameter *population* '())
(defparameter *startlen* 10)
(defparameter *mutation-rate* 15)

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
  (declare (type cons seq))
  (and *debug* (print "smutate-swap"))
  (let* ((len (length seq))
         (i (random len))
         (j (random len))
         (tmp (nth i seq)))
    (setf (nth i seq) (nth i seq))
    (setf (nth j seq) tmp))
  seq)

(defun smutate-push (seq)
  "Adds another (random) instruction to a sequence."
  (declare (type cons seq))
  (and *debug* (print "smutate-push"))
  (push (random #x100) seq)
  seq)

(defun smutate-pop (seq)
  "Decapitates a sequence."
  (declare (type cons seq))
  (and *debug* (print "smutate-pop"))
  (and (> (length seq) *minlen*) (pop seq))
  seq)

(defun smutate-butlast (seq)
  "Cuts the last instruction off of a sequence."
  (declare (type cons seq))
  (and *debug* (print "smutate-butlast"))
  (and (> (length seq) *minlen*) (setf seq (butlast seq)))
  seq)

(defun smutate-grow (seq)
  "Adds another (random) instruction to the end of the sequence."
  (declare (type cons seq))
  (and *debug* (print "smutate-append"))
  (setf seq (concatenate 'list seq `(,(random #x100))))
  seq)

(defun smutate-imutate (seq)
  "Applies imutate-flip to a random instruction in the sequence."
  (declare (type cons seq))
  (and *debug* (print "smutate-imutate"))
  (let ((idx (random (length seq))))
    (setf (elt seq idx) (imutate-flip (elt seq idx))))
  seq)

(defparameter *mutations*
  (vector #'smutate-grow #'smutate-imutate #'smutate-swap))

(defun random-mutation (seq)
  (declare (type cons seq))
  (apply (aref *mutations* (random (length *mutations*))) `(,seq)))

(defun maybe-mutate (seq)
  (declare (type cons seq))
  (if (< (random 100) *mutation-rate*)
      (random-mutation seq)
      seq))

(defun mutate-at-population-index (idx)
  (setf (elt *population* idx) (random-mutation (elt *population* idx))))

(defun crossover (p0 p1)
  (declare (type cons p0 p1))
  (declare (optimize (speed 2)))
  (let* ((p00 (cdr p0))
         (p01 (cdr p1))
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
    (declare (type cons mother father daughter son))
    (declare (type fixnum idx0 idx1 minidx maxidx offset r-align))
    ;(format t "minidx: ~d  maxidx: ~d  offset: ~d~%" minidx maxidx offset)
    (setf (subseq daughter (+ offset minidx) (+ offset maxidx))
          (subseq father minidx maxidx))
    (setf (subseq son minidx maxidx)
          (subseq mother (+ offset minidx) (+ offset maxidx)))         
    ;(format t "mother: ~a~%father: ~a~%daughter: ~a~%son: ~a~%"
    ;        mother father daughter son)
    (list (cons nil (maybe-mutate son)) (cons nil (maybe-mutate daughter)))))
;; we still need to make this modification to roulette, to accommodate the
;; fitness-storing cons cell in the car of each individual 

(defun n-rnd (low high &optional (r '()) (n 4))
  "Returns a list of n distinct random numbers between low and high."
  (declare (type fixnum low high n))
  (declare (optimize speed))
  (when (< (- high low) n)
    (error "Error in n-rnd: interval too small: infinite loop"))
  (loop (when (= (length r) n)
          (return r))
     (setf r (remove-duplicates (cons (+ low (random high)) r)))))


;; Standard for fitness functions: let 1 be maximum value. no lower bound -- may
;; go to arbitrarily small fractions < 1


(declaim (inline binary-error-measure))
(defun binary-error-measure (raw goal)
  ;; goal is a boolean value (t or nil)
  ;; raw is, typically, the return value in r0
  (let ((div (/ *maxval* 10000)))
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
  (declare (type hash-table hashtable))
  (declare (type cons seq))
  (declare (optimize speed))
  
  (let ((results (loop for pattern being the hash-keys in hashtable collect
                      (binary-error-measure (execute-sequence seq :input pattern)
                                            (gethash pattern hashtable)))))
    (declare (type (cons rational) results))
    (and *debug* *verbose*
         (format t "SEQUENCE:~a~%RESULTS:~%~a~%" seq results))
    (/ (apply #'+ results) (length results)))) ;; average

(defun fitness-binary-classifier-2 (seq hashtable)
  (declare (type hash-table hashtable))
  (declare (type (cons rational) seq))
  (declare (optimize speed))
  (let ((correct 0)
        (incorrect 0))
    (declare (type integer correct incorrect))
    (loop for pattern being the hash-keys in hashtable do
         (let ((f (execute-sequence seq :input pattern))
               (v (gethash pattern hashtable)))
           (declare (type rational f v))
           (if (> (* f v) 0) (incf correct) (incf incorrect))))
    ;;    (format t "SEQ: ~a~%CORRECT: ~d    INCORRECT ~d~%~%" seq correct incorrect)
    (if (zerop incorrect) 1
        (/ correct (+ correct incorrect)))))


(defun fitness-0 (seq)
  (let ((target 666))
    (/ 1 (1+ (abs (- (execute-sequence (final-dst seq 0)
                                       :init-state *initial-register-state*)
                     target))))))
;; it's better to use final-dst in places like this. If we use it to filter out the
;; population at the get-go, we destroy a lot of diversity, and potentially useful
;; "junk DNA". 

(defun fitness (seq &key (lookup nil))
  ;; we execute (cdr seq), because the first cons cell of
  ;; each sequence stores the fitness of that sequence, or
  ;; else, nil. 
  (unless (car seq)
    
    (flet ((fitfunc (s)
             (fitness-binary-classifier-1 s lookup)))
      (pop seq)
      (push (fitfunc seq) seq)
      (when (or (null *best*) (> (car seq) (car *best*)))
        (setf *best* (copy-seq seq))
        (and *debug* (format t "FITNESS: ~f~%BEST:    ~f~%" (car seq) (car *best*))))))
  ;;(format t "SEQ: ~a~%" seq)
  (car seq))

(defun tournement (population &key (lookup nil))
  (let* ((lots (n-rnd 0 (length *population*)))
         (combatants (mapcar #'(lambda (i) (list (nth i population) i)) lots)))
  ;;  (format t "COMBATANTS: ~a~%" combatants)
    (loop for combatant in combatants do
         (if (null (caar combatant))
             (setf (caar combatant)
                   (fitness (car combatant) :lookup lookup))))
;;    (format t "COMBATANTS: ~a~%" combatants)
    (let* ((ranked (sort combatants #'(lambda (x y) (< (caar x) (caar y)))))
           (winners (cddr ranked))
           (parents (mapcar #'car winners))
           (children (apply #'crossover parents))
           (losers (subseq ranked 0 2))
           (graves (mapcar #'cadr losers)))
      ;;  (format t "GRAVES: ~a~%" graves)
      (map 'list #'(lambda (grave child) (setf (nth grave population) child))
           graves children)
    ;;  (format t "RANKED: ~a~%" ranked)
      *best*)))
    ;; (mapcar #'fitness children) ;; to update the *best* variable
    ;; (format t "LOSERS:~c~c~a [~d]~c~a [~d]~%WINNERS:~c~a [~d]~c~a [~d]~%OFFSPRING:~c~a~c~a~%~%"
            ;; #\Tab #\Tab (caar losers) (cadar losers)
            ;; #\Tab (caadr losers) (cadadr losers)
            ;; #\Tab (car parents) (cadar winners)
            ;; #\Tab (cadr parents) (cadadr winners) 
            ;; #\Tab (car children) #\Tab (cadr children))))
    

;; starting to wonder if *population* should just be a list, instead of a vector...
;; i seem to be converting it to a list at almost every turn. how much time is really
;; spend indexing into it, anyways? only in tournement, really.

(defun spin-wheel (wheel top)
  (let ((ball (random (float top)))
        (ptr (car wheel)))
    ;; (format t "TOP: ~a~%BALL: ~a~%" top ball)
    (loop named spinning for slot in wheel do
         (when (< ball (car slot))
           (return-from spinning))
         (setf ptr slot))
    ;; ptr now points to where the ball 'lands'
    ;; (and (null ptr) (error "NIL PTR RESULTING FROM SPIN WHEEL."))
    ;; (format t "PTR: ~a~%" ptr)
    (cdr ptr)))
    
  
(defun roulette (population &key (lookup nil))
  (let* ((tally 0)
         (popsize (length population))
         (wheel (loop for creature in population
                   collect (progn
                             (let ((f (float (fitness creature
                                                      :lookup lookup))))
                               (incf tally f)
                               (cons tally creature)))))
         ;; the roulette wheel is now built
         ;;(format t "WHEEL: ~a~%" wheel)
         (breeders (loop for i from 1 to popsize
                      collect (spin-wheel wheel tally)))
         (half (/ popsize 2))
         (mothers (subseq breeders 0 half))
         (fathers (subseq breeders half popsize)))
    ;;(format t "MOTHERS: ~a~%FATHERS: ~a~%" mothers fathers)
    ;;(print "CHILDREN:")
    (apply #'concatenate 'list (map 'list #'crossover mothers fathers))))
           
(defun next-generation (&key (lookup nil))
  (setf *population* (roulette *population* :lookup lookup)))

;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; Running the programme
;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

(defun spawn-sequence (len)
  (cons nil (loop repeat len collect (random *max-inst*))))

(defun init-population (popsize slen)
  (loop repeat popsize collect (spawn-sequence (+ *minlen* (random slen)))))


(defun test ()
  (and (= (length *population*) 0)
       (setf *population* (init-population 50 6)))
  (load-registers 1 2 3 4 5 6 7 8)
  (tournement *population*)
  (format t "BEST: ~a~cFITNESS: ~f~%~%"
          (cdr *best*) #\newline (car *best*)))

(defun setup-tictactoe (&optional (graycode t))
  (let* ((filename "/home/oblivia/Projects/genetic-exercises/genetic-linear/datasets/TicTacToe/tic-tac-toe-balanced.data")
         (hashtable (datafile->hashtable filename :int graycode)))
    (setf *best* '())
    (setf *population* (init-population 500 *max-startlen*))
    (print "population initialized; data read; returning hashtable")
    (setf *ht* hashtable)
    hashtable))
    
;; read in tictactoe boards as base-3 numerals

;; I think nth is marginally faster than elt for list access.

;; modify sequence structure so that each sequence stores its fitness score
;; in its car. this will let us avoid re-evaluating the same individual
;; and access to the code is easy obtained in Theta(1) with cdr.

(defun classification-report (s &optional (ht *ht*))
  (format t "REPORT FOR ~a~%=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=~%" s)
  (let ((correct 0)
        (incorrect 0)
        (seq (cdr s)))
    (loop for k being the hash-keys in ht do
         (let ((i (aref k 0))
               (f (execute-sequence seq :input k)))
           (format t "~a~%" (int->board i))
         (cond ((> (* (gethash k ht) f) 0)
                (format t "CORRECTLY CLASSIFIED ~a -> ~f~%~%" i f)
                (incf correct))
               ((< (* (gethash k ht) f) 0)
                (format t "INCORRECTLY CLASSIFIED ~a -> ~f~%~%" i f)
                (incf incorrect))
               (t (format t "WHO'S TO SAY? ~a -> ~f~%~%" i f)))))
    (format t "TOTAL CORRECT:   ~d~%TOTAL INCORRECT: ~d~%"
            correct incorrect)))

(defun do-tournements (rounds)
  (let ((oldbest *best*))
    (loop repeat rounds do
         (tournement *population* :lookup *ht*)
         (when (or (null *best*) (> (car *best*) (car oldbest)))
           (setf oldbest *best*)
           (format t "NEW BEST: ~a~%" *best*)))
    (format t "BEST: ~f~%" (car *best*))))

