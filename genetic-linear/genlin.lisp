;;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;;; Linear Genetic Algorithm
;;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

(defpackage :genetic.linear
  (:use :common-lisp))

(in-package :genetic.linear)

(defun loadfile (filename)
  (load (merge-pathnames filename *load-truename*)))

(loadfile "tictactoe.lisp")

(defparameter *tictactoe-path* "/home/oblivia/Projects/genetic-exercises/genetic-linear/datasets/TicTacToe/tic-tac-toe-balanced.data")
(defparameter *DEBUG* nil)
(defparameter *VERBOSE* nil)
(defparameter *ht* (make-hash-table :test 'equal))


;;; TODO:



;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; Virtual machine
;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=


(defun % (&rest args)
  "A divide-by-zero-proof division operator."
  (if (some #'zerop args) 0
      (apply #'/ args)))

(defparameter *minlen* 1) ;; we want to prevent seqs shrinking to nil!

(defparameter *opcodes*
  (vector  #'% #'* #'- #'+))
;; nb: setting [0] as - will push towards emptying registers


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

;; There are 128 registers in total, but only 8 of these are writeable.
;; R8 through R127 store the first 120 prime numbers. 

               ;;                   0-7 0-7
               ;;                   111  1111|111  11    

;; DD SSSS OO
;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

(defparameter *opbits* (byte 2 0))

(defparameter *srcbits* (byte 4 2))

(defparameter *dstbits* (byte 2 6))

;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

(defparameter *maxval* (expt 2 16)) ;; max val that can be stored in reg

(defparameter *wordsize* (expt 2 8)) ;; max val of instruction, as int

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


;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; debugging functions

(defun print-registers (registers)
  (loop for i from 0 to (1- (length registers)) do
       (format t "R~d: ~x ~%" i (aref registers i)))
  (format t "~%"))

(defun disassemble-sequence (seq)
  (let ((registers *initial-register-state*))
    (setf registers (copy-seq *initial-register-state*))
    (print-registers registers)
    (loop for inst in seq do
         
         (format t "INST: ~8,'0b~%OP:  ~a~%SRC: R~d -> 0x~x~%DST: R~d -> 0x~x~%"
                 inst (op? inst) (src? inst)
                 (aref registers (src? inst))
                 (dst? inst) (aref registers (dst? inst)))
         (setf (aref registers (dst? inst))
               (apply (op? inst)
                      (list (aref registers (src? inst))
                            (aref registers (dst? inst))))))
    (print-registers registers)))
    
(defun dbg ()
  (setf *debug* (not *debug*)))

;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

(defun parse-instruction (inst)
  (declare (type fixnum inst))
  (let ((opcode (op? inst))
        (srcreg (src? inst))
        (dstreg (dst? inst)))
    (list (aref *opcodes* opcode) srcreg dstreg)))



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

(defun disassemble-inst (inst)
  (format t "~a R~d, R~d~%" (op? inst) (src? inst) (dst? inst))
  t)
;; THESE EXECUTE FUNCS ARE VERY SLOW
;; *********************************


(defun load-registers (&rest rdata)
  (loop for i from 0 to (1- *regnum*) do
       (setf (aref *registers* i) (elt rdata i))))

(defun final-dst (seq reg)
  "Gives the subsequence of instructions that terminates in the last 
occurrence of reg in the DST position. Returns NIL if DST doesn't occur."
  (declare (type (cons fixnum) seq))
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

(defparameter *best* '(0 '()))
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
  (declare (type (cons fixnum) seq))
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
  (declare (type (cons fixnum) seq))
  (and *debug* (print "smutate-push"))
  (push (random #x100) seq)
  seq)

(defun smutate-pop (seq)
  "Decapitates a sequence."
  (declare (type (cons fixnum) seq))
  (and *debug* (print "smutate-pop"))
  (and (> (length seq) *minlen*) (pop seq))
  seq)

(defun smutate-butlast (seq)
  "Cuts the last instruction off of a sequence."
  (declare (type (cons fixnum) seq))
  (and *debug* (print "smutate-butlast"))
  (and (> (length seq) *minlen*) (setf seq (butlast seq)))
  seq)

(defun smutate-grow (seq)
  "Adds another (random) instruction to the end of the sequence."
  (declare (type (cons fixnum) seq))
  (and *debug* (print "smutate-append"))
  (setf seq (concatenate 'list seq `(,(random #x100))))
  seq)

(defun smutate-imutate (seq)
  "Applies imutate-flip to a random instruction in the sequence."
  (declare (type (cons fixnum) seq))
  (and *debug* (print "smutate-imutate"))
  (let ((idx (random (length seq))))
    (setf (elt seq idx) (imutate-flip (elt seq idx))))
  seq)

(defparameter *mutations*
  (vector #'smutate-grow #'smutate-imutate #'smutate-swap))

(defun random-mutation (seq)
  (declare (type (cons fixnum) seq))
  (apply (aref *mutations* (random (length *mutations*))) `(,seq)))

(defun maybe-mutate (seq)
  (declare (type (cons fixnum) seq))
  (if (< (random 100) *mutation-rate*)
      (random-mutation seq)
      seq))

(defun mutate-at-population-index (idx)
  (setf (elt *population* idx) (random-mutation (elt *population* idx))))

(defun crossover (p0 p1)
  (declare (type (cons fixnum) p0 p1))
  (declare (optimize (speed 2)))
  (let* ((parents (sort (list p0 p1) #'(lambda (x y) (< (length x) (length y)))))
         (father (car parents))
         (mother (cadr parents)) ;; let the father be the shorter of the two
         (r-align (random 2)) ;; 1 or 0
         (offset (* r-align (- (length mother) (length father))))
         (daughter (copy-seq mother))
         (son (copy-seq father))
         (idx0 (random (length father)))
         (idx1 (random (length father)))
         (minidx (min idx0 idx1))
         (maxidx (max idx0 idx1)))
    (declare (type (cons fixnum) mother father daughter son))
    (declare (type fixnum idx0 idx1 minidx maxidx offset r-align))
    ;(format t "minidx: ~d  maxidx: ~d  offset: ~d~%" minidx maxidx offset)
    (setf (subseq daughter (+ offset minidx) (+ offset maxidx))
          (subseq father minidx maxidx))
    (setf (subseq son minidx maxidx)
          (subseq mother (+ offset minidx) (+ offset maxidx)))         
    ;(format t "mother: ~a~%father: ~a~%daughter: ~a~%son: ~a~%"
    ;        mother father daughter son)
    (list (maybe-mutate son) (maybe-mutate daughter))))

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


(defun binary-error-measure (raw goal)
  ;; goal is a boolean value (t or nil)
  ;; raw is, typically, the return value in r0
  ;(let ((d 1))
    (flet ((sigmoid (x)
             (tanh (/ x *maxval*))))
      (/ (abs (+ (sigmoid raw) goal)) 2)))



(defun fitness-binary-classifier-1 (seq hashtable)
  ;; positive = t; negative = nil
  (declare (type hash-table hashtable))
  (declare (type (cons fixnum) seq))
  (declare (optimize speed))
  (let ((results
         (loop for pattern being the hash-keys in hashtable collect
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
  (flet ((fitfunc (s)
           (fitness-binary-classifier-2 s lookup)))
    (let ((f (fitfunc seq)))
      (when (> f (car *best*))
        (setf (cadr *best*) seq)
        (setf (car *best*) f))
      (and *debug* (format t "FITNESS: ~f~%BEST:    ~f~%" f (car *best*)))
      f)))

(defun fitter (x y &key (lookup nil))
  "Adopting the convention that fitness is a fraction between 0 and 1."
  (> (fitness x :lookup lookup) (fitness y :lookup lookup)))

(defun tournement (population &key (lookup nil))
  (let* ((lots (n-rnd 0 (length *population*)))
         (combatants (mapcar #'(lambda (i) (list (nth i population) i)) lots))
         (ranked (sort combatants #'(lambda (x y) (fitter (car x) (car y)
                                                     :lookup lookup))))
         (winners (cddr ranked))
         (parents (mapcar #'car winners))
         (children (apply #'crossover parents))
         (losers (subseq ranked 0 2))
         (graves (mapcar #'cadr losers)))
  ;;  (format t "GRAVES: ~a~%" graves)
    (map 'list #'(lambda (grave child) (setf (nth grave population) child))
         graves children)
    ;;(format t "RANKED: ~a~%" ranked)
    *best*))
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
  (loop repeat len collect (random #x400)))

(defun init-population (popsize slen)
  (loop repeat popsize collect (spawn-sequence (+ *minlen* (random slen)))))


(defun test ()
  (and (= (length *population*) 0)
       (setf *population* (init-population 50 6)))
  (load-registers 1 2 3 4 5 6 7 8)
  (tournement *population*)
  (format t "BEST: ~a~cFITNESS: ~f~%~%"
          (cadr *best*) #\newline (car *best*)))

(defun setup-tictactoe (&optional (int nil))
  (let* ((filename "/home/oblivia/Projects/genetic-exercises/genetic-linear/datasets/TicTacToe/tic-tac-toe-balanced.data")
         (hashtable (datafile->hashtable filename :int int)))
    (setf *population* (init-population 500 *max-startlen*))
    (print "population initialized; data read; returning hashtable")
    (setf *ht* hashtable)
    hashtable))
    
;; read in tictactoe boards as base-3 numerals

;; I think nth is marginally faster than elt for list access.

;; modify sequence structure so that each sequence stores its fitness score
;; in its car. this will let us avoid re-evaluating the same individual
;; and access to the code is easy obtained in Theta(1) with cdr.

(defun classification-report (seq ht)
  (format t "REPORT FOR ~a~%=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=~%" seq)
  (let ((correct 0)
        (incorrect 0))
    (loop for k being the hash-keys in ht do
         (let ((f (execute-sequence seq :input k)))
           (format t "~a~%" (draw-board k))
         (cond ((> (* (gethash k ht) f) 0)
                (format t "CORRECTLY CLASSIFIED ~a -> ~f~%~%" k f)
                (incf correct))
               ((< (* (gethash k ht) f) 0)
                (format t "INCORRECTLY CLASSIFIED ~a -> ~f~%~%" k f)
                (incf incorrect))
               (t (format t "WHO'S TO SAY? ~a -> ~f~%~%" k f)))
         (format t "TOTAL CORRECT:   ~d~%TOTAL INCORRECT: ~d~%"
                 correct incorrect)))))

(defun do-tournements (rounds)
  (let ((oldbest *best*))
    (loop repeat rounds do
         (tournement *population* :lookup *ht*)
         (if (> (car *best*) (car oldbest))
             (print *best*)))))

