;;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;;; Linear Genetic Algorithm
;;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

(defpackage :genetic.linear
  (:use :common-lisp))

(in-package :genetic.linear)

(defparameter *DEBUG* nil)


;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; Virtual machine
;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=


(defun ./. (&rest args)
  "A divide-by-zero-proof division operator."
  (if (some #'zerop args) 0
      (apply #'/ args)))

(defparameter *regnum* 8)

(defparameter *registers*
  (make-array *regnum*))

(defparameter *minlen* 1) ;; we want to prevent seqs shrinking to nil!

(defparameter *opcodes*
  (vector  #'./. #'* #'- #'+))
;; nb: setting [0] as - will push towards emptying registers

(defvar *initial-register-state*
  '#(1 2 3 4 5 6 7 8))

(defparameter *opbits* (byte 2 0))

(defparameter *srcbits* (byte 3 2))

(defparameter *dstbits* (byte 3 5))

(defparameter *maxval* (expt 2 64))


(defun src? (inst)
  (ldb *srcbits* inst))

(defun dst? (inst)
  (ldb *dstbits* inst))

(defun op? (inst)
  (ldb *opbits* inst))

(defun parse-instruction (inst registers)
  (let ((opcode (op? inst))
        (srcreg (src? inst))
        (dstreg (dst? inst)))
    (when *DEBUG*
      (format t
              "read: ~8,'0b~%opcode: ~a~%src: R~d -> 0x~x~%dst: R~d -> 0x~x~%"
              inst (aref *opcodes* opcode) srcreg (aref registers srcreg)
              dstreg (aref registers dstreg)))
    (list (aref *opcodes* opcode) srcreg dstreg)))

(defun execute (inst registers)
  (let* ((ilist (parse-instruction inst registers))
         (op (car ilist))
         (dstreg (caddr ilist))
         (srcval (aref registers (cadr ilist)))
         (dstval (aref registers dstreg)))
    (setf (aref registers dstreg) (mod (apply op (list srcval dstval)) *maxval*))))

(defun execute-sequence (seq &optional (init-state *registers*))
  "Takes a sequence of instructions, seq, and an initial register 
state vector, init-state, and then runs the virtual machine, returning the
resulting value in R0."
  (let ((registers (copy-seq init-state))) ;; why is this affecting *r*?
    (loop for instruction in seq do
         (execute instruction registers))
    (and *debug* (loop for r from 0 to (1- (length registers)) do
                      (format t "R~d: 0x~x~%" r (aref registers r))))
    (aref registers 0)))

;; THESE EXECUTE FUNCS ARE VERY SLOW
;; *********************************


(defun load-registers (&rest rdata)
  (loop for i from 0 to (1- *regnum*) do
       (setf (aref *registers* i) (elt rdata i))))

(defun final-dst (seq reg)
  "Gives the subsequence of instructions that terminates in the last 
occurrence of reg in the DST position. Returns NIL if DST doesn't occur."
  (subseq seq 0 (1+ (or (position reg seq :key #'dst? :from-end t) -1))))
  

(defun tooshort (seq)
  (< (length seq) *minlen*))

(defun regfilter (popul r)
  (flet ((f (x) (final-dst x r)))
    (let* ((plist (concatenate 'list popul))
           (pfilt (remove-if #'null (mapcar #'f plist))) ; alt: #'tooshort
           (pvec (concatenate 'vector pfilt)))
      pvec)))

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
  (logxor inst (ash 1 (random 8))))

;; Some of these mutations are destructive, some are not, so we'll
;; rig each of them to return the mutated sequence, and we'll treat
;; them as if they were purely functional, and combine them with an
;; setf to mutate their target. 

(defun smutate-swap (seq)
  "Exchanges the position of two instructions in a sequence."
  (and *debug* (print "smutate-swap"))
  (let* ((len (length seq))
         (i (random len))
         (j (random len))
         (tmp (elt seq i)))
    (setf (elt seq i) (elt seq j))
    (setf (elt seq j) tmp))
  seq)

(defun smutate-push (seq)
  "Adds another (random) instruction to a sequence."
  (and *debug* (print "smutate-push"))
  (push (random #x100) seq)
  seq)

(defun smutate-pop (seq)
  "Decapitates a sequence."
  (and *debug* (print "smutate-pop"))
  (and (> (length seq) *minlen*) (pop seq))
  seq)

(defun smutate-butlast (seq)
  "Cuts the last instruction off of a sequence."
  (and *debug* (print "smutate-butlast"))
  (and (> (length seq) *minlen*) (setf seq (butlast seq)))
  seq)

(defun smutate-append (seq)
  "Adds another (random) instruction to the end of the sequence."
  (and *debug* (print "smutate-append"))
  (setf seq (append seq `(,(random #x100))))
  seq)

(defun smutate-imutate (seq)
  "Applies imutate-flip to a random instruction in the sequence."
  (and *debug* (print "smutate-imutate"))
  (let ((idx (random (length seq))))
    (setf (elt seq idx) (imutate-flip (elt seq idx))))
  seq)

(defparameter *mutations*
  (vector #'smutate-push #'smutate-imutate #'smutate-swap))

(defun random-mutation (seq)
  (apply (aref *mutations* (random (length *mutations*))) `(,seq)))

(defun maybe-mutate (seq)
  (if (< (random 100) *mutation-rate*)
      (random-mutation seq)
      seq))

(defun mutate-at-population-index (idx)
  (setf (elt *population* idx) (random-mutation (elt *population* idx))))

(defun crossover (p0 p1)
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
  (when (< (- high low) n)
    (error "Error in n-rnd: interval too small: infinite loop"))
  (loop (when (= (length r) n)
          (return r))
     (setf r (remove-duplicates (cons (+ low (random high)) r)))))


;; Standard for fitness functions: let 1 be maximum value. no lower bound -- may
;; go to arbitrarily small fractions < 1

(defun fitness-0 (seq)
  (let ((target 666))
    (/ 1 (1+ (abs (- (execute-sequence (final-dst seq 0)
                                       *initial-register-state*) target))))))
;; it's better to use final-dst in places like this. If we use it to filter out the
;; population at the get-go, we destroy a lot of diversity, and potentially useful
;; "junk DNA". 

(defun fitness (seq)
  (flet ((fitfunc (s)
           (fitness-0 s)))
    (let ((f (fitfunc seq)))
      (when (> f (car *best*))
        (setf (cadr *best*) seq)
        (setf (car *best*) f))
      f)))

(defun fitter (x y)
  "Adopting the convention that fitness is a fraction between 0 and 1."
  (> (fitness x) (fitness y)))

(defun tournement (population)
  (let* ((lots (n-rnd 0 (length *population*)))
         (combatants (mapcar #'(lambda (i) (list (elt population i) i)) lots))
         (ranked (sort combatants #'(lambda (x y) (fitter (car x) (car y)))))
         (winners (cddr ranked))
         (parents (mapcar #'car winners))
         (children (apply #'crossover parents))
         (losers (subseq ranked 0 2))
         (graves (mapcar #'cadr losers)))
    ;; (format t "GRAVES: ~a~%" graves)
    (map 'list #'(lambda (grave child) (setf (elt population grave) child))
         graves children)))
    ;; (format t "RANKED: ~a~%~%" ranked)
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
    
  
(defun roulette (population)
  ;; redundantly recalculates fitness every generation. would be good to memoize this shit. 
  ;;(let* ((wheel (mapcar #'(lambda (x) (cons (fitness x) x)) population)) ;; bump zeroes
  ;;       (fitsum (float (apply #'+ (mapcar #'car wheel))))
  ;;       (popsize (length population))
  ;;       (tally 0))
    ;; (format t "fitpop: ~a~%fitsum: ~f~%" wheel fitsum)
    ;;(loop for slot in wheel do
    ;;     (incf tally (float (car slot)))
    ;;    (setf (car slot) tally))
  (let* ((tally 0)
         (popsize (length population))
         (wheel (loop for creature in population
                   collect (progn
                             (let ((f (float (fitness creature))))
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
           
(defun next-generation ()
  (setf *population* (roulette *population*)))
    
    


(defun spawn-sequence (len)
  (loop repeat len collect (random #x100)))

(defun init-population (popsize slen)
  (loop repeat popsize collect (spawn-sequence slen)))


(defun test ()
  (and (= (length *population*) 0)
       (setf *population* (init-population 50 6)))
  (load-registers 1 2 3 4 5 6 7 8)
  (tournement *population*)
  (format t "BEST: ~a~cFITNESS: ~f~%~%" (cadr *best*) #\newline (car *best*)))


