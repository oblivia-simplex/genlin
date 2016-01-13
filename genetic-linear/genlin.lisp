;;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;;; Linear Genetic Algorithm
;;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

(defpackage :genetic.linear
  (:use :common-lisp))

(in-package :genetic.linear)

(defparameter *DEBUG* t)


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

(defparameter *initial-register-state*
  '(1 1 1 1 1 1 1 1))


(defparameter *opbits* (byte 2 0))
(defparameter *srcbits* (byte 3 2))
(defparameter *dstbits* (byte 3 5))


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
      (format t "read: ~8,'0b~%opcode: ~a~%src: R~d -> 0x~x~%dst: R~d -> 0x~x~%"
              inst (aref *opcodes* opcode) srcreg (aref registers srcreg)
              dstreg (aref registers dstreg)))
    (list (aref *opcodes* opcode) srcreg dstreg)))

(defun execute (inst registers)
  (let* ((ilist (parse-instruction inst registers))
         (op (car ilist))
         (dstreg (caddr ilist))
         (srcval (aref registers (cadr ilist)))
         (dstval (aref registers dstreg)))
    (setf (aref *registers* dstreg) (apply op (list srcval dstval)))))


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

(defun load-registers (&rest rdata)
  (loop for i from 0 to (1- *regnum*) do
       (setf (aref *registers* i) (elt rdata i))))

(defun final-dst (seq reg)
  "Gives the subsequence of instructions that terminates in the last 
occurrence of reg in the DST position."
  (labels ((fdr (s r)
             (append (and s (if (= (dst? (car s)) r) s (fdr (cdr s) r))))))
    (fdr (reverse seq) reg)))

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

(defparameter *best* '(999999999999999 '()))
(defparameter *popsize* 100)
(defparameter *population* (make-array `(,*popsize*)
                                       :adjustable t :fill-pointer 0))
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
  (vector #'smutate-imutate #'smutate-butlast #'smutate-pop #'smutate-push
          #'smutate-append #'smutate-swap))

(defun random-mutation (seq)
  (apply (aref *mutations* (random (length *mutations*))) `(,seq)))

(defun maybe-mutate (seq)
  (if (< (random 100) *mutation-rate*)
      (random-mutation seq)
      seq))

(defun mutate-at-population-index (idx)
  (setf (aref *population* idx) (random-mutation (aref *population* idx))))

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


(defun fitness-0 (seq)
  (let ((target 666))
    (abs (- (execute-sequence seq) target))))


(defun fitness (seq)
  (flet ((fitfunc (s)
           (fitness-0 s)))
    (let ((f (fitfunc seq)))
      (when (< f (car *best*))
        (setf (cadr *best*) seq)
        (setf (car *best*) f))
      f)))

(defun fitter (x y)
  "Assumes that fitness peaks at 0."
  (< (fitness x) (fitness y)))


(defun tournement (population)
  (let* ((lots (n-rnd 0 (length *population*)))
         (combatants (mapcar #'(lambda (i) (list (aref population i) i)) lots))
         (ranked (sort combatants
                       #'(lambda (x y) (< (fitness (car y)) (fitness (car x))))))
         (winners (cddr ranked))
         (parents (mapcar #'car winners))
         (children (apply #'crossover parents))
         (losers (subseq ranked 0 2))
         (graves (mapcar #'cadr losers)))
    ;; (format t "GRAVES: ~a~%" graves)
    (map 'list #'(lambda (grave child) (setf (aref population grave) child))
         graves children)
    (format t "RANKED: ~a~%~%" ranked)
    ;; (mapcar #'fitness children) ;; to update the *best* variable
    (format t "LOSERS:~c~c~a [~d]~c~a [~d]~%WINNERS:~c~a [~d]~c~a [~d]~%OFFSPRING:~c~a~c~a~%~%"
            #\Tab #\Tab (caar losers) (cadar losers)
            #\Tab (caadr losers) (cadadr losers)
            #\Tab (car parents) (cadar winners)
            #\Tab (cadr parents) (cadadr winners) 
            #\Tab (car children) #\Tab (cadr children))
    ))

(defun spawn-sequence (len)
  (loop repeat len collect (random #x100)))

(defun init-population (popsize slen)
  (concatenate 'vector (loop repeat popsize collect (spawn-sequence slen))))


(defun test ()
  (and (= (length *population*) 0)
       (setf *population* (init-population 50 6)))
  (load-registers 1 2 3 4 5 6 7 8)
  (tournement *population*)
  (format t "BEST: ~a~cFITNESS: ~f~%~%" (cadr *best*) #\newline (car *best*)))
