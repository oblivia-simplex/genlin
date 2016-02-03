;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;;   ___ ___ _  _ _    ___ _  _
;;  / __| __| \| | |  |_ _| \| |
;; | (_ | _|| .` | |__ | || .` |
;;  \___|___|_|\_|____|___|_|\_| Linear Genetic Programming Engine
;;
;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=



;;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;;; Linear Genetic Algorithm
;;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
(load #p"~/Projects/genlin/package.lisp")

(in-package :genlin)

(defparameter *project-path*
  "~/Projects/genlin/")

(defun loadfile (filename)
  (load (merge-pathnames filename *load-truename*)))

;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

(defparameter *machine* :stackmachine)


;; (defparameter *machine-file*
;;   (case *machine*
;;     ((:stackmachine "stackmachine.lisp"))
;;     ((:slomachine) "slomachine.lisp")
;;     ((:r-machine) "r-machine.lisp")))
;; ;; slomachine is an unstable VM for self-modifying code

(defun load-other-files ()
  (loop for f in `("params.lisp"
                   "auxiliary.lisp"
                   "stackmachine.lisp"
                   "datafiler.lisp"
                   "frontend.lisp"
                   "tictactoe.lisp"
                   "iris.lisp") do
       (loadfile (concatenate 'string *project-path* f))))


(load-other-files)

;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; Logging
;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

(defun make-logger ()
  (let ((log (list (cons 0 0)))) ;; prevents a "nil is not a number" error
    (lambda (&optional (entry nil))
      (cond ((eq entry 'CLEAR) (setf log '()))
            (entry (push entry log))
            (t log)))))

(defparameter *records* '())

(defun reset-records ()
  (setf *records*
        '(:fitter-than-parents 0
          :less-fit-than-parents 0
          :as-fit-as-parents 0)))

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
  (divide (length (remove-if #'null (boolean-seq-schema-match seq1 seq2)))
     (max (length seq1) (length seq2))))


;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; debugging functions
;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

(defun dbg (&optional on-off)
  (case on-off
    ((on)  (setf *debug* t))
    ((off) (setf *debug* nil))
    (otherwise (setf *debug* (not *debug*)))))

;; ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;; Intron Removal
;; ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

(defun remove-introns (seq &key (output '(0)))
  "Filters out non-effective instructions from a sequence -- instructions
that have no bearing on the final value(s) contained in the output 
register(s)."
  ;; NB: When code is r/w w LOD, STO, then remove-introns cannot be expected
  ;; to return reliable results.
  ;; we can test sensitivity of LOD/STO instructions to input. If there are
  ;; either no LOD/STO instructions, or if the LOD/STO instructions are in-
  ;; sensitive to input, then EFF can be extracted in the usual fashion.
  ;; Creatures whose control flow is contingent on input via LOD/STO should be
  ;; flagged or logged, just out of curiosity, and to gather statistics on
  ;; their frequency. 
  (let ((efr output)
        (efseq '()))
    ;; Until I code up a good, efficient way of finding entrons in
    ;; a piece of potentially self-modifying code, this will have to do:
    (cond ((and (eq *machine* :slomachine)
                (some #'(lambda (inst) (or (equalp (op? inst) #'STO)
                                      ;; and LOD can access any instruction
                                      (equalp (op? inst) #'LOD))) seq))
           seq) ;; hence the filename: slomachine.lisp (also Store/LOad)
          (t (loop for i from (1- (length seq)) downto 0 do
                  (let ((inst (elt seq i)))
                    (when (member (dst? inst) efr)
                      (push (src? inst) efr)
                      (push inst efseq)
                   (when (and (not (zerop i)) (jmp? (elt seq (1- i))))
                     ;; a jump immediately before an effective instruction
                     ;; is also an effective instruction. 
                     (let ((prevjmp (elt seq (1- i))))
                       (push (src? prevjmp) efr)
                       (push (dst? prevjmp) efr))))))
             efseq))))

;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; Functions related to fitness measurement
;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

(defun get-fitfunc ()
  *fitfunc*)

(defun get-out-reg ()
  *out-reg*)

(defun set-fitfunc (name)
  (case name
    ((binary-1) (setf *fitfunc* #'fitness-binary-classifier-1)
     (setf *out-reg* '(0)))
    ((binary-2) (setf *fitfunc* #'fitness-binary-classifier-2)
     (setf *out-reg* '(0)))
    ((binary-3) (setf *fitfunc* #'fitness-binary-classifier-3)
     (setf *out-reg* '(0 1)))
    ((ternary-1) (setf *fitfunc* #'fitness-ternary-classifier-1)
     (setf *out-reg* '(0 1 2)))
    ((n-ary) (setf *fitfunc* #'fitness-n-ary-classifier)
     (set-out-reg)) ;; sets the out-reg wrt the label-scanner   
    (otherwise (error "FITFUNC NICKNAME NOT RECOGNIZED. MUST BE ONE OF THE FOLLOWING: BINARY-1, BINARY-2, BINARY-3, TERNARY-1."))))

(defun init-fitness-env (&key fitfunc-name training-hashtable testing-hashtable)
  "Works sort of like a constructor, to initialize the fitness 
environment."
  (set-fitfunc fitfunc-name)
  (setf *training-hashtable* training-hashtable)
  (setf *testing-hashtable* testing-hashtable))

;; DO NOT TINKER WITH THIS ANY MORE. 
(defun sigmoid-error(raw goal)
  (let ((divisor 8))
    (flet ((sigmoid (x)
             (tanh (/ x divisor))))
      (/ (abs (+ (sigmoid raw) goal)) 2))))

;; design a meta-gp to evolve the sigmoid function?
(defun fitness-binary-classifier-1 (crt &key (ht *training-hashtable*))
    "Fitness is gauged as the output of a sigmoid function over the
output in register 0 times the labelled value of the input (+1 for
positive, -1 for negative."
  (if (null *out-reg*) (setf *out-reg* '(0)))
  (unless (> 0 (length (creature-eff crt)))
    (setf (creature-eff crt) ;; assuming eff is not set, if fit is not.
          (remove-introns (creature-seq crt) :output '(0))))
  (let ((seq (creature-eff crt)))
    (let ((results
           (loop for pattern
              being the hash-keys in ht collect
                (sigmoid-error
                 (car (execute-sequence seq
                                        :registers *initial-register-state*
                                        :input pattern
                                        :output '(0) )) ;; raw
                 (gethash pattern ht))))) ;; goal
      (and *debug* *verbose*
           (format t "SEQUENCE:~a~%RESULTS:~%~a~%" seq results))
      (/ (reduce #'+ results) (length results)))))

(defun fitness-binary-classifier-2 (crt &key (ht *training-hashtable*))
  (if (null *out-reg*) (setf *out-reg* '(0)))
  (unless (> 0 (length (creature-eff crt)))
    (setf (creature-eff crt) ;; assuming eff is not set, if fit is not.
          (remove-introns (creature-seq crt) :output *out-reg*)))
  (let ((hit 0)
        (miss 0)
        (seq (creature-eff crt)))
    (loop for pattern being the hash-keys in ht
       using (hash-value v) do
         (let ((f ;; shooting in the dark, here. 
                (tanh (/ (car (execute-sequence seq :input pattern
                                                :output *out-reg*)) 300))))
           (if (> (* f v) .5) (incf hit) (incf miss))))
    (if (zerop miss) 1
        (/ hit (+ hit miss)))))

(defun fitness-binary-classifier-3 (crt &key (ht *training-hashtable*))
  "Measures fitness as the proportion of the absolute value
contained in the 'correct' register to the sum of the values in
registers 0 and 1. Setting R0 for negative (-1) and R1 for
positive (+1)."
  (if (null *out-reg*) (setf *out-reg* '(0 1)))
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
             (incf acc1 (divide (deneg (nth (to-idx val) output))
                             (reduce #'+ (mapcar #'deneg output))))
             (incf acc2 (check output (to-idx val)))))

      (setf acc1 (float (/ acc1 (hash-table-count ht))))
      (setf acc2 (float (/ acc2 (hash-table-count ht))))
      (+ (* acc1 w1) (* acc2 w2)))))

;; In Linear Genetic Programming, the author suggests using the
;; sum of two fitness measures as the fitness: 

;; This classifier needs a little bit of tweaking.

;; REFACTOR: use one of the per-case functions
;; try to avoid the abs thing. 
(defun fitness-ternary-classifier-1 (crt &key (ht *training-hashtable*))
  "Where n is the target register, measures fitness as the ratio of
Rn to the sum of all output registers R0-R2 (wrt absolute value)."
  (if (null *out-reg*) (setf *out-reg* '(0 1 2)))
  (unless (> 0 (length (creature-eff crt)))
    (setf (creature-eff crt) ;; assuming eff is not set, if fit is not.
          (remove-introns (creature-seq crt) :output '(0 1 2))))
  (let ((acc 0)
        (seq (creature-eff crt)))
    (loop for pattern being the hash-keys in ht
       using (hash-value i) do
         (let ((output (execute-sequence seq
                                         :input pattern
                                         :output *out-reg*)))
           (incf acc (divide (abs (nth i output))
                          (reduce #'+ (mapcar #'abs output))))))
    (/ acc (hash-table-count *training-hashtable*))))


;; move this up to the fitness section:

;; if lexicase individuals aren't assigned a "fitness score", this may
;; break some parts of the programme -- which should be easy enough to
;; patch, once we see what they are. 

(defun register-vote (output &key (comparator #'>) (pre #'abs))
  "Return the index of the register with the highest or lowest value."
  (reduce #'(lambda (x y) (if (funcall comparator
                                  (funcall pre (nth x output))
                                  (funcall pre (nth y output))) 
                         x
                         y))
          *out-reg*))

;; -- boolean returning per-cases --

(defun per-case-binary (crt case-kv &key (case-storage *case-storage*))
  "Returns a boolean."
  (cond ((gethash (car case-kv) (creature-cas crt))
         t)
        (t
         (let ((output (execute-sequence (creature-eff crt)
                                         :output *out-reg*
                                         :input (car case-kv))))
           ;; the binary table has -1 for no, +1 for yes
           ;; if the output agrees with the value, then the product is +
           (if (< 0 (* (car output) (cdr case-kv)))
               (or (not case-storage)
                   (setf (gethash (car case-kv) (creature-cas crt)) t))
               nil)))))

(defun per-case-n-ary (crt case-kv)
  "Returns a boolean."
  (let ((output (execute-sequence (creature-eff crt)
                                  :output *out-reg*
                                  :input (car case-kv))))
    (= (register-vote output :comparator #'> :pre #'abs) ;; get lowest
       (cdr case-kv))))

;; -- real returning per-cases --

(defun per-case-n-ary-proportional (crt case-kv)
  "Case-kv is a cons of key and value from hashtable."
  (let ((output (execute-sequence (creature-eff crt)
                                  :output *out-reg*
                                  :input (car case-kv))))
    (values (divide (elt output (register-vote output :comparator #'>))
                    (reduce #'+  (mapcar #'abs output))) ;; problem when < 1!
            (if (> (* (abs (nth (cdr case-kv) output))
                      (length output))
                   (reduce #'+ output))
                1
                0))))
         
(defun fitness-n-ary-classifier (crt &key (ht *training-hashtable*))
  "Where n is the target register, measures fitness as the ratio of
Rn to the sum of all output registers R0-R2 (wrt absolute value)."

  (unless (> 0 (length (creature-eff crt)))
    (setf (creature-eff crt) ;; assuming eff is not set, if fit is not.
          (remove-introns (creature-seq crt) :output *out-reg*)))
  (let ((acc1 0)
        (acc2 0)
        (w1 0.4)
        (w2 0.6))
    (loop for pattern being the hash-keys in ht
       using (hash-value i) do
         (multiple-value-bind (a1 a2)
             (per-case-n-ary-proportional crt (cons pattern i))
           (incf acc1 a1)
           (incf acc2 a2)))
    (+ (* w1 (/ acc1 (hash-table-count ht)))
       (* w2 (/ acc2 (hash-table-count ht))))))

(defun fitness (crt &key (ht *training-hashtable*))
  "Measures the fitness of a specimen, according to a specified
fitness function."
  (unless (creature-fit crt)  
    (setf (creature-fit crt)
          (funcall *fitfunc* crt :ht ht))
    (loop for parent in (creature-parents crt) do
         (cond ((> (creature-fit crt) (creature-fit parent))
                (incf (getf *records* :fitter-than-parents)))
               ((< (creature-fit crt) (creature-fit parent))
                (incf (getf *records* :less-fit-than-parents)))
               (t (incf (getf *records* :as-fit-as-parents))))))
  (creature-fit crt))


(defun gauge-accuracy (crt &key (ht *training-hashtable*)
                             (per-case))
  (let ((results))
    (unless per-case
      (setf per-case (if (= (length *out-reg*) 1)
                         #'per-case-binary
                         #'per-case-n-ary)))
    (setf results (loop for k being the hash-keys in ht
                     using (hash-value v)
                     collect (funcall per-case crt (cons k v))))
;;    (print results)
    (/ (reduce #'+ (mapcar #'(lambda (x) (if x 1 0)) results))
       (length results))))

;; there needs to be an option to run the fitness functions with the testing hashtable. 


;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; Pack operations
;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

;; a pack is a list of creatures
;; we'll start with a simple control structure: the car of the pack is
;; the delegator. It delegates the decision to (elt pack (execute-sequence
;; (creature-eff (car pack))) mod (length pack)), which is then executed.
;; obviously, it is possible to develop more complicated control structures,
;; so long as provisions are made to handle the halting problem (a timer, e.g.)
;; but we'll start with these simple constructions of "star packs".

(defun execute-astropack (pack &key (input) (output *out-reg*))
  (let ((delegation-reg (list (1- (expt 2 *destination-register-bits*)))))
    ;; let the delegation task use a distinct register, if available,
    ;; by setting its output register to the highest available R/W reg. 
    (execute-sequence
     (creature-eff
      (elt pack (mod (car
                      (execute-sequence (creature-eff (car pack))
                                        :input input
                                        :output delegation-reg))
                     (length pack))))
     :input input
     :output output)))


(defun pmutate-shuffle (pack)
  (shuffle pack))

(defun pmutate-shuffle-underlings (pack)
  (cons (car pack) (shuffle (cdr pack))))

(defun pmutate-swap-alpha (pack)
  (let ((tmp))
    (setf tmp (car pack))
    (setf (pick (cdr pack)) (car pack))
    (setf (car pack) tmp)
    pack))

(defun pmutate-inbreed (pack)
  (let* ((p0 (pick pack))
         (p1 (if (> (length pack) 2)
                 (pick (remove p0 pack))
                 (random-mutation p0)))
         (cubs))
    (unless (or (null p0) (null p1))
      (setf cubs (mate p0 p1 :sex t)))
    (nsubst p0 (car cubs) pack)
    (nsubst p1 (cdr cubs) pack)    
    pack))

(defun pmutate-alpha (pack)
  (setf #1=(creature-seq (car pack))
          (random-mutation #1#))
  pack)

(defun pmutate-smutate (pack)
  (let ((mutant (pick pack)))
    (setf #1=(creature-seq mutant)
          (random-mutation #1#))
  pack))

(defun pack-mingle (pack1 pack2)
  "Destructively mingles the two packs, preserving alphas and size."
  (let ((underlings (shuffle (concatenate 'list (cdr pack1) (cdr pack2)))))
    (setf pack1 (cons (car pack1)
                      (subseq underlings 0 (length pack1))))
    (setf pack2 (cons (car pack2)
                      (subseq underlings (length pack1))))
    (list pack1 pack2)))


(defun pack-mate (pack1 pack2)
  "Nondestructively mates the two packs, pairwise, producing two new packs."
  (let ((offspring
         (loop
            for p1 in pack1
            for p2 in pack2 collect
              (mate p1 p2))))
    (list (mapcar #'car offspring)
          (mapcar #'cadr offspring))))
    

(defparameter *pack-mutations*
  (vector #'pmutate-smutate #'pmutate-inbreed #'pmutate-swap-alpha
  #'pmutate-shuffle-underlings #'pmutate-alpha))

;; accidental cloning problem...
;; packs seem to collapse into pointers to identical creatures.
;; needs some tinkering. 

(defun random-pack-mutation (pack)
  (setf pack (funcall (pick *pack-mutations*) pack)))
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
  (declare (type cons seq))
  (and *debug* (print "smutate-swap"))
  (let* ((len (length seq))
         (i (random len))
         (j (random len))
         (tmp (elt seq i)))
    (setf (elt seq i) (elt seq i))
    (setf (elt seq j) tmp))
  seq)

(defun smutate-insert (seq)
  "Doubles a random element of seq."
  (declare (type cons seq))
  (let ((idx (random (length seq))))
  (and *debug* (print "smutate-append"))
  (setf seq (concatenate 'list
                         (subseq seq 0 idx)
                         (list (elt seq idx))
                         (subseq seq idx (length seq))))
  seq))

(defun smutate-grow (seq)
  "Doubles a random element of seq."
  (declare (type cons seq))
  (and *debug* (print "smutate-append"))
  (setf seq (concatenate 'list
                         seq
                         (list (random (expt 2 *wordsize*)))))
  seq)

(defun smutate-shrink (seq)
  "Doubles a random element of seq."
  (declare (type cons seq))
  (and *debug* (print "smutate-shrink"))
  (cond ((>= (length seq) (max 3 *min-len*))
         (let* ((tocut (random  (length seq)))
                (newseq (concatenate 'list
                                     (subseq seq 0 tocut)
                                     (subseq seq (1+ tocut)))))
           newseq))
        (t seq)))

           

(defun smutate-imutate (seq)
  "Applies imutate-flip to a random instruction in the sequence."
  (declare (type cons seq))
  (and *debug* (print "smutate-imutate"))
  (let ((idx (random (length seq))))
    (setf (elt seq idx) (imutate-flip (elt seq idx))))
  seq)

(defparameter *mutations*
  (vector #'smutate-insert #'smutate-shrink #'smutate-shrink #'smutate-grow #'smutate-imutate #'smutate-swap))

(defun random-mutation (seq)
  (declare (type cons seq))
  (apply (pick *mutations*) `(,seq)))

(defun maybe-mutate (seq mutation-rate)
  (declare (type cons seq))
  (if (< (random 1.0) mutation-rate)
      (random-mutation seq)
      seq))

(defun shufflefuck (p0 p1)
  "Crossover operator. Works just like it sounds."
  (declare (type creature p0 p1))
  (declare (optimize (speed 1)))

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
 
    (setf (subseq daughter (+ offset minidx) (+ offset maxidx))
          (subseq father minidx maxidx))
    (setf (subseq son minidx maxidx)
          (subseq mother (+ offset minidx) (+ offset maxidx)))         
    ;;    (format t "mother: ~a~%father: ~a~%daughter: ~a~%son: ~a~%"
    ;;            mother father daughter son)
    (setf children (list (make-creature :seq son)
                         (make-creature :seq daughter)))))

(defun metamutate (mut)
  (if (< (random 1.0) *metamutation-rate*)
      (min 1 (max 0
                  (+ mut
                     (* (if (= 0 (random 2)) 1 -1)
                        *metamutation-step*)))))
  mut)

(defun mate (p0 p1 &key (genealogy *track-genealogy*)
                     (output-registers *out-reg*)
                     (sex *sex*))
  (let ((offspring (if sex
                       (shufflefuck p0 p1) ;; sexual reproduction
                       (list (copy-structure p0)
                             (copy-structure p1))))) ;; asexual
                       
    ;; mutation
    (loop for child in offspring do
       ;; now, let the mutation rate be linked to each creature, and
       ;; potentially mutate, itself.
         (setf (creature-cas child) (make-hash-table))
         (setf (creature-mut child) ;; baseline: avg of parents' muts
               (metamutate (/ (+ (creature-mut p0) (creature-mut p1)) 2)))
         (setf (creature-seq child) (maybe-mutate (creature-seq child)
                                                  (creature-mut child)))
         (setf (creature-eff child) (remove-introns
                                     (creature-seq child)
                                     :output output-registers))
         (loop for parent in (list p0 p1) do
              (if (equalp (creature-eff child) (creature-eff parent))
                  (setf (creature-fit child) (creature-fit parent))))
         ;; genealogical records: costly in terms of memory space, but neat
         (when genealogy
           (mapcar #'(lambda (x) (setf (creature-gen x) ;; update gen and parents
                                  (1+ (max (creature-gen p0)
                                           (creature-gen p1)))
                                  (creature-parents x)
                                  (list p0 p1)))
                   offspring)))
    offspring))

;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; Selection functions
;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=


(defun f-lexicase (island
                   &key (ht *training-hashtable*)
                     (per-case #'per-case-n-ary)
                     (case-storage *case-storage*)
                     (combatant-ratio *lexicase-combatant-ratio*))
  "Note: per-case can be any boolean-returning fitness function."
  (flet ((set-eff (crt)
           (unless (creature-eff crt)
             (setf (creature-eff crt) (remove-introns (creature-seq crt)
                                                      :output *out-reg*)))
           crt))
    (let* ((cases (shuffle (loop for k being the hash-keys in ht
                              using (hash-value v) collect (cons k v))))
           (candidates (copy-seq (island-deme island)))
           (next-candidates candidates)
           ;; (total (length cases))
           ;; (highscore 0)
           (worst))
      (mapcar #'set-eff candidates)
      (loop while (and next-candidates cases) do
           (let ((the-case))
             (setf the-case (pop cases))
             (setf candidates next-candidates)
             (setf next-candidates
                   (remove-if-not 
                    #'(lambda (x) (funcall per-case x the-case))
                    candidates))
             (unless worst ;; somewhat better way to get worst
               (setf worst
                     (car (set-difference candidates next-candidates))))))
      (when (null candidates)
        (setf candidates
              (list (elt (island-deme island)
                         (random (length (island-deme island)))))))
      ;; (setf highscore
      ;;       (/ (- total (length cases)) total))
      (values (car candidates)
              worst)))) ;; return just one parent
  
(defun lexicase! (island &key (per-case))
  "Selects parents by lexicase selection." ;; stub, expand
  (multiple-value-bind (best-one worst-one)
      (f-lexicase island :per-case #'per-case-n-ary)
    (multiple-value-bind (best-two worst-two)
        (f-lexicase island :per-case #'per-case-n-ary)
      (update-accuracy-log best-one island)
      (update-accuracy-log best-two island)
      (let ((children (mate best-one best-two
                            :genealogy *track-genealogy*)))
        (unless per-case
          (setf per-case (if (= (length *out-reg*) 1)
                             #'per-case-binary
                             #'per-case-n-ary)))
        (mapcar #'(lambda (x) (setf (creature-home x)
                               (island-id island))) children)
        (nsubst (car children) worst-one (island-deme island)
                :test #'equalp)
        (nsubst (cadr children) worst-two (island-deme island)
                :test #'equalp)
        (mapcar #'bury-cases (list worst-one worst-two))
        children))))

(defun update-accuracy-log (crt island)
  (let ((acc (gauge-accuracy crt :ht *testing-hashtable*)))
    (setf (creature-fit crt) acc)
    (when (> acc (creature-fit (island-best island)))
      (setf (island-best island) crt)
      (funcall (island-logger island) (cons (island-era island)
                                            acc)))))

(defun update-best-if-better (crt island)
  ;; NB: When using lexicase selection, because island-best preserves a
  ;; reference to, and not just a copy of, the island's best, the fitness
  ;; of "best" may perplexingly fluctuate. This is to be expected.
  ;; But since numbers are passed by value, the logger will only record
  ;; the fitness of the island best at the time it was logged.
  (when (and (eq crt (island-best island))
             (/= (creature-fit crt) (cdar (funcall (island-logger island)))))
    (funcall (island-logger island) (cons (island-era island)
                                          (creature-fit crt))))
  (when (> (nil0 (creature-fit crt))
           (nil0 (creature-fit (island-best island))))
    ;; (FORMAT T "~F IS BETTER THAN ~F~%"
    ;;         (creature-fit crt) (creature-fit (island-best island)))
    (setf (island-best island) crt) 
    (funcall (island-logger island) (cons (island-era island) 
                                          (creature-fit crt)))))

(defun tournement2! (island &key (tournement-size 4))
                             
  "Tournement selction function: selects four combatants at random
from the population provide, and lets the two fittest reproduce. Their
children replace the two least fit of the four. Disable genealogy to
facilitate garbage-collection of the dead, at the cost of losing a
genealogical record."  
  (let* ((deme (island-deme island))
          (parents)
          (children)
          (thedead)
          (lots (sort
                 (n-rnd 0 (length deme) :n tournement-size) #'<))
          (lucky (subseq lots 0 2))
          (unlucky (subseq lots (- tournement-size 2))))
     (mapcar #'fitness deme)
     (setf deme (fitsort-deme deme))
     (update-best-if-better (car deme) island)
     (setf parents (mapcar #'(lambda (x) (elt deme x)) lucky))
     (setf thedead (mapcar #'(lambda (x) (elt deme x)) unlucky))
     ;; (FORMAT T "FIT OF PARENTS: ~A~%FIT OF THEDEAD: ~A~%"
     ;;         (mapcar #'creature-fit parents)
     ;;         (mapcar #'creature-fit thedead))
     (setf children (apply #'mate parents))
     (mapcar #'(lambda (crt idx) (setf (elt (island-deme island) idx) crt))
             children unlucky)
     children))


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
    (loop for combatant in combatants do
         (unless (creature-fit combatant)
           (fitness combatant)))
    (let* ((ranked (sort combatants
                         #'(lambda (x y) (< (creature-fit x)
                                       (creature-fit y)))))
           (best-in-show  (car (last ranked)))
           (parents (cddr ranked))
           (children (apply
                      #'(lambda (x y) (mate x y :genealogy genealogy))
                      parents)))
      (update-best-if-better best-in-show island)
      ;; Now replace the dead with the children of the winners
      (mapcar #'(lambda (x) (setf (creature-home x) (island-id island))) children)
      (loop for creature in (concatenate 'list parents children) do
           (push creature population))
      (setf (island-deme island) population)
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
         (popsize *population-size*)
         ;;(popsize (length population))
         (wheel (loop for creature in population
                   collect (progn
                             (let ((f (float (fitness creature))))
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

;; a more generic, and just as efficient, version is in auxiliary.lisp
;; (defun de-ring (island-ring)
;;   (subseq island-ring 0 (island-of (car island-ring))))

(defun extract-seqs-from-island-ring (island-ring)
  "Returns lists of instruction-vectors representing each creature on
each island. Creatures dwelling on the same island share a list."
  (mapcar #'(lambda (x) (mapcar #'(lambda (y) (creature-seq y)) (island-deme x)))
          (de-ring island-ring)))

(defun fitsort-deme (deme &key (fraction 1))
  (flet ((nil0 (n)
           (if (null n) 0 n)))
    (let* ((ratio (if (rationalp fraction) fraction 1/1))
           (population (if (< ratio 1)
                           (shuffle (copy-seq deme))
                           (copy-seq deme)))
           (num (floor (* ratio (length population))))
           (to-sort (subseq deme 0 num))
           (not-to-sort (subseq deme num)))
      (concatenate 'list
                   (sort to-sort
                         #'(lambda (x y) (> (nil0 (creature-fit x))
                                       (nil0 (creature-fit y)))))
                   not-to-sort))))

;; this needs to be mutex protected
(defun reorder-demes (island-ring &key (greedy t))
  "Shuffles the demes of each island in the ring, or fitsorts a
fraction of the deme and places the fittest at the top."
  (loop for isle in (de-ring island-ring) do
       (sb-thread:grab-mutex (island-lock isle))
       (setf (island-deme isle)
             (if greedy
                 (fitsort-deme (island-deme isle) :fraction greedy)
                 (shuffle (island-deme isle))))
       (sb-thread:release-mutex (island-lock isle))))
  
;; now chalk-full of locks. 
(defun migrate (island-ring &key (emigrant-fraction 1/10)
                              (greedy *greedy-migration*))
  "Migrates a randomly-populated percentage of each island to the
preceding island in the island-ring. The demes of each island are
shuffled in the process."
    (let* ((island-pop (length (island-deme (car island-ring))))
           (emigrant-count (floor (* island-pop emigrant-fraction)))
           (buffer))
      (reorder-demes island-ring :greedy greedy)
      (setf buffer (subseq (island-deme (car island-ring)) 0 emigrant-count))
      (loop ;; migrate the emigrants counterclockwise around the island-ring
         for (isle-1 isle-2) on island-ring
         for i from 1 to (1- (island-of (car island-ring))) do
           (sb-thread:grab-mutex (island-lock isle-1))
           (sb-thread:grab-mutex (island-lock isle-2))
           (setf (subseq (island-deme isle-1) 0 emigrant-count)
                 (subseq (island-deme isle-2) 0 emigrant-count))
           (sb-thread:release-mutex (island-lock isle-1))
           (sb-thread:release-mutex (island-lock isle-2)))
      (sb-thread:grab-mutex (island-lock
                     (elt island-ring (1- (island-of (car island-ring))))))
      (setf (subseq (island-deme
                     (elt island-ring (1- (island-of (car island-ring)))))
                    0 emigrant-count) buffer)
      (sb-thread:release-mutex (island-lock
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
                                         :era 0
                                         ;; stave off <,> errors w null crt
                                         :best (make-creature :fit 0)
                                         :logger (make-logger)
                                         :lock (sb-thread:make-mutex :name
                                                (format nil "isle-~d-lock"
                                                        i))))))
    (loop for creature in population do ;; allocate pop to the islands
         (unless (null creature) ;; shouldn't be an issue, but is, so: hack
           (let ((home-isle (mod i number-of-islands)))
             (setf (creature-home creature) home-isle)
             (push creature (island-deme (elt islands home-isle)))
             (incf i))))
    (circular islands))) ;; circular is defined in auxiliary.lisp
;; it forms the list (here, islands) into a circular linked list, by
;; setting the cdr of its last element as a pointer to its car.
             
(defun islands->population (island-ring)
  (apply #'concatenate 'list (mapcar #'(lambda (x) (island-deme x))
                                     (de-ring island-ring))))

(defun spawn-sequence (len)
  (concatenate 'list (loop repeat len collect (random *max-inst*))))

(defun spawn-creature (len)
  (make-creature :seq (spawn-sequence len)
                 :gen 0
                 :cas (make-hash-table)
                 :mut *mutation-rate*))

(defun init-population (popsize slen &key (number-of-islands 4))
  (let ((adjusted-popsize (+ popsize (mod popsize (* 2 number-of-islands)))))
    ;; we need to adjust the population size so that there exists an integer
    ;; number of mating pairs on each island -- so long as we want roulette to
    ;; work without a hitch. 
    (population->islands (loop for i from 0 to (1- adjusted-popsize) collect
                            (spawn-creature (+ *min-len*
                                               (random slen))))
                       number-of-islands)))
;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; User interface functions
;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

(defun setup-data (&key (ratio *training-ratio*)
                     (dataset *dataset*)
                     (selection-method *selection-method*)
                     (fitfunc-name)
                     (filename *data-path*))
  (let ((hashtable)
        (training+testing))
    (setf *method* (case selection-method
                     ((:tournement) #'tournement!)
                     ((:roulette) #'roulette!)
                     ((:greedy-roulette) #'greedy-roulette!)
                     ((:lexicase) #'lexicase!)
                     (otherwise (progn
                                  (format t "WARNING: METHOD NAME NOT RECO")
                                  (format t "GNIZED. USING #'TOURNEMENT!.~%")
                                  #'tournement!))))
    (setf *dataset* dataset)
    (reset-records)
    (funcall =label-scanner= 'flush)
    (case dataset   ;; at some point, I'll have to clean up all of this
      ((:tictactoe) ;; ad-hoc spaghetti code. 
       (unless filename (setf filename *tictactoe-path*))
       (unless fitfunc-name (setf fitfunc-name 'binary-1)))
      (otherwise
       (unless filename (setf filename *iris-path*))
       (unless fitfunc-name (setf fitfunc-name 'n-ary))))
    (setf hashtable (datafile->hashtable :filename filename))
    (when *testing-data-path*
      (setf training+testing (cons hashtable
                                   (datafile->hashtable
                                    :filename *testing-data-path*))))
    (unless *split-data*
      (setf training+testing (cons hashtable hashtable)))
    (unless training+testing
      (setf training+testing (partition-data hashtable ratio)))
    (init-fitness-env :training-hashtable (car training+testing)
                      :testing-hashtable  (cdr training+testing)
                      :fitfunc-name fitfunc-name)
    training+testing))
;; note: I should have a separate label-scanner object for the
;; testing and training sets, if they come from different files.
;; copies, otherwise. right now, the same object is used, which
;; is fine if the testing and training sets have the same attributes
;; and classes, but which could lead to obsure bugs otherwise.  

;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; Runners
;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

(defun best-of-all (island-ring)
  (let ((best-so-far (make-creature :fit 0)))
    (loop for isle in (de-ring island-ring) do
         (if (> (nil0 (creature-fit (island-best isle)))
                (creature-fit best-so-far))
             (setf best-so-far (island-best isle))))
    ;; you could insert a validation routine here
    (setf *best* best-so-far)
    best-so-far))

(defun release-all-locks (island-ring)
  "Mostly for running from the REPL after aborting the programme."
  (sb-thread:release-mutex -migration-lock-)
  (loop for island in (de-ring island-ring) do
       (sb-thread:release-mutex (island-lock island))))

(defun poll-for-break ()
  (loop while (null *STOP*) do
       (when (eql #\k (read-char-no-hang))
         (setf *STOP* t))))



(defun make-stopwatch ()
  (let ((start-time 0))
    (lambda (&optional arg)
      (case arg
        ((set) (setf start-time (get-universal-time)))
        (otherwise (- (get-universal-time) start-time))))))

(defun evolve (&key (method *method*) (dataset *dataset*)
                 (rounds *rounds*) (target *target*)
                 (stat-interval *stat-interval*) (island-ring +island-ring+) 
                 (migration-rate *migration-rate*)
                 (migration-size *migration-size*)
                 (parallelize *parallel*))
  ;; adjustments needed to add fitfunc param here. 
  (setf *STOP* nil)
  (setf *parallel* parallelize)
  (let ((stopwatch (make-stopwatch))
        (timereport "")
        (correct+incorrect))
    (funcall stopwatch 'set)
  ;;; Just putting this here temporarily:
    (setf timereport
          (with-output-to-string (*trace-output*)
            (time (block evolver
                    (loop for i from 1 to (* rounds *number-of-islands*) do
                         (let ((isle (pop island-ring)))
                           ;; island-ring is circular, so pop
                           ;; will cycle & not exhaust it
                           (labels ((time-for (interval)
                                      (= 0 (mod (/ i *number-of-islands*) interval)))
                                    (dispatcher ()
                                      ;; we don't want more than one thread per island.
                                      (when *parallel*
                                        (sb-thread:grab-mutex (island-lock isle)))
                              (funcall method isle)
                              (when *parallel*
                                (sb-thread:release-mutex (island-lock isle))))
                                    (dispatch ()
                                      (incf (island-era isle))
                                      (if *parallel*
                                          (sb-thread:make-thread #'dispatcher)
                                          (dispatcher))))
                             (dispatch)
                             (when (equalp (read-char-no-hang) #\k)
                               (setf *stop* t))
                             (when (time-for migration-rate)
                               (when *parallel*
                                 (sb-thread:grab-mutex -migration-lock-))
                               (princ ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>")
                               (princ " MIGRATION EVENT ")                       
                               (migrate island-ring :emigrant-fraction migration-size)
                               (format t "<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<~%")
                               (when *parallel*
                                 (sb-thread:release-mutex -migration-lock-)))
                             (when (time-for stat-interval)
                               (print-statistics island-ring)
                               (best-of-all +island-ring+)
                               (format t "BEST: ~F ON ISLE ~A  *  ~D SEC. INTO ~A ON ~A~%"
                                       (creature-fit *best*)
                                       (roman (creature-home *best*))
                                       (funcall stopwatch)
                                       (func->string method)
                                       *dataset*)
                               ;; (when (eq *selection-method* :LEXICASE)
                               ;;   (format t "ACCURACY: ~F~%"
                               ;;           (gauge-accuracy *best*
                               ;;                           :ht *testing-hashtable*)))
                               (hrule))
                             (when (or (> (creature-fit (island-best isle))
                                          target)
                                       *STOP*)
                               (format t "~%TARGET OF ~f REACHED AFTER ~d ROUNDS~%"
                                       target i)
                               (return-from evolver)))))))))
          (print-statistics +island-ring+)
          (best-of-all island-ring)
          ;;        (print-creature (best-of-all island-ring))
          (setf correct+incorrect (classification-report *best* dataset))
          (loop for isle in (de-ring island-ring) do
               (plot-fitness isle))
          (when *track-genealogy* (genealogical-fitness-stats))
          (hrule)
          (when (not (zerop (creature-fit *best*)))
            (format t "                         -oO( BEST SPECIMEN )Oo-~%")
            (hrule)
            (format t "~D TESTS CLASSIFIED CORRECTLY, ~D INCORRECTLY.~%"
                    (car correct+incorrect) (cdr correct+incorrect))
            (hrule)
            (print-creature *best*))
            ;; (format t "BEST FITNESS IS ~F~%ACHIEVED BY A GENERATION ~D CREATURE, FROM ISLAND ~A, AT ERA ~D~%WITH SEQUENCE:~%~%"
            ;;         (creature-fit *best*)
            ;;         (creature-gen *best*)
            ;;         (roman (creature-home *best*))
            ;;         (island-era (elt +island-ring+ (creature-home *best*))))
;;            (disassemble-sequence (creature-seq *best*) :static t))
          (format t "~%~A" timereport))
    (hrule))
          
;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; Debugging functions and information output
;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=


(defun disassemble-sequence (seq &key (registers *initial-register-state*)
                                   (input *default-input-reg*)
                                   (static nil))
  (if static (disassemble-history :static seq)
      (execute-sequence seq :debug t :input input :registers registers)))


(defun print-params ()
  "Prints major global parameters, and some statistics, too."
  (hrule)
  (format t "[+] VIRTUAL MACHINE:      ~A~%" *machine*)
  (format t "[+] INSTRUCTION SIZE:     ~d bits~%" *wordsize*)
  (format t "[+] # of RW REGISTERS:    ~d~%"
          (expt 2 *destination-register-bits*))
  (format t "[+] # of RO REGISTERS:    ~d~%" (expt 2 *source-register-bits*))
  (format t "[+] PRIMITIVE OPERATIONS: ")
  (loop for i from 0 to (1- (expt 2 *opcode-bits*)) do
       (format t "~a " (func->string (elt *operations* i))))
  (format t "~%[+] POPULATION SIZE:      ~d~%"
          *population-size*)
  (format t "[+] NUMBER OF ISLANDS:    ~d~%" (island-of (car +island-ring+)))
  (format t "[+] MIGRATION RATE:       ONCE EVERY ~D CYCLES~%" *migration-rate*)
  (format t "[+] MIGRATION SIZE:       ~D~%" *migration-size*)
  (format t "[+] MIGRATION ELITISM:    ~D~%" *greedy-migration*)
  (format t "[+] REPRODUCTION:         ~A~%"
          (if *sex* :sexual :asexual))
  (format t "[+] MUTATION RATE:        ~d~%" *mutation-rate*)
  (format t "[+] METAMUTATION RATE:    ~d~%" *metamutation-rate*)
  (format t "[+] MAX SEQUENCE LENGTH:  ~d~%" *max-len*)
  (format t "[+] MAX STARTING LENGTH:  ~d~%" *max-start-len*)
  (format t "[+] # of TRAINING CASES:  ~d~%"
          (hash-table-count *training-hashtable*))
  (format t "[+] # of TEST CASES:      ~d~%"
          (hash-table-count *testing-hashtable*))
;;  (format t "[+] FITNESS FUNCTION:     ~s~%" (get-fitfunc))
  (format t "[+] SELECTION FUNCTION:   ~s~%" *method*)
  (format t "[+] OUTPUT REGISTERS:     ~a~%" *out-reg*)
  (hrule))

(defun likeness-to-specimen (population specimen)
  "A weak, but often informative, likeness gauge. Assumes gene alignment,
for the sake of a quick algorithm that can be dispatched at runtime
without incurring delays."
  (float (divide (reduce #'+
                      (mapcar #'(lambda (x) (likeness (creature-eff specimen)
                                                 (creature-eff x)))
                              (remove-if
                               #'(lambda (x) (or (null x)
                                            (equalp #() (creature-eff x))))
                               population)))
              (length population))))

(defun print-fitness-by-gen (logger)
  (flet ((.*. (x y)
           (if (numberp y)
               (* x y)
               NIL)))
  (let ((l (funcall logger)))
    (loop
       for (e1 e2) on l by #'cddr
       for i from 1 to 5 do
         (format t "~c~d:~c~5,4f %~c~c~d:~c~5,4f %~%" #\tab
                 (car e1) #\tab (.*. 100 (cdr e1)) #\tab #\tab
                 (car e2) #\tab (.*. 100 (cdr e2)))))))

(defun opcode-census (population)
  (let* ((buckets (make-array (expt 2 *opcode-bits*)))
         (instructions (reduce #'(lambda (x y) (concatenate 'list x y))
                               (mapcar #'creature-eff population)))
         (sum (length instructions)))
    (loop for inst in instructions do
         (incf (elt buckets (ldb (byte *opcode-bits* 0) inst))))
    (loop repeat (/ (length buckets) 2)
       for x = 0 then (+ x 2)
       for y = 1 then (+ y 2) do
         (format t "~C~A: ~4D~C(~5,2f %)~C" #\tab
                 (func->string (elt *operations* x))
                 (elt buckets x)
                 #\tab
                 (* 100 (divide (elt buckets x) sum)) #\tab)
         (format t "~C~A: ~4D~c(~5,2f %)~%" #\tab
                 (func->string (elt *operations* y))
                 (elt buckets y)
                 #\tab
                 (* 100 (divide (elt buckets y) sum))))))

(defun percent-effective (crt &key (out *out-reg*))
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
  (format t "              *** STATISTICS FOR ISLAND ~A AT ERA ~D ***~%"
          (roman (island-id island))
          (island-era island))
  (hrule)
  (format t "[*] BEST FITNESS SCORE ACHIEVED ON ISLAND: ~5,4f %~%"
          (* 100 (creature-fit (island-best island))))
  (format t "[*] AVERAGE FITNESS ON ISLAND: ~5,2f %~%"
          (* 100 (divide (reduce #'+
                                 (remove-if #'null
                                            (mapcar #'creature-fit
                                                    (island-deme island))))
                         (length (island-deme island)))))
  (format t "[*] BEST FITNESS BY GENERATION:  ~%")
  (print-fitness-by-gen (island-logger island))
  (format t "[*] AVERAGE SIMILARITY TO BEST:  ~5,2f %~%"
          (* 100 (likeness-to-specimen (island-deme island) (island-best island))))
  (format t "[*] STRUCTURAL INTRON FREQUENCY: ~5,2f %~%"
          (* 100 (- 1 (average-effective (island-deme island)))))

  (format t "[*] AVERAGE LENGTH: ~5,2f instructions (~5,2f effective)~%"
          (divide (reduce #'+ (mapcar #'(lambda (x) (length (creature-seq x)))
                                 (island-deme island)))
             (length (island-deme island)))
          (divide (reduce #'+ (mapcar #'(lambda (x) (length (creature-eff x)))
                                 (island-deme island)))
             (length (remove-if #'(lambda (x) (equalp #() (creature-eff x)))
                                (island-deme island)))))
  (format t "[*] EFFECTIVE OPCODE CENSUS:~%")
  (opcode-census (island-deme island)))

(defun plot-fitness (island)
  (hrule)
  (format t "           PLOT OF BEST FITNESS OVER GENERATIONS FOR ISLAND ~A~%"
          (roman (island-id island)))
  (hrule)
  (let* ((fitlog (funcall (island-logger island)))
         (lastgen (caar fitlog))
         (row #\newline)
         (divisor 35)
         (interval (max 1 (floor (divide (nil0 lastgen) divisor))))
         (scale 128)
         (bar-char #\X))
    ;;         (end-char #\x))
    (format t "~A~%" fitlog)
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
      (dotimes (i (floor (divide scale 2)))
        (if (= (safemod i (floor (divide scale 10.5))) 0)
            (progn 
              (format t "~d" x-axis)
              (when (>= x-axis 100) (return))
              (incf x-axis 10))
            (format t " "))))
    (format t "~%")
    (hrule)))

(defun avg-mutation-rate (island)
  (divide (reduce #'+ (mapcar #'creature-mut (island-deme island)))
          (length (island-deme island))))

(defun print-creature (crt)
  (flet ((nil0 (n)
           (if (null n) 0 n)))
    (format t "FIT: ~F~%SEQ: ~A~%EFF: ~A~%MUT: ~F~%HOME: ISLAND #~D~%GEN: ~D~%"
            (nil0 (creature-fit crt))
            (creature-seq crt)
            (creature-eff crt)
            (creature-mut crt)
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
                 (* 100 (divide stat sum))))))

(defun print-new-best-update (island)
  ;;  (hrule)
  (sb-thread:grab-mutex -print-lock-)
  (format t "****************** NEW BEST ON ISLAND #~d AT GENERATION ~d ******************~%"
          (island-id island) (island-era island))
  (print-creature (island-best island))
  (sb-thread:release-mutex -print-lock-))
  ;;(format t "*****************************************************************************~%"))
          
(defun classification-report (crt dataset &key (testing t)
                                            (ht *testing-hashtable*))
  (if testing
      (setf ht *testing-hashtable*)
      (setf ht *training-hashtable*))
  (case dataset
    (:tictactoe (ttt-classification-report :crt crt :ht ht :out *out-reg*))
    (otherwise (data-classification-report :crt crt :ht ht :out *out-reg*))))
;;; the new generic report form should handle any iris-like data -- any with
;;; numerical fields, and string labels. 

;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; extra
;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

;; for testing in the REPL:
(defun grab-specimen (&key island)
  (let* ((isle (if island island
                   (elt +island-ring+ (random *number-of-islands*))))
        (crt (elt (island-deme isle) (random (length (island-deme isle))))))
    crt))

(defun params-for-shuttle ()
  (setf *parallel* nil)
  (setf *rounds* 10000)
  (setf *target* 1)
  (setf *track-genealogy* nil)
  (setf *stat-interval* 100)
  (setf *dataset* :shuttle)
  (setf *data-path* #p"~/Projects/genlin/datasets/shuttle/shuttle.csv")
  (setf *destination-register-bits* 3)
  (setf *source-register-bits* 4)
  (setf *selection-method* :lexicase))

;;(dbg)
;;(setf *stop* t)
;; a new file for output funcs?
;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;;

(defun save-island-ring (island-ring filename)
  (let ((copy (de-ring island-ring)))
    (mapcar #'(lambda (x) (setf (island-lock x) 'mutex-unreadable
                           (island-logger x) 'logger-unreadable))
            copy)
    (with-open-file (stream filename :direction :output)
      (format stream "~A~%~%;; tweakable parameters are:~%'(" (timestring))
      (loop for tweak in *tweakables* do
           (format stream "~S ~S~%" tweak (symbol-value tweak)))
      (format stream ")~%~%;; +ISLAND-RING+ below: ~%~%~S" +ISLAND-RING+))))

(defun restore-island-ring (filename)
  ;; todo
  )

;; Note: it should be simple enough to generalize the ttt data processing
;; technique.
;; - scan the dataset
;; - count the possible values for each field, and if there are an equal
;;   number of possibilities for each field, say n, formalize the key as
;;   an m-digit base-n gray code integer.
;; - this may, in some cases, even work when there is a maximum number
;;   of possibilities per field. or if each field can have any of n
;;   values, when unconstrained by other fields (the mutual constraints,
;;   of course, are an impt aspect of the pattern that the algo's meant
;;   to detect). 


(defun bury (crt)
  (bury-cases crt)
  (bury-parents crt))

(defun bury-cases (crt)
  "Free some memory when burying a creature."
  (setf (creature-cas crt) nil))

(defun bury-parents (crt)
  (setf (creature-parents crt) nil))
;; use tp, fp?
;;(dbg)

(setf *parallel* nil)
