;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;;   ___ ___ _  _ _    ___ _  _
;;  / __| __| \| | |  |_ _| \| |
;; | (_ | _|| .` | |__ | || .` |
;;  \___|___|_|\_|____|___|_|\_| Linear Genetic Programming Engine
;;
;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; Execution: interface between genetic components and virtual machine
;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

(defun execute-creature (crt &key (input) (output *out-reg*) (debug nil))
  (case (creature-typ crt)
    ((:alpha) (execute-pack crt
                            :input input
                            :output output
                            :debug debug))
    (otherwise 
     (unless (creature-eff crt)
       (setf (creature-eff crt)
             (remove-introns (creature-seq crt))))
     (execute-sequence (creature-eff crt)
                       :stack ()
                       :input input :output output :debug debug))))


(defun execute-pack (alpha-crt &key (input) (output *out-reg*)
                                 (debug *debug*))
  (let ((pack-reg ;; should store as a constant. 
         (loop for i from 0 to (1- (length (creature-pack alpha-crt)))
            collect i))
        (choice))
    ;; let the delegation task use a distinct register, if available,
    ;; by setting its output register to the highest available R/W reg. 
    (unless (creature-eff alpha-crt)
      (setf (creature-eff alpha-crt)
            (remove-introns (creature-seq alpha-crt) :output pack-reg)))
    (assert input)
    (setf choice (register-vote
                  (execute-sequence
                   (creature-eff alpha-crt)
                   :debug debug
                   :input input
                   :output pack-reg)))
    (when debug
      (format t "~%<<<| DELEGATING TASK TO UNDERLING ~D |>>>~%~%" choice))

    (execute-sequence
     (creature-eff (elt (creature-pack alpha-crt)
                        choice))
     :debug debug
     :input input
     :output output)))


;; (defun execute-queen (queen  &key input
;;                                hive
;;                                (worker-output *out-reg*)
;;                                (queen-output '(0))
;;                                (debug nil))
;;   (unless (creature-eff queen)
;;     (setf (creature-eff queen)
;;           (remove-introns (creature-seq queen) :output queen-output)))
;;   (execute-sequence ;; workers should already have their effs sorted out
;;    (creature-eff (elt (island-deme hive)
;;                       (mod (abs (floor
;;                                  (car
;;                                   (execute-sequence (creature-eff queen)
;;                                                     :input input
;;                                                     :output queen-output
;;                                                     :debug debug
;;                                                     :stack (creature-seq queen)))))
;;                            (length (island-deme hive))))))
;;   :input input
;;   :output worker-output
;;   :debug debug
;;   :stack (creature-seq queen))
;; no good. underlings are a totally chaotic arrangement.
;; far too sensitive to noise. different approach needed. packs.


;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; debugging functions
;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

(defun dbg (&optional on-off)
  (case on-off
    ((on)  (setf *debug* t))
    ((off) (setf *debug* nil))
    (otherwise (setf *debug* (not *debug*)))))

;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; Functions related to fitness measurement
;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

(defun get-fitfunc ()
  *fitfunc*)

(defun get-out-reg ()
  *out-reg*)

(defun set-fitfunc (&optional (name *fitfunc-name*))
  (print *fitfunc-name*)
  (print name)
  (setf *fitfunc*
        (case name
          ((:n-ary-prop-vote) #'fitness-n-ary-classifier)
          ((:detection-rate)  #'fitness-dr)
          ((:accuracy) #'fitness-acc)
          ((:avg-acc-dr) #'fitness-avg-acc-dr)
          (otherwise (error "FITFUNC NICKNAME NOT RECOGNIZED. MUST BE ONE OF THE FOLLOWING: :N-ARY-PROP-VOTE, :DETECTION-RATE, :ACCURACY, :AVG-ACC-DR.")))))

(defun init-fitness-env (&key training-hashtable testing-hashtable)
  "Works sort of like a constructor, to initialize the fitness 
environment."
  (set-out-reg)
  (set-fitfunc)
  (setf *training-hashtable* training-hashtable)
  (setf *testing-hashtable* testing-hashtable))

;; DO NOT TINKER WITH THIS ANY MORE. 
(defun sigmoid-error(raw goal)
  (let ((divisor 8))
    (flet ((sigmoid (x)
             (tanh (/ x divisor))))
      (/ (abs (+ (sigmoid raw) goal)) 2))))

;; design a meta-gp to evolve the sigmoid function?
(defun fitness-binary-classifier-1 (crt &key island (ht *training-hashtable*))
    "Fitness is gauged as the output of a sigmoid function over the
output in register 0 times the labelled value of the input (+1 for
positive, -1 for negative."
    (declare (ignore island))
    (if (null *out-reg*) (setf *out-reg* '(0)))
    ;; (unless (> 0 (length (creature-eff crt)))
    ;;   (setf (creature-eff crt) ;; assuming eff is not set, if fit is not.
    ;;         (remove-introns (creature-seq crt) :output '(0))))
    (let ((results
           (loop for pattern
              being the hash-keys in ht collect
                (sigmoid-error
                 (car (execute-creature crt
                                        :input pattern
                                        :output '(0) )) ;; raw
                 (gethash pattern ht))))) ;; goal
      (/ (reduce #'+ results) (length results))))

(defun fitness-binary-classifier-2 (crt &key island (ht *training-hashtable*))
  (declare (ignore island))
  (if (null *out-reg*) (setf *out-reg* '(0)))
  ;; (unless (> 0 (length (creature-eff crt)))
  ;;   (setf (creature-eff crt) ;; assuming eff is not set, if fit is not.
  ;;         (remove-introns (creature-seq crt) :output *out-reg*)))
  (let ((hit 0)
        (miss 0))
    (loop for pattern being the hash-keys in ht
       using (hash-value v) do
         (let ((f ;; shooting in the dark, here. 
                (tanh (/ (car (execute-creature crt
                                                :input pattern
                                                :output *out-reg*)) 300))))
           (if (> (* f v) .5) (incf hit) (incf miss))))
    (if (zerop miss) 1
        (/ hit (+ hit miss)))))

(defun fitness-binary-classifier-3 (crt &key island (ht *training-hashtable*))
  "Measures fitness as the proportion of the absolute value
contained in the 'correct' register to the sum of the values in
registers 0 and 1. Setting R0 for negative (-1) and R1 for
positive (+1)."
  (declare (ignore island))
  (if (null *out-reg*) (setf *out-reg* '(0 1)))
  ;; (unless (> 0 (length (creature-eff crt)))
  ;;   (setf (creature-eff crt) ;; assuming eff is not set, if fit is not.
  ;;         (remove-introns (creature-seq crt) :output '(0 1 2))))
  (fitness-n-ary-classifier crt
                            :val-transform #'(lambda (x) (if (< x 0) 0 1))
                            :ht ht
                            :island island))

  
;; In Linear Genetic Programming, the author suggests using the
;; sum of two fitness measures as the fitness: 

;; This classifier needs a little bit of tweaking.

;; REFACTOR: use one of the per-case functions
;; try to avoid the abs thing. 
(defun fitness-ternary-classifier-1 (crt &key (ht *training-hashtable*) island)
  "Where n is the target register, measures fitness as the ratio of
Rn to the sum of all output registers R0-R2 (wrt absolute value)."
  (declare (ignore island))
  (if (null *out-reg*) (setf *out-reg* '(0 1 2)))
  ;; (unless (> 0 (length (creature-eff crt)))
  ;;   (setf (creature-eff crt) ;; assuming eff is not set, if fit is not.
  ;;         (remove-introns (creature-seq crt) :output '(0 1 2))))
  (let ((acc 0))
    (loop for pattern being the hash-keys in ht
       using (hash-value i) do
         (let ((output (execute-creature crt
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
  (assert output)
  (reduce #'(lambda (x y) (if (funcall comparator
                                  (funcall pre (nth x output))
                                  (funcall pre (nth y output))) 
                         x
                         y))
          *out-reg*))

;; -- boolean returning per-cases --

(defun per-case-binary (crt case-kv &optional island)
  "Returns a boolean."
  (declare (ignorable island))
  (cond ((and *case-storage* (solved? (car case-kv) crt island))
         t)
        (:DEFAULT
         (let ((output (execute-creature crt
                                         :output *out-reg*
                                         :input (car case-kv))))
           (cond ((> (* (car output) (cdr case-kv)) 0)
                  T)
                 (:DEFAULT NIL))))))

(defun per-case-n-ary (crt case-kv &optional island)
  "Returns a boolean."
  (declare (ignorable island))
  (if (and *case-storage* (solved? (car case-kv) crt island))
      t
      (= (cdr case-kv) (register-vote (execute-creature crt
                                                        :output *out-reg*
                                                        :input (car case-kv))
                                      :comparator #'> :pre #'abs))))


;; -- real returning per-cases --

(defun per-case-n-ary-proportional (crt case-kv  &key (island)
                                                   (out *out-reg*))
  "Case-kv is a cons of key and value from hashtable."
  (declare (ignorable island))
  (cond ((solved? (car case-kv) crt island)
         1)
        (t
         (let ((output (execute-creature crt
                                         :output out
                                         :input (car case-kv))))
           (values (divide (elt output (register-vote output :comparator #'>))
                           (reduce #'+  (mapcar #'abs output))) 
                   (cond ((> (* (abs (nth (cdr case-kv) output))
                                (length output))
                             (reduce #'+ output))
                          1)
                         (:DEFAULT 0)))))))

(defun fitness-avg-acc-dr (crt &key (ht *training-hashtable*))
  (unless (creature-cm crt)
    (compute-cmatrix crt :ht ht)) ;; updates cm fields in crt
  (avg (cmatrix->accuracy (creature-cm crt))
       (cmatrix->detection-rate (creature-cm crt))))

(defun fitness-dr (crt &key (ht *training-hashtable*))
  (unless (creature-cm crt)
    (compute-cmatrix crt :ht ht))
  (cmatrix->detection-rate (creature-cm crt)))

(defun fitness-acc (crt &key (ht *training-hashtable*))
  (unless (creature-cm crt)
    (compute-cmatrix crt :ht ht))
  (cmatrix->accuracy (creature-cm crt)))
                                 
(defun fitness-n-ary-classifier (crt &key
                                       (ht *training-hashtable*)
                                       (val-transform #'(lambda (x) x)))
                    ;;                   (sharing t))
  "Where n is the target register, measures fitness as the ratio of
Rn to the sum of all output registers R0-R2 (wrt absolute value)."

  ;; (unless (> 0 (length (creature-eff crt)))
  ;;   (setf (creature-eff crt) ;; assuming eff is not set, if fit is not.
  ;;         (remove-introns (creature-seq crt) :output *out-reg*)))
  (let ((acc1 0) ;; proportional
        (acc2 0) ;; first-past-the-post
        (w1 0.4)
        (w2 0.6))
    (loop for pattern being the hash-keys in ht
       using (hash-value i) do
         (multiple-value-bind (a1 a2)
             (per-case-n-ary-proportional crt
                                          (cons pattern
                                                (funcall val-transform i)))              
           ;; could introduce fitness sharing here. 
           (incf acc1 a1)
           (incf acc2 a2)))
    (+ (* w1 (/ acc1 (hash-table-count ht)))
       (* w2 (/ acc2 (hash-table-count ht))))))

(defun fitness (crt &key
                      (ht *training-hashtable*)
                      (fitfunc *fitfunc*))
  "Measures the fitness of a specimen, according to a specified
fitness function."
  (unless (creature-fit crt)  
    (setf (creature-fit crt)
          (funcall fitfunc crt  :ht ht))
    (loop for parent in (creature-parents crt) do
         (cond ((> (creature-fit crt) (creature-fit parent))
                (incf (getf *records* :fitter-than-parents)))
               ((< (creature-fit crt) (creature-fit parent))
                (incf (getf *records* :less-fit-than-parents)))
               (t (incf (getf *records* :as-fit-as-parents))))))
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

(defun smutate-comp (seq)
  ;; smutate-insert with bias towards complementary instructions
  (let ((shuffle-ops (shuffle (copy-seq *complementary-ops*))))
    (flet ((rnd-inst-w-op (op)
             (logior (ash (random
                           (expt 2 (- *wordsize* *opcode-bits*)))
                        *opcode-bits*)
                     (op->opcode op))))
      (loop for idx from 0 to (1- (length seq)) do
           (let ((inst.pair nil))
             (cond ((setf inst.pair (assoc (op? (elt seq idx)) shuffle-ops))
                    (if (> idx 0)
                        (setf (elt seq (random idx))
                              (rnd-inst-w-op (cdr inst.pair)))
                        (setf seq (concatenate 'vector
                                               (vector (rnd-inst-w-op
                                                        (cdr inst.pair)))
                                               seq)))
                    (return))
                   ((setf inst.pair (rassoc (op? (elt seq idx)) shuffle-ops))
                    (if (= idx (1- (length seq)))
                        (setf (elt seq (+ idx (random (- (length seq) idx))))
                              (rnd-inst-w-op (car inst.pair)))
                        (setf seq (concatenate 'vector seq
                                               (vector (rnd-inst-w-op
                                                        (car inst.pair))))))
                    (return))
                   (t nil))))
      seq)))

(defun smutate-swap (seq)
  "Exchanges the position of two instructions in a sequence."
  (declare (type (simple-array integer) seq))
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
  (declare (type (simple-array integer) seq))
  (let ((idx (random (length seq))))
  (and *debug* (print "smutate-append"))
  (setf seq (concatenate 'vector
                         (subseq seq 0 idx)
                         (vector (elt seq idx))
                         (subseq seq idx (length seq))))
  seq))

(defun smutate-grow (seq)
  "Doubles a random element of seq."
  (declare (type (simple-array integer) seq))
  (and *debug* (print "smutate-append"))
  (setf seq (concatenate 'vector
                         seq
                         (vector (random (expt 2 *wordsize*)))))
  seq)

(defun smutate-shrink (seq)
  "Removes a random element of seq."
  (declare (type (simple-array integer) seq))
  (and *debug* (print "smutate-shrink"))
  (cond ((>= (length seq) (max 3 *min-len*))
         (let* ((tocut (random  (length seq)))
                (newseq (concatenate 'vector
                                     (subseq seq 0 tocut)
                                     (subseq seq (1+ tocut)))))
           newseq))
        (t seq)))

(defun smutate-imutate (seq)
  "Applies imutate-flip to a random instruction in the sequence."
  (declare (type (simple-array integer) seq))
  (and *debug* (print "smutate-imutate"))
  (let ((idx (random (length seq))))
    (setf (elt seq idx) (imutate-flip (elt seq idx))))
  seq)

(defparameter *mutations*
  (vector #'smutate-insert #'smutate-shrink #'smutate-imutate))
         

(defparameter *rare-mutations*
  (vector #'smutate-swap #'smutate-comp))

(defun random-mutation (seq)
  (declare (type (simple-array integer) seq))
  (let ((mutation (if (< (random 1.0) 0.2)
                      (pick *rare-mutations*)
                      (pick *mutations*))))
    (funcall mutation seq)))

(defun maybe-mutate (seq mutation-rate)
  (declare (type (simple-array integer) seq))
  (if (< (random 1.0) mutation-rate)
      (random-mutation seq)
      seq))

(defun shufflefuck-1pt (p0 p1)
  "Crossover operator. Works just like it sounds."
  ;; just replaced the old, two point, zero-growth crossover
  ;; with one-point crossover that allows growth. 
  (declare (type creature p0 p1))
  (declare (optimize (speed 1)))

  (let* ((mates (shuffle (list p0 p1)))
         (mother (creature-seq (car mates)))
         (father (creature-seq (cadr mates)))
         (idx0 (random (length mother)))
         (idx1 (random (length father)))
         (children))
    (push
     (concatenate 'vector
                  (subseq mother 0 idx0)
                  (subseq father idx1))
     children)
    (push
     (concatenate 'vector
                  (subseq father 0 idx1)
                  (subseq mother idx0))
     children)                       
    (mapcar #'(lambda (genome) (make-creature
                           :seq (subseq genome 0 (min (length genome)
                                                      *max-len*))))
            children)))


(defun shufflefuck-2pt-constant (p0 p1)
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


     ;; run remove-intron naively, but (a) pass seq as stack to execute-sequence and (b) have instructions like LOD, STO, etc. operate on stack?
     
(defun metamutate (mut)
  (if (< (random 1.0) *metamutation-rate*)
      (min 1 (max 0
                  (+ mut
                     (* (if (= 0 (random 2)) 1 -1)
                        *metamutation-step*)))))
  mut)

(defun clone (p0 p1)
  (list
   (make-creature :seq (copy-seq (creature-seq p0)))
   (make-creature :seq (copy-seq (creature-seq p1)))))

(defun pack-mingle (alpha1 alpha2)
  "Destructively mingles the two packs, preserving alphas and size."
  (let ((underlings (shuffle (concatenate 'list
                                          (creature-pack alpha1)
                                          (creature-pack alpha2))))
        (num (length (creature-pack alpha1))))
    (setf (creature-pack alpha1) (subseq underlings 0 num)
          (creature-pack alpha2) (subseq underlings num))))

(defun mate (p0 p1 &key (island nil)
                     (genealogy *track-genealogy*)
                     (output-registers *out-reg*)
                     (sex *sex*)
                     (mingle-rate *mingle-rate*))
  (declare (ignorable island))
  (let* ((mating-func (case sex
                        ((:2pt) #'shufflefuck-2pt-constant)
                        ((:1pt) #'shufflefuck-1pt)
                        (otherwise #'clone)))
         (offspring (funcall mating-func p0 p1))) ;; sexual reproduction

    ;; PACK MINGLING
    (when (and (eq :alpha (creature-typ p0))
               (eq :alpha (creature-typ p1)))
               ;;(< (random 1.0) mingle-rate))
      (setf (creature-pack (car offspring)) (creature-pack p0)
            (creature-pack (cadr offspring)) (creature-pack p1))
      (when (and (< (random 1.0) mingle-rate))
        ;; (< (pack-coverage p0 island) 1)
        ;; (< (pack-coverage p1 island) 1)
        (apply #'pack-mingle offspring))
      ;; let there be a chance for each underling to continue to
      ;; mutate, but divide this chance among the underlings, to
      ;; reduce step size. 
      (loop for cub in offspring do
           (loop for underling in (creature-pack cub) do
                (setf (creature-seq underling)
                      (maybe-mutate (creature-seq underling)
                                    (/ (creature-mut underling)
                                       (length (creature-pack cub))))))))
    ;; inheritance of attributes
;;    (when sex
      (loop for child in offspring do
         ;; now, let the mutation rate be linked to each creature, and
         ;; potentially mutate, itself.
           (setf (creature-mut child) ;; baseline: avg of parents' muts
                 (metamutate (/ (+ (creature-mut p0) (creature-mut p1)) 2)))
           (setf (creature-seq child) (maybe-mutate (creature-seq child)
                                                    (creature-mut child)))
           (setf (creature-typ child) (creature-typ p0))
           (setf (creature-eff child) (remove-introns
                                       (creature-seq child)
                                       :output output-registers));; NB
           (loop for parent in (list p0 p1) do
                (when (equalp (creature-eff child) (creature-eff parent))
                  (setf (creature-fit child) (creature-fit parent))))

           (setf (creature-gen child) (1+ (max (creature-gen p0)
                                               (creature-gen p1))))

           (when genealogy (setf (creature-parents child) (list p0 p1))))
           
         ;; if we had a reference to the island handy, here, we could update
         ;; island coverage as well.
         ;; but there's really no need, since solved? does its look-up by creature-seq
         ;; not by the entire creature. could quotient this by remove-introns, come to
         ;; think of it. 
           
    offspring))

;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; Selection functions
;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

(defun solved? (exemplar creature island)
  "Returns nil if the creature has not solved that exemplar, on this island."
  (and *case-storage* (gethash (creature-seq creature)
                               (gethash exemplar (island-coverage island)))))



(defun record-case (exemplar creature island)
  ;; can we afford to maintain a set of hashes of creatures who have solved each
  ;; problem, globally per island? See if this can be done more cheaply than creature-cas
  ;; and then get rid of any mention of creature-cas entirely. 
  (unless (solved? exemplar creature island)
    (setf (gethash (creature-seq creature)
                   (gethash exemplar (island-coverage island))) T)))
;; we're now using island-coverage as a hashtable of hashtables.
;; HT1:  exemplar -> hashtable of creatures that solved that exemplar
;; HT2:  creature -> T


(defun fit-in-list (lst &key (comparator #'>))
  (reduce #'(lambda (x y) (if (funcall comparator
                                  (nil0 (creature-fit x))
                                  (nil0 (creature-fit y))) x y))
          lst))

(defun get-a-creature-who-solved-the-hardest (isle)
  "A greedy parent selection function, to be used, ideally, in
conjunction with a stochastic selector, like f-lexicase."
  (let ((seqs (loop for crt being the hash-keys in
                   (car (sort
                         (remove-if #'(lambda (x) (zerop (hash-table-count x))) ;; ignore empty tables
                                    (loop for crtset being the hash-values in (island-coverage isle)
                                       collect crtset))
                         #'(lambda (x y) (if (< (hash-table-count x) ;; sorting function
                                           (hash-table-count y)) x y))))
                 collect crt)))
    (mapcar #'(lambda (x) (find x (island-deme isle) ;; should be deme or packs
          :key #'creature-seq :test #'equalp)) seqs)))
;; seems inefficient. no need to sort the entire set when we just want the min


(defun cmatrix->accuracy (cmatrix)
  (divide (loop for i below (car (array-dimensions cmatrix)) sum
                     (aref cmatrix i i))
          (loop for i below (expt (car (array-dimensions cmatrix)) 2) sum
               (row-major-aref cmatrix i))))
  

(defun cmatrix->detection-rate (cmatrix)
  (/ (loop for i below (car (array-dimensions cmatrix)) sum
          (divide (aref cmatrix i i) ;; tp
                  (+ (loop for j below
                          (car (array-dimensions cmatrix)) sum
                          (aref cmatrix j i)))))
     (car (array-dimensions cmatrix))))

(defun cmatrix->worst-detection-rate (cmatrix)
  (reduce #'min (loop for i below (car (array-dimensions cmatrix)) collect
                     (divide (aref cmatrix i i) ;; tp
                             (+ (loop for j below
                                     (car (array-dimensions cmatrix)) sum
                                     (aref cmatrix j i)))))))


(defun compute-cmatrix (crt &key (ht *training-hashtable*)
                              (set t))
  ;; should gauge accuracy obey the sampling policy as well?
  ;; doing so seems to lead to premature termination of the evolution
  ;; in the case of iris, at least. 
  (let ((cmatrix (build-confusion-matrix *out-reg*)))
    (loop for k being the hash-keys in ht using (hash-value v) do
         (let ((guess (register-vote
                       (execute-creature crt
                                         :input k
                                         :output *out-reg*))))
           (incf (aref cmatrix guess v))))
    (when set
      (setf (creature-cm crt) cmatrix))
    cmatrix))


(defun invoke-sampling-policy (ht &key
                             (format 'alist)
                             (policy *sampling-policy*))
  (declare (ignorable format)) ;; a stub to expand on later, if needed
  (case policy
    ((:div-by-class-num-shuffle)
     (subseq (shuffle (loop for k being the hash-keys in ht
                         using (hash-value v) collect (cons k v)))
             0 (ceiling (/ (hash-table-count ht) *number-of-classes*))))
    ((:full-shuffle)
     (shuffle (loop for k being the hash-keys in ht
                 using (hash-value v) collect (cons k v))))
    (:otherwise (error "Request for unimplemented sampling policy."))))
    

(defun refresh-sample-if-needed (island &key
                                          (ht *training-hashtable*)
                                          (sp *sampling-policy*))
  (when (or (not (island-sample island))
            (= 0 (mod (island-era island) ;; when full gen elapsed
                      (/ (length (or (island-packs island)
                                     (island-deme island))) 2))))
    (setf (island-sample island)
          (invoke-sampling-policy ht
                                  :format 'alist
                                  :policy sp))))
    
(defun f-lexicase (island
                   &key (per-case #'per-case-n-ary))
  "Note: per-case can be any boolean-returning fitness function."
  (let* ((cases (copy-seq (island-sample island)))
         (sample-size (length cases))
         (candidates (copy-seq (or (island-packs island)
                                   (island-deme island))))
         (next-candidates candidates)
         (worst))
    (labels ((case-eval (crt cas cases-left)
               (declare (ignorable cases-left)) ;; kludge
               (cond ((funcall per-case crt cas island)
                      (when *case-storage*
                        (record-case (car cas) crt island))
                      T)
                       (:DEFAULT
                        NIL))))
      (loop while (and next-candidates cases) do
           (let ((the-case))
             (setf the-case (pop cases))
             (setf candidates next-candidates)
             (setf next-candidates
                   (remove-if-not 
                    #'(lambda (x) (case-eval x the-case cases))
                    candidates))
             (unless worst ;; somewhat better way to get worst
               (setf worst
                     (car (set-difference candidates next-candidates)))
               (unless worst ;; if we STILL can't find a worst, random
                 (setf worst (pick (if (island-packs island)
                                       (island-packs island)
                                       (island-deme island))))))))
      ;; a new, less memory-expensive approach to calculating fitness
      ;; (but more time expensive, since we're abandoning case storage)
      ;; (mapc #'(lambda (x) (grade-fitness x)) candidates)
      ;;(grade-fitness candidate cases)))
      (when (null candidates)
        (setf candidates
              (list (elt (island-deme island)
                         (random (length (island-deme island)))))))
      (values (car candidates)
              worst)))) ;; return just one parent


(defun lexicase! (island &key (elite-prob 0 )) ;; under construction*lexicase-elitism*))
  ;; make elitism a probability, instead of a boolean
  "Selects parents by lexicase selection." ;; stub, ex
  (let ((elitism (< (random 1.0) elite-prob))
        (per-case 
         (if (= (length *out-reg*) 1)
             #'per-case-binary
             #'per-case-n-ary))
        (population (if (island-packs island)
                        (island-packs island)
                        (island-deme island))))
        ;; (popsize (if (island-packs island)
        ;;              (length (island-packs island))
        ;;              (length (island-deme island)))))
    (refresh-sample-if-needed island
                              :ht *training-hashtable*
                              :sp *sampling-policy*)
    (multiple-value-bind (best-one worst-one)
        (f-lexicase island :per-case per-case)
      (multiple-value-bind (best-two worst-two)
          (cond (elitism (let* ((rarity ;; unstable. use at own peril. 
                                 (get-a-creature-who-solved-the-hardest
                                  island))
                                (unrarity (pick
                                           (set-difference population
                                                           (list rarity)))))
                           (if rarity
                               (values rarity unrarity))
                           (f-lexicase island :per-case per-case)))
                (:DEFAULT (f-lexicase island :per-case per-case)))

        ;; a bit of a kludge: lacking any reliable and efficient way of
        ;; extracting a fitness score from f-lexicase, we'll just measure
        ;; accuracy the old fashioned way, but only for the f-lex winners.
        ;; fitness, as such, will not be used for selection, but only for
        ;; gauging the success of the evolution, and the choice of (best)
        (loop for selected in (list best-one best-two) do
             (setf (creature-fit selected)
                   (or (creature-fit selected)
                       (fitness-dr selected)))) 
        ;; ranking fitness by detection rate, which seems more to the point
        ;; than accuracy in most applications of lexicase, but this should
        ;; be parameterizable by the user.
        
        (update-best-if-better best-one island)
        (update-best-if-better best-two island)
        
        (let ((children (mate best-one best-two
                              :island island
                              :genealogy *track-genealogy*)))

          (assert (not (some #'null children)))
          
          (mapc #'(lambda (x) (setf (creature-home x)
                               (island-id island))) children)

          (nsubst (car children) worst-one (island-deme island)
                  :test #'equalp)
          (nsubst (cadr children) worst-two (island-deme island)
                  :test #'equalp)

          ;; get rid of the creatures disposed of
          (when *case-storage*
            (loop for worst in (list worst-one worst-two) do
                 (bury-cases worst island)))
          children)))))
  
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
    (if (eq :alpha (creature-typ crt))
        (FORMAT T "**** PACK NEW BEST ON ISLAND ~A: FIT = ~5,2F ****~%"
                (roman (creature-home crt)) (creature-fit crt)))
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
     (mapc #'fitness deme)
     (setf deme (fitsort-deme deme))
     (update-best-if-better (car deme) island)
     (setf parents (mapcar #'(lambda (x) (elt deme x)) lucky))
     (setf thedead (mapcar #'(lambda (x) (elt deme x)) unlucky))
     ;; (FORMAT T "FIT OF PARENTS: ~A~%FIT OF THEDEAD: ~A~%"
     ;;         (mapcar #'creature-fit parents)
     ;;         (mapcar #'creature-fit thedead))
     (setf children (apply #'(lambda (x y) (mate x y :island island)) parents))
     (mapc #'(lambda (crt idx) (setf (elt (island-deme island) idx) crt))
             children unlucky)
     children))


(defun tournement! (island &key (genealogy *track-genealogy*)
                             (fitfunc *fitfunc*))
  "Tournement selction function: selects four combatants at random
from the population provide, and lets the two fittest reproduce. Their
children replace the two least fit of the four. Disable genealogy to
facilitate garbage-collection of the dead, at the cost of losing a
genealogical record."  
  (labels ((popgetter (isle)
             (or (island-packs isle)
                 (island-deme isle)))
           (popsetter (isle p)
             (if (island-packs isle)
                 (setf (island-packs isle) p)
                 (setf (island-deme isle) p))))
    (let ((population)
          (combatants))
      (setf (island-deme island) (shuffle (island-deme island))
            population (cddddr (popgetter island)) ;;(island-deme island))
            combatants (subseq (popgetter island) 0 4)) ;;(subseq (island-deme island) 0 4 ))
      (loop for combatant in combatants do
           (unless (creature-fit combatant)
             (fitness combatant
                      :fitfunc fitfunc)))
      (let* ((ranked (sort combatants
                           #'(lambda (x y) (< (creature-fit x)
                                         (creature-fit y)))))
             (best-in-show  (car (last ranked)))
             (parents (cddr ranked))
             (children (apply
                        #'(lambda (x y) (mate x y
                                       :island island
                                       :genealogy genealogy))
                        parents)))
        (update-best-if-better best-in-show island)
        ;; Now replace the dead with the children of the winners
        (mapc #'(lambda (x) (setf (creature-home x) (island-id island))) children)
        (loop for creature in (concatenate 'list parents children) do
             (push creature population))
        (popsetter island population)
        (island-best island)))))

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
         (broods (mapcar #'(lambda (x y) (mate x y
                                          :genealogy genealogy
                                          :island island))
                         mothers fathers)))
    (setf children (apply #'concatenate 'list broods))
    (mapc #'(lambda (x) (setf (creature-home x) (island-id island))) children)
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

(defun bury (crt island)
  (bury-cases crt island)
  (bury-parents crt))

(defun bury-cases (crt island)
  "Free some memory when burying a creature."
  (loop for crtset being the hash-values in (island-coverage island) do
       (remhash crt crtset)))

(defun bury-parents (crt)
  (setf (creature-parents crt) nil))

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

(defun reorder-demes (island-ring &key (greedy t))
  "Shuffles the demes of each island in the ring, or fitsorts a
fraction of the deme and places the fittest at the top."
  (loop for isle in (de-ring island-ring) do
       (with-mutex ((island-lock isle))
         (setf (island-deme isle)
               (if greedy
                   (fitsort-deme (island-deme isle) :fraction greedy)
                   (shuffle (island-deme isle)))))))
  
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
           (with-mutex ((island-lock isle-1))
             (with-mutex ((island-lock isle-2))
               ;; (sb-thread:grab-mutex (island-lock isle-1))
               ;; (sb-thread:grab-mutex (island-lock isle-2))
               (setf (subseq (island-deme isle-1) 0 emigrant-count)
                     (subseq (island-deme isle-2) 0 emigrant-count)))))
;;           (sb-thread:release-mutex (island-lock isle-1))
;;           (sb-thread:release-mutex (island-lock isle-2)))
      ;; (sb-thread:grab-mutex (island-lock (elt island-ring (1- (island-of (car island-ring))))))
      (with-mutex ((island-lock (elt island-ring (1- (island-of (car island-ring))))))
        (setf (subseq (island-deme
                       (elt island-ring (1- (island-of (car island-ring)))))
                      0 emigrant-count) buffer))
      (sb-ext:gc :full t) ;; maybe this will help?
      island-ring))

;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

(defstruct pier crowd lock)

(defparameter *pier* (make-pier :crowd '()
                          :lock (sb-thread:make-mutex :name "pier-lock")))

(defparameter *island-capacity* (floor (/ *population-size*
                                          *number-of-islands*)))

(defparameter *m-counter* 0)

;; not ready for deployment yet. figure out what's meant by CASable places. 
(defun migrate-freely (island &key (pier *pier*)
                                           (emigrant-fraction *migration-size*)
                                           (greedy *greedy-migration*))
    (with-mutex ((pier-lock pier))
      (cond ((and (>= (length (island-deme island)) *island-capacity*)
                  (= 0 (mod (island-era island) *migration-rate*)))
             (setf (island-deme island)
                   (fitsort-deme (island-deme island) :fraction greedy))
             (loop repeat (* (length (island-deme island)) emigrant-fraction)
                do
                  (incf *m-counter*)
                  (push (pop (island-deme island)) (pier-crowd pier)))
             (setf (pier-crowd pier) (shuffle (pier-crowd pier))))
            ((= (floor (/ *migration-rate* 2))
                (mod (island-era island) *migration-rate*))
             (loop
                while pier
                while (< (length (island-deme island)) *island-capacity*) do
                  (incf *m-counter*)
                  (and *debug*
                       (FORMAT T "== MIGRATING CREATURE FROM ISLAND ~A TO ISLAND ~A ==~%"
                               (roman (creature-home (car (pier-crowd pier))))
                               (roman (island-id island))))
                  (push (pop (pier-crowd pier)) (island-deme island)))))))
  

                                  


;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
    
(defun init-cases-covered (ht)
  (let ((cov (make-hash-table :test #'equalp)))
    (loop for k being the hash-keys in ht do
         (setf (gethash k cov) (make-hash-table :test #'equalp)))
    cov))

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
                                         :coverage (if *case-storage*
                                                       (init-cases-covered
                                                        *training-hashtable*))
                                         :logger (make-logger)
                                         :lock (sb-thread:make-mutex
                                                :name
                                                (format nil
                                                        "isle-~d-lock"
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
  (concatenate 'vector (loop repeat len collect (random *max-inst*))))

(defun spawn-creature (len)
  (make-creature :seq (spawn-sequence len)
                 :gen 0
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



(defun release-all-locks (island-ring)
  "Mostly for running from the REPL after aborting the programme."
  (sb-thread:release-mutex -migration-lock-)
  (loop for island in (de-ring island-ring) do
       (sb-thread:release-mutex (island-lock island))))


;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; Data gathering and reporting
;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=


;; (defun best-of-all (island-ring)
;;   (let ((best-so-far (make-creature :fit 0)))
;;     (loop for isle in (de-ring island-ring) do
;;          (if (> (nil0 (creature-fit (island-best isle)))
;;                 (creature-fit best-so-far))
;;              (setf best-so-far (island-best isle))))
;;     ;; you could insert a validation routine here
;;     ;; (setf *best* best-so-far) ;; cause of race condition?
;;     best-so-far))

(defun best ()
  (island-best (reduce #'(lambda (x y) (if (> (creature-fit (island-best x))
                                         (creature-fit (island-best y)))
                                      x y))
                       (de-ring +island-ring+))))
                         

(defun make-stopwatch ()
  (let ((start-time 0))
    (lambda (&optional arg)
      (case arg
        ((set) (setf start-time (get-universal-time)))
        (otherwise (- (get-universal-time) start-time))))))

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

(defun print-params2 ()
  "Prints major global parameters, and some statistics, too."
  (hrule)
  (format t "[+] STAT INTERVAL:        ~D~%" *stat-interval*)
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
  (format t "[+] REPRODUCTION:         ~A (~A)~%"
          (if *sex* :sexual :asexual)
          *sex*)
  (format t "[+] MUTATION RATE:        ~d~%" *mutation-rate*)
  (format t "[+] METAMUTATION RATE:    ~d~%" *metamutation-rate*)
  (format t "[+] PACK FORMATION:       ~A~%"
          (if *packs* "YES" "NO"))
  (format t "[+] MAX SEQUENCE LENGTH:  ~d~%" *max-len*)
  (format t "[+] MAX STARTING LENGTH:  ~d~%" *max-start-len*)
  (format t "[+] # of TRAINING CASES:  ~d~%"
          (hash-table-count *training-hashtable*))
  (format t "[+] # of TEST CASES:      ~d~%"
          (hash-table-count *testing-hashtable*))
;;  (format t "[+] FITNESS FUNCTION:     ~s~%" (get-fitfunc))
  (format t "[+] NUMBER OF CLASSES:    ~D~%" (funcall =label-scanner= 'count))
  (format t "[+] SELECTION FUNCTION:   ~s~%" *method*)
  (format t "[+] OUTPUT REGISTERS:     ~a~%" *out-reg*)
  (hrule))

(defun print-params (&optional (stream *standard-output*))
  (hrule stream)
  (loop for tweakable in *tweakables* do
       (format stream "[+] ~A: ~A~%"
               tweakable (symbol-value tweakable)))
  (format stream "[+] *FITFUNC*: ~A~%" *fitfunc*)
  (hrule stream))



(defun print-fitness-by-gen (logger &key (stream *standard-output*))
  (flet ((.*. (x y)
           (if (numberp y)
               (* x y)
               NIL)))
  (let ((l (funcall logger)))
    (loop
       for (e1 e2) on l by #'cddr
       for i from 1 to 5 do
         (format stream "~c~d:~c~5,4f %~c~c~d:~c~5,4f %~%" #\tab
                 (car e1) #\tab (.*. 100 (cdr e1)) #\tab #\tab
                 (car e2) #\tab (.*. 100 (cdr e2)))))))

(defun opcode-census (population &key (stream *standard-output*))
  (let* ((buckets (make-array (expt 2 *opcode-bits*)))
         (instructions (reduce #'(lambda (x y) (concatenate 'list x y))
                               (mapcar #'creature-seq population)))
         (sum (length instructions)))
    (loop for inst in instructions do
         (incf (elt buckets (ldb (byte *opcode-bits* 0) inst))))
    (loop repeat (/ (length buckets) 2)
       for x = 0 then (+ x 2)
       for y = 1 then (+ y 2) do
         (format stream "~C~A: ~4D~C(~5,2f %)~C" #\tab
                 (func->string (elt *operations* x))
                 (elt buckets x)
                 #\tab
                 (* 100 (divide (elt buckets x) sum)) #\tab)
         (format stream "~C~A: ~4D~c(~5,2f %)~%" #\tab
                 (func->string (elt *operations* y))
                 (elt buckets y)
                 #\tab
                 (* 100 (divide (elt buckets y) sum))))))

(defun percent-introns (crt)
  (if (null crt) 0
      (float (* (divide (length (remove-if-not #'intron? (creature-eff crt)))
                        (length (creature-eff crt)))
                100))))

;; start using this instead, for statistics
(defun avg-property-population (population prop-p)
  (apply #'avg (mapcar prop-p population)))

(defun average-intron-rate (population)
  (avg-property-population (remove-if-not #'creature-eff population)
                           #'percent-introns))
                           
                                
(defun get-total-population (island-ring)
  (apply #'concatenate 'list (mapcar #'island-deme (de-ring island-ring))))

(defun proportion-native-population (island)
  (flet ((native-p (x)
           (= (creature-home x) (island-id island))))
  (divide (length
           (remove-if-not #'native-p (island-deme island)))
          (length (island-deme island)))))


(defun print-statistics (island-ring &key (stream *standard-output*)
                                       (stopwatch #'(lambda () :inactive)))
  (mapc #'(lambda (x) (print-statistics-for-island x :stream stream))
        (de-ring island-ring))
  (hrule stream)
  (format stream "BEST: ~F % FROM ISLE ~A ~A *  ~D SEC. INTO ~A ON ~A~%"
          (* 100 (creature-fit (best)))
          (roman (creature-home (best)))
          (if (creature-pack (best))
              (format nil "(PACK)")  
              "")
          (funcall stopwatch)
          (func->string *method*)
          *dataset*)
  (hrule stream)
;;  (unless (creature-cm (best))
;;    (compute-cmatrix (best)))
  (when (creature-cm (best))
    (print-confusion-matrix (creature-cm (best)) stream)
    (hrule stream)))


(defun print-statistics-for-island (island &key (stream *standard-output*))
  ;; eventually, we should change *best* to list of bests, per deme.
  ;; same goes for logger. 
  (hrule stream)
  (let ((intron-rate (average-intron-rate (island-deme island)))
        (alpha-intron-rate (average-intron-rate (island-packs island)))
        (average-length
         (divide (reduce #'+
                         (mapcar #'(lambda (x) (length (creature-seq x)))
                                 (island-deme island)))
                 (length (island-deme island))))
        (average-length-of-alphas
         (if (island-packs island)
             (apply #'avg (mapcar #'(lambda (x) (length (creature-seq x)))
                          (island-packs island)))
             0)))
    
    (format stream "              *** STATISTICS FOR ISLAND ~A AT ERA ~D ***~%"
            (roman (island-id island))
            (island-era island))
    (hrule stream)
    (format stream "[*] DEVELOPMENTAL STAGE: ~A~%"
            (if (island-packs island)
                "PACKS"
                "INDIVIDUALS"))
    (format stream "[*] PERCENT NATIVE: ~5,2F %~%"
            (* 100 (proportion-native-population island)))
    (format stream "[*] SELECTION METHOD ON ISLAND: ~A~%"
            (func->string (island-method island)))
    (format stream "[*] BEST FITNESS SCORE ACHIEVED ON ISLAND: ~5,4f %~%"
            (* 100 (creature-fit (island-best island))))
    (format stream "[*] AVERAGE FITNESS~A ON ISLAND: ~5,2f %~%"
            (if (island-packs island) " OF UNDERLINGS" "")
            (* 100 (apply #'avg (remove-if #'null
                                   (mapcar #'creature-fit
                                           (island-deme island))))))
    (when (island-packs island)
      (format stream "[*] AVERAGE FITNESS OF PACKS ON ISLAND: ~5,2f %~%"
              (* 100 (apply #'avg (remove-if #'null
                                     (mapcar #'creature-fit
                                             (island-packs island)))))))
    (format stream "[*] BEST FITNESS BY GENERATION:  ~%")
    (print-fitness-by-gen (island-logger island) :stream stream)
    (format stream "[*] AVERAGE SIMILARITY TO BEST:  ~5,2f %~%"
          (* 100 (likeness-to-specimen (island-deme island) (island-best island))))
    (format stream "[*] STRUCTURAL INTRON FREQUENCY: ~5,2f %~%"
            intron-rate)
  (when *case-storage*
    (let ((numcases (hash-table-count *training-hashtable*)))
      (format stream "[*] CREATURES TO SOLVE HARDEST CASE OF ~D:   ~D~%"
              numcases (difficulty-check island :reducer #'min) )
      (format stream "[*] CREATURES TO SOLVE EASIEST CASE OF ~D:   ~D~%"
              numcases (difficulty-check island :reducer #'max) )
      (format stream "[*] AVERAGE NUMBER OF CREATURES TO SOLVE EACH CASE: ~5,2F~%"
              (divide (difficulty-check island :reducer #'+) numcases))))
  (format stream "[*] AVERAGE LENGTH: ~5,2F INSTRUCTIONS (~5,2F EFFECTIVE)~%"
          average-length (- average-length
                            (* average-length intron-rate 1/100)))
  (when (island-packs island)
    (format stream "[*] AVERAGE LENGTH OF ALPHAS: ~5,2F INSTRUCTIONS (~5,2F EFFECTIVE)~%"
            average-length-of-alphas (- average-length-of-alphas
                                        (* average-length-of-alphas
                                           alpha-intron-rate 1/100))))
  (format stream "[*] OPCODE CENSUS~A:~%"
          (if (island-packs island) " OF UNDERLINGS" ""))
  (if (< *population-size* 2000)
      (opcode-census (island-deme island) :stream stream)
      (format stream "    POPULATION TOO LARGE TO SURVEY EFFICIENTLY.~%"))
  (when (island-packs island)
    (format stream "[*] OPCODE CENSUS OF ALPHAS:~%")
    (if (< *population-size* 2000)
        (opcode-census (island-packs island) :stream stream)
        (format stream "    POPULATION TOO LARGE TO SURVEY EFFICIENTLY.~%")))))
  
  
(defun difficulty-check (island &key (reducer #'min))
  (reduce reducer (loop for v being the hash-value in
                       (island-coverage island) collect
                       (hash-table-count v))))


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
              (creature-home (car (creature-parents crt)))))
    (when (creature-pack crt)
      (format t "PACK: ~%")
      (hrule)
      (loop
         for c in (creature-pack crt)
         for i from 0 do
           (format t "[~D] FIT: ~F~%    SEQ: ~A~%"
                   i (creature-fit c) (creature-seq c)))))
    
  (hrule)
  (format t "DISASSEMBLY OF EFFECTIVE CODE:~%~%")
  (disassemble-sequence (creature-eff crt) :static t))
  
(defun genealogical-fitness-stats ()
  (let ((sum 0))
    (mapc #'(lambda (x) (if (numberp x) (incf sum x))) *records*)
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
          
(defun classification-report (crt dataset &key (testing t)
                                            (ht *testing-hashtable*))
  (if testing
      (setf ht *testing-hashtable*)
      (setf ht *training-hashtable*))
  (case dataset
    (:tictactoe (ttt-classification-report :crt crt :ht ht :out *out-reg*))
    (otherwise (data-classification-report :crt crt :ht ht :out *out-reg*))))

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


;;(setf *stop* t)

;; (defun check-cas (exemplar crt)
;;   (cond ((null (creature-cas crt))
;;          (setf (creature-cas crt)
;;                (make-hash-table :test #'equalp
;;                                 :size (hash-table-count *training-hashtable*)))
;;          nil)
;;         (t (gethash exemplar (creature-cas crt)))))
         
;; gauge-accuracy isn't a good measure, as it stands.
;; instead, go by the size of creature-cas, in lexicase.
;; but creature-cas needs to be debugged, first



;; more efficient technique: maintain a list of unsolved cases, and remove
;; them as they're solved. 

(defun case-coverage-helper (crt island)
  ;; inefficient, but let's try not to use this function
  ;; O(n) for n = (hash-table-count *training-hashtable*)
  (let ((tally (loop for v being the hash-values in (island-coverage island)
                  collect (gethash (creature-seq crt) v))))
    tally))

(defun case-coverage (crt island)
  (let ((tally
         (case-coverage-helper crt island)))
                               ;; (find (creature-home crt)
                               ;;           +ISLAND-RING+
                               ;;           :key #'island-id))))
    
    (divide (length (remove-if #'null tally)) (length tally))))

;; (defun pack-coverage (alpha island)
;;   ;; a bit expensive. don't use too often, or optimize.
;;   ;; it would be nice to just mapcar #'or over the case-coverage
;;   (let ((tally (mapcar #'
;;                          (print (map 'list #'(lambda (x) (case-coverage-helper x island))
;;                                  (creature-pack alpha))))))
                         
;;     (print tally)
;;     (divide (length (remove-if #'null tally)) (length tally))))


(defun populate-island-with-packs (isle)
 ;; (loop for isle in (de-ring island-ring) do
;;  (sb-thread:grab-mutex (island-lock isle))
  (with-mutex ((island-lock isle))
    (let ((pack-size (1- (expt 2 *destination-register-bits*))))
      ;;    (setf *sex* nil)
      (format t "ISLAND ~A IS BEING POPULATED WITH PACKS...~%"
              (roman (island-id isle)))
      (setf (island-packs isle)
            (loop repeat *pack-count*
               collect
                 (make-creature
                  :seq (spawn-sequence (+ *min-len*
                                          (random *max-start-len*)))
                  :typ :alpha
                  :gen 0
                  :home (island-id isle)
                  :mut *mutation-rate*
                  :pack (loop repeat pack-size collect
                             (pick (island-deme isle))))))
      (setf (island-method isle) *pack-method*))))


;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; Functions only intended to facilitate experimentation and development
;; in the REPL
;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=


;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; extra
;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

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


;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; The main event
;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

(defun time-for-packs (isle)
  (and *packs*
       (not (island-packs isle))
       (or (< *pack-thresh-by-fitness*
              (creature-fit (island-best isle)))
           (and *case-storage*
                (< *pack-thresh-by-difficulty*
                   (difficulty-check isle :reducer #'min)))
           (< *pack-thresh-by-era* (island-era isle))
           (have-i-plateaued? isle :span *pack-thresh-by-plateau*))))

(defun max-era (island-ring)
  (reduce #'max (mapcar #'island-era (de-ring island-ring))))

(defun evolve (&key (method *method*)
                 (dataset *dataset*)
                 (rounds *rounds*)
                 (target *target*)
                 (stat-interval *stat-interval*)
                 (island-ring +island-ring+) 
                 (migration-rate *migration-rate*)
                 (migration-size *migration-size*)
                 (parallelize *parallel*))
  ;; adjustments needed to add fitfunc param here. 
  (setf *STOP* nil)
  (setf *parallel* parallelize)
  (mapc #'(lambda (x) (setf (island-method x) method)) (de-ring island-ring))
  (let ((stopwatch (make-stopwatch))
        (timereport "")
        (use-migration t)
        (correct+incorrect))
    (funcall stopwatch 'set)
  ;;; Just putting this here temporarily:
    (setf timereport
          (with-output-to-string (*trace-output*)
            (time (block evolver
                    (handler-case 
                        (loop for i from 1 do
                             (let ((isle (pop island-ring)))
                               ;; island-ring is circular, so pop
                               ;; will cycle & not exhaust it
                               (labels ((time-for (interval)
                                          (= 0 (mod (max-era +island-ring+)
                                             interval)))

                                        (parallel-dispatcher ()
                                          (handler-case 
                                              (with-mutex ((island-lock isle)
                                                           :wait-p nil)
                                                (incf (island-era isle))
                                                (funcall (island-method isle)
                                                         isle)
                                                (when (eq *migration-method*
                                                          :free)
                                                  (migrate-freely isle))
                                                (sb-ext:gc))
                                            (sb-sys:memory-fault-error ()
                                              (progn (format t "ENCOUNTERED MEMORY FAULT. WILL TRY TO EXIT GRACEFULLY.~%")
                                                   (setf *STOP* t)))))
                                            
                                            

                                        (serial-dispatcher ()
                                          (incf (island-era isle))
                                          (funcall (island-method isle) isle)
                                          (when (eq *migration-method*
                                                    :free)
                                            (migrate-freely isle))
                                          (sb-ext:gc))
                                        
                                        (dispatch ()
                                          (if *parallel*
                                              (sb-thread:make-thread
                                               #'parallel-dispatcher)
                                              (serial-dispatcher))))
                                 (dispatch)
                                 (when *gc* (sb-ext:gc :full t)) ;; see if this helps with heap exhaustion
                                 (when (and (eq *migration-method*
                                                :cyclic)
                                            (time-for migration-rate))
                                   (with-mutex (-migration-lock- :wait-p t)
                                     (princ ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>")
                                     (princ " MIGRATION EVENT ")
                                     (migrate island-ring
                                              :emigrant-fraction migration-size)
                                     (format t "<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<~%")))
                                 (when (time-for stat-interval)
                                   ;;(best-of-all +island-ring+)
                                   (print-statistics island-ring
                                                     :stopwatch stopwatch))
                                
                                 (when (time-for-packs isle)             
                                   (populate-island-with-packs isle)
                                   (setf use-migration nil))
                                 (when (time-for *save-every*)
                                   (format t "~%~%--- SAVING ISLAND-RING AND PARAMETERS ---~%~%")
                                   (save-all +ISLAND-RING+))
                                 (when (or (> (creature-fit
                                               (island-best isle))
                                              target)
                                           (time-for *rounds*)
                                           *STOP*)
                                   (format t "~%TARGET OF ~f REACHED AFTER ~d ROUNDS~%"
                                           target (max-era +island-ring+))
                                   (return-from evolver)))))
                      (sb-sys:interactive-interrupt () (setf *STOP* t)))))))
          (print-statistics +island-ring+)
          ;;(best-of-all island-ring)

          (loop for isle in (de-ring island-ring) do
               (plot-fitness isle))
          (setf correct+incorrect (classification-report (best) dataset))
          (when *track-genealogy* (genealogical-fitness-stats))
          (hrule)
          (when (not (zerop (creature-fit (best))))
            (format t "                         -oO( BEST SPECIMEN )Oo-~%")
            (hrule)
            (if (creature-pack (best))
                (format t "PACK OF ISLAND ~A~%" (roman (creature-home (best))))
                (format t "DENIZEN OF ISLAND ~A AT ERA ~D~%"
                        (roman (creature-home (best)))
                        (island-era (find (creature-home (best))
                                          (de-ring +island-ring+)
                              :key #'island-id))))
            (format t "~D TESTS CLASSIFIED CORRECTLY, ~D INCORRECTLY.~%"
                    (car correct+incorrect) (cdr correct+incorrect))
            (hrule)
            (print-creature (best)))
          (format t "~%~A" timereport))
    (hrule))
          

;; case storage is really hard on the heap for large datasets. but we
;; don't need to store the entire exemplar. we could easily get by
;; with a unique hash of it, using a cheap hashing algo. are
;; hash-tables necessary, as opposed to sets? (I mean, hash tables
;; consisting only of keys?)

;; the problem is that there's such a huge, almost exponential maybe,
;; redundancy of data. we need a new, different data structure for
;; case storage.


;; (defun reset-best ()
;;   (setf *best* (make-creature :fit 0)))

(defun stop ()
  (setf *stop* t))

(defun have-i-plateaued? (island &key (span 1000))
  (if (funcall (island-logger island))
      (> (- (island-era island)
            (caar (funcall (island-logger island))))
         span)))
     

(stop)
;; Start distinguishing between true positives, false positives, true
;; negatives, false negatives in the data-classification-report
;; section.

;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; tests
;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

(defun test-execution-constancy (n)
  (loop for i below n do
       (let ((seq (spawn-sequence 30))
             (result1)
             (result2))
         (hrule)
         (format t "[~D] TESTING FOR SEQ = ~A~%" i seq)
         (setf result1 (execute-sequence seq
                                         :input #(3 4 5 6)
                                         :output '(0)))
         (loop repeat 10 do
              (setf result2 (execute-sequence seq
                                              :input #(3 4 5 6)
                                              :output '(0)))
              (format t "~A = ~A~%" result1 result2)
              (assert (equalp result1 result2))))))

                 
