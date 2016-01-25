(defpackage :genetic.hello
  (:use :common-lisp))

(in-package :genetic.hello)

;(defparameter *target* "Genetic Algorithm")
(defparameter *target* "You do look, my son, in a moved sort, As if you were dismay'd: be cheerful, sir. Our revels now are ended. These our actors, As I foretold you, were all spirits and Are melted into air, into thin air: And, like the baseless fabric of this vision, The cloud-capp'd towers, the gorgeous palaces, The solemn temples, the great globe itself, Ye all which it inherit, shall dissolve And, like this insubstantial pageant faded, Leave not a rack behind. We are such stuff As dreams are made on, and our little life Is rounded with a sleep. Sir, I am vex'd; Bear with my weakness; my, brain is troubled: Be not disturb'd with my infirmity: If you be pleased, retire into my cell And there repose: a turn or two I'll walk, To still my beating mind")
(defparameter *creature-len* (length *target*))
(defparameter *pop-size* 5)
(defparameter *mutation-rate* 90) ; percent chance of mutation
(defparameter *default-best* '(9999999999 ""))
(defparameter *best* '())
(defparameter *population* '())
(defparameter *display-thresh* 99999999999)
(defparameter *display-step* 100)

(defun cls()
  (format t "~A[H~@*~A[J" #\escape))

(defun rnd-char ()
  (code-char (+ (random #x5F) #x20)))

(defun rnd-str (len)
  (concatenate 'string (loop repeat len collect (rnd-char))))

(defun char-dist (a b)
  (abs (- (char-code a) (char-code b))))

(defun str-dist (a b &optional (len *creature-len*))
  (loop for i from 0 to (1- len)
       sum (char-dist (char a i) (char b i))))


(defun fitness (creature)
  (let ((f (str-dist creature *target*)))
    (when (< f (car *best*))
      (setf (car *best*) f)
      (setf (cadr *best*) creature)
      (when (< f *display-thresh*)
        (cls)
        (format t "~a[~d] ~a~%~%" #\return (car *best*) (cadr *best*))
        (setf *display-thresh* (- f (floor (/ f *display-step*))))))
    f))

(defun init-population (&optional (psize *pop-size*) (clen *creature-len*))
  (concatenate 'vector (loop repeat psize collect (rnd-str clen))))


(defun mutate (creature)
  "Destructively mutates a string, by randomly shifting one character."
  (let* ((shift (if (= (random 2) 0) -1 1))
         (idx (random (length creature)))
         (allele (char-code (char creature idx)))
         (mutation (code-char (max #x20 (min #x7F (+ allele shift))))))
    (setf (char creature idx) mutation)
    creature))

(defun maybe-mutate (creature)
  (if (< (random 100) *mutation-rate*)
      (mutate creature)
      creature))

(defun crossover (mother father &optional (idx0 (random *creature-len*))
                                  (idx1 (random *creature-len*)))
  (let ((child (copy-seq mother))
        (minidx (min idx0 idx1))
        (maxidx (max idx0 idx1)))
;;    (format t "swapping ~a for ~a at ~d in ~a~%" (char father idx) (char mother idx) idx child)
    (setf (subseq child minidx maxidx) (subseq father minidx maxidx))
    (maybe-mutate child)))

(defun shufflefuck (mother father)
  (let ((child mother))
    (loop for i from 0 to (1- *creature-len*)
       do (setf (char child i) (if (= 0 (random 2)) (char mother i)
                                   (char father i))))
    (maybe-mutate child)))


(defun mate (mother father)
  (list (crossover mother father) (crossover father mother)))
(defun n-rnd (low high &optional (r '()) (n 4))
  "Returns a list of n distinct random numbers between low and high."
  (when (<= (- high low) n)
    (error "This will loop forever, you fool!"))
  (loop

     (when (= (length r) n)
         (return r))
     (setf r (remove-duplicates (cons (+ low (random high)) r)))))

(defun tournement (population &optional (psize *pop-size*))
  (let* ((lots (n-rnd 0 psize))
         (combatants (mapcar #'(lambda (i) (list (elt population i) i)) lots))
         (ranked (sort combatants
                       #'(lambda (x y) (< (fitness (car y)) (fitness (car x))))))
         (winners (cddr ranked))
         (parents (mapcar #'car winners))
         (children (apply #'mate parents))
         (losers (subseq ranked 0 2))
         (graves (mapcar #'cadr losers)))
    (map 'list #'(lambda (grave child) (setf (elt population grave) child))
         graves children)
    (mapcar #'fitness children))) ;; to update the *best* variable
;;    (format t "LOSERS:~c~c~a~c~a~%WINNERS:~c~a~c~a~%OFFSPRING:~c~a~c~a~%~%"
;;            #\Tab #\Tab (caar losers) #\Tab (caadr losers)
;;            #\Tab (car parents)  #\Tab (cadr parents)
;;            #\Tab (car children) #\Tab (cadr children))
  



(defun main-loop (iterations)
  ;;(let ((population (init-population)))
  (setf *population* (init-population))
  (setf *best* (copy-seq *default-best*))
  (setf *display-thresh* 9999999999999)
  (loop for i from 1 to iterations
       do
         (tournement *population*)
         (when (equal (cadr *best*) *target*)
           (format t "**** TARGET REACHED AFTER ~d TOURNEMENTS ****~%" i)
           (return *best*))))

(defun main ()
  (main-loop 10000000))



