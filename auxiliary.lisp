;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; General Mathematical Helper Functions
;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=


(in-package :genlin)

(defun avg (&rest args)
  (divide (reduce #'+ args) (length args)))

(export 'reverse-hash-table)
(defun reverse-hash-table (hash-table &key (test 'eql))
  (let ((newtable (make-hash-table :test test)))
    (loop for k being the hash-keys in hash-table do
         (setf (gethash (gethash k hash-table) newtable) k))
    newtable))

(export 'roman)
(defun roman (n)
  (cond ((null n) "NULL")
        ((= 0 n) "ZERO")
        (t (format nil "~@R" n))))

(export 'divide)
(defun divide (&rest args)
  (if (some #'zerop args) 0
      (reduce #'/ args)))


(export 'to-gray-vec)
(defun to-gray-vec (base digits value)
  ;; adapted from C programme found in Wikipedia's Gray Code entry
  (let ((gray (make-array digits))
        (base-n (make-array digits))
        (shift 0))
    ;; put the normal base-n number into the base-n array. For
    ;; base 10, 109 would be stored as #(9 0 1).
    (loop for i from 0 to (1- digits) do
         (setf (aref base-n i) (mod value base)
                 value (floor (/ value base))))
      ;; convert the normal base-n number into the graycode equivalent
      ;; note that the loop starts at the most significant digit and goes down
      (loop for i from (1- digits) downto 0 do
           (setf (aref gray i) (mod (+ (aref base-n i) shift) base)
                 shift (+ shift (- base (aref gray i))))) ;; subtract from base so shift is +
      gray))

;; tictactoe-specific
(defparameter *gray-lookup*
  (let ((grayhash (make-hash-table)))
    (loop for i from 0 to #3r222222222 do
         (setf (gethash i grayhash) (to-gray-vec 3 9 i)))
    grayhash))

(defparameter *degray-lookup*
    (reverse-hash-table *gray-lookup* :test 'equalp))



(export 'sub-map)
(defun sub-map (seq map)
  "Substitutes elements in seq according to hashtable ht. Keys and vals of ht 
must be disjoint."
  (let ((el (concatenate 'list (remove-duplicates seq)))
        (copy (copy-seq seq))
        (mapper (case (type-of map)
                  (cons #'(lambda (x) (cdr (assoc x map))))
                  (hash-table #'(lambda (x) (gethash x map)))
                  (otherwise (error "Error in sub-map.")))))
    (loop for e in el do
         (when (funcall mapper e)
           (setf copy (substitute (funcall mapper e) e copy))))
    copy))

(export 'sub-assoc)
(defun sub-assoc (seq al)
  (let ((el (concatenate 'list (remove-duplicates seq)))
        (copy (copy-seq seq)))
    (loop for e in el do
         (setf copy (substitute (assoc e al) e copy)))
    copy))

(export 'shuffle)
(defun shuffle (seq)
  (sort seq #'(lambda (x y) (declare (ignore x y))(= 0 (random 2)))))

(defun n-rnd (low high &key (n 4))
  "Returns a list of n distinct random numbers between low and high."
  (let ((r '()))
    (declare (type fixnum low high n))
    (declare (optimize speed))
    (when (< (- high low) n)
      (error "Error in n-rnd: interval too small: infinite loop"))
   (loop (when (= (length r) n)
            (return r))
       (setf r (remove-duplicates (cons (+ low (random high)) r))))))

;; a helper:
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


(defun pospart (n)
  (if (< n 0) 0 n))

(defun ! (n)
  "Just like C's -- except that 0 will not be interpreted as false in CL."
  (if (= n 0) 1 0))

;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; pretty printing

(defun hrule (&optional (stream *standard-output*))
  (format stream "-----------------------------------------------------------------------------~%"))

(defun nil0 (n)
  (if (null n) 0 n))

(defun func->string (func)
  (flet ((nil+1 (n)
           (if (null n) 0 (1+ n))))
  (let* ((fm (format nil "~a" func))
         (i (mismatch fm "#<FUNCTION __"))
         (o (subseq fm i (1- (length fm))))
         (plain (subseq o (nil+1 (position #\: o :from-end t)))))
    #+sbcl plain
    #+clisp (subseq plain 0 (position #\space plain)))))

;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; data processing tools
;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

(defun split-at-label (str)
  (let ((lastcomma-idx (position #\, str :from-end t)))
    (cons (subseq str 0 lastcomma-idx) (subseq str (1+ lastcomma-idx)))))

;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; List processing
;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

;; from Rosetta Code

;; Memoization Macro for dynamic programming
(defmacro mem-defun (name args body)
  (let ((hash-name (gensym)))
    `(let ((,hash-name (make-hash-table :test 'equal)))
       (defun ,name ,args
         (or (gethash (list ,@args) ,hash-name)
             (setf (gethash (list ,@args) ,hash-name)
                   ,body))))))

;; Longest common subsequence
(mem-defun lcs (xs ys)
           (labels ((longer (a b) (if (> (length a) (length b)) a b)))
             (cond ((or (null xs) (null ys)) nil)
                   ((equal (car xs) (car ys)) (cons (car xs) (lcs (cdr xs) (cdr ys))))
                   (t (longer (lcs (cdr xs) ys)
                                    (lcs xs (cdr ys)))))))


;; from Paul Graham, On Lisp
(defun flatten (x)
  (labels ((rec (x acc)
             (cond ((null x) acc)
                   ((atom x) (cons x acc))
                   (t (rec
                       (car x)
                       (rec (cdr x) acc))))))
    (rec x nil)))


(defun circularp (list)
  "Checks to see if a list is circular."
  ;; from stackoverflow
  (loop for tortoise on list
     for hare on (cdr list) by #'cddr
     thereis (eq tortoise hare)))

(export 'de-ring)
(defun de-ring (ring)
  (let ((idx 0))
    (loop for cell on (cdr ring) do
         (incf idx)
         (when (eq cell ring) (return)))
    (subseq ring 0 idx)))

(defun copy-circ (circle)
  (circular (copy-seq (de-ring circle))))

(defun circlen (circ)
  (length (de-ring circ)))

(defun subcirc (circ i j)
  (values (subseq circ i (if (< j i) (+ j (circlen circ)) j))
          (subseq circ j (if (< i j) (+ i (circlen circ)) i))))
  


(defun circular (list)
  (setf *print-circle* t)
  (setf (cdr (last list)) list))

(defun make-iter (list)
  (lambda ()
    (pop list)))


(defun rnd (integer)
  (if (zerop integer) 0 (random integer)))

(export 'pick)
(defmacro pick (seq)
  `(elt ,seq (random (length ,seq))))


        
(defun timestring ()
  (let ((day-names
         '("Monday" "Tuesday" "Wednesday"
           "Thursday" "Friday" "Saturday"
           "Sunday")))
    (multiple-value-bind
          (second minute hour date month year day-of-week dst-p tz)
        (get-decoded-time)
      (declare (ignore dst-p))
      (format nil ";; It is now ~2,'0d:~2,'0d:~2,'0d on ~a, ~d/~2,'0d/~d (GMT~@d)"
              hour
              minute
              second
              (nth day-of-week day-names)
              date
              month
              year
              (- tz)))))

(defun guard-val (val minval maxval)
  "Formats values for registers: guards against overflow, and type casts."
  ;;(declare (type rational val minval maxval))
  (let ((sign (if (< val 0) -1 1)))
    ;;(declare (type rational sign))
    (the real (cond ((zerop val) val)
                            ((< (abs val) minval) (* sign minval))
                            ((> (abs val) maxval) (mod val maxval))
                            (t val)))))

(defun safemod (x y)
  (if (zerop y) y
      (mod x y)))


(defun square (x)
  (expt x 2))
