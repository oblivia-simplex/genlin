;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; Data reading functions
;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

(defpackage :genlin
  (:use :common-lisp))

(in-package :genlin)

(defun parse-numeric-csv-key (k)
  (coerce (mapcar #'rational
                  (read-from-string
                   (concatenate 'string "("
                                (substitute #\space #\, k) ")")))
          'vector))


(defun data-hash (line ht)
  (let* ((kv (split-at-label line))
         (raw-k (car kv))
         (raw-v (cdr kv))
         (val (funcall =label-scanner= raw-v))
         (key (parse-numeric-csv-key raw-k)))
    (setf (gethash key ht) val)))

(defun set-out-reg ()
  (setf *out-reg*
        (loop for i from 0 to (1- (length (funcall =label-scanner= 'get)))
           collect i)))

(defun datafile->hashtable (&key filename)
  (cond ((eq *dataset* :tictactoe)
         (ttt-datafile->hashtable :filename filename))
        (t (format t "FILENAME: ~a~%" filename)
           (let ((ht (make-hash-table :test 'equal))
                 (input (open filename :if-does-not-exist nil)))
             (when input
               (loop
                  for line = (read-line input nil)
                  while line do
                    (data-hash line ht)
                    (and *debug* (format t "READ|  ~a~%" line)))
               (close input))
             ;;    (set-out-reg) ;; set the output registers, knowing the classes
             ht))))

(defun attribute-string (vec)
  (let ((str ""))
    (loop for i from 1 to (length vec) do
         (setf str (concatenate 'string str
                                (format nil "ATTRIBUTE ~@R:~C~F~%"
                                        i #\Tab (aref vec (1- i)))))) str))

;; use the helper functions in the fitness section in here, instead. 
(defun data-classification-report  (&key (crt *best*) (ht) (out '(0 1 2)))
  (print-creature crt)
  (let ((seq (creature-eff crt))
        (correct 0)
        (incorrect 0)
        (names (mapcar #'string-upcase (funcall =label-scanner= 'get)))
        (failures '()))
    (loop for k being the hash-keys in ht using (hash-value v) do
         (let* ((output (mapcar #'abs (execute-creature crt
                                                        :debug t
                                                        :input k
                                                        :output out)))
                (sum (reduce #'+ output))
                (certainties (loop for guess in output collect
                                  (* (divide guess sum) 100)))
                (success (= (elt output v)
                            (reduce #'max output))))
           (hrule)
           (format t "~A~%" (attribute-string k))
           (terpri)
           (loop for i from 0 to (1- (length output)) do
                (format t "CLASS ~A: ~f%~%" (elt names i)
                        (elt certainties i)))
           (cond (success
                  (format t "~%CORRECTLY CLASSIFIED AS ~A~%"
                          (elt names v))
                  (incf correct))
                 ((not success)
                  (format t "~%INCORRECTLY CLASSIFIED. ")
                  (format t "WAS ACTUALLY ~A~%"
                          (elt names v))
                  (incf incorrect)
                  (push k failures))))
         (hrule))
    (format t "                            ~@(~R~) Failures:~%"
            (length failures))
    (hrule)
    (loop for fail in failures do
         (format t "CLASS: ~A~%VECTOR: ~a~%"
                 (elt names (gethash fail ht)) fail)
         (format t "~%~A" (attribute-string fail))
         (hrule))
    (hrule)
    (format t "TOTAL CORRECT:   ~d~%TOTAL INCORRECT: ~d~%"
            correct incorrect)
    (hrule)
    (values (cons correct incorrect)
            failures)))

;; moved over here from genlin.lisp:

(defun partition-data (hashtable ratio)
  ;; need to modify this so that the distribution of values in the test
  ;; set is at least similar to the distribution of values in the training
  (let* ((size (hash-table-count hashtable))
         (training (make-hash-table :test 'equalp))
         (testing (make-hash-table :test 'equalp))
         (keys (shuffle (loop for k being the hash-keys in hashtable collect k)))
         (vals)
           (keys-by-val)
         (portions))
    (setf vals (remove-duplicates (loop
                                     for v being the hash-values
                                     in hashtable collect v)))
    (setf keys-by-val (loop for v in vals collect
                           (remove-if-not
                            #'(lambda (x) (equalp (gethash x hashtable) v))
                            keys)))
    (loop
       for klist in keys-by-val do
           (push (round (* ratio  size  (/ (length klist)
                                           size)))
                 portions))
    (setf portions (reverse portions))
    (and *debug* (format t "TOTAL COUNT OF SPECIMENS: ~D~%CLASS COUNTS: ~A~%PORTIONS FOR TRAINING, BY CLASS: ~A~%"
                         size (mapcar #'length keys-by-val) portions))
    (loop
       for keylist in keys-by-val
       for portion in portions do
         (if (some #'null keylist) (format t "FOUND NULL IN ~A~%" keylist))
         (loop for i from 1 to portion do
              (let ((k))
                (setf k (pop keylist))
                (setf (gethash k training)
                      (gethash k hashtable))))
         (loop for k in keylist do
              (setf (gethash k testing)
                    (gethash k hashtable))))
    (cons training testing)))


;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
