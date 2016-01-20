;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; Data reading functions
;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=


(defpackage :genetic.linear
  (:use :common-lisp))

(in-package :genetic.linear)

(defparameter *tictactoe-path* "/home/oblivia/Projects/genetic-exercises/genetic-linear/datasets/TicTacToe/tic-tac-toe-balanced.data")


(defun reverse-hash-table (hash-table &key (test 'eql))
  (let ((newtable (make-hash-table :test test)))
    (loop for k being the hash-keys in hash-table do
         (setf (gethash (gethash k hash-table) newtable) k))
    newtable))

(defparameter *xob-digits*
  (let ((ht (make-hash-table)))
    (setf (gethash #\x ht) 2)
    (setf (gethash #\o ht) 1)
    (setf (gethash #\b ht) 0)
    ht))

(defparameter *digits-xob*
    (reverse-hash-table *xob-digits*))                   

(defparameter *gray-lookup*
  (let ((grayhash (make-hash-table)))
    (loop for i from 0 to #3r222222222 do
         (setf (gethash i grayhash) (to-gray-vec 3 9 i)))
    grayhash))

(defparameter *degray-lookup*
  (reverse-hash-table *gray-lookup* :test 'equalp))

(defun sub-map (seq ht)
  "Substitutes elements in seq according to hashtable ht. Keys and vals of ht 
must be disjoint."
  (let ((el (concatenate 'list (remove-duplicates seq)))
        (copy (copy-seq seq)))
    (loop for e in el do
         (setf copy (substitute (gethash e ht) e copy)))
    copy))

(defun tictactoe->int (xobstring &key (gray t))
  "Reads a csv string represenation of a tictactoe board, and returns an integer that uniquely represents that board. The integer is just the board read as a base-3 numeral, with b = 0, x = 1, o = 2."
  (let* ((stripped (remove-if #'(lambda (x) (char= #\, x)) xobstring))
         (vec (sub-map (concatenate 'vector stripped) *xob-digits*)))
    (if gray (gethash vec *degray-lookup*)
        (read-from-string (concatenate 'string "#3r"
                                       (coerce (mapcar #'(lambda (x) (code-char (+ (char-code #\0) x)))
                                                       (coerce vec 'list)) 'string))))))

(defun int->tictactoe (int &key (gray t))
  (flet ((convert (n)
           (if gray (gethash n *gray-lookup*)
               (coerce (mapcar #'(lambda (x) (- (char-code x) (char-code #\0)))
                               (coerce (format nil "~3r" n) 'list)) 'vector))))
    (coerce (sub-map (convert int) *digits-xob*) 'string)))

(defun fmt-board (string)
  (let* ((row " ~c | ~c | ~c ~%")
         (lin "---+---+---~%")
         (str (substitute #\space #\b string))
         (board (concatenate 'string row lin row lin row)))
    (format nil board (char str 0) (char str 1) (char str 2)
            (char str 3) (char str 4) (char str 5)
            (char str 6) (char str 7) (char str 8))))
    

(defun int->board (int &key (gray t))
  (format nil "~a" (fmt-board (int->tictactoe int :gray gray))))

(defun grayvec->board (vec)
  (int->board (aref vec 0)))

(defun list->board (lis)
  (fmt-board (concatenate 'string (substitute #\x -1 (substitute #\b 0 (substitute #\o 1 lis))))))
                                                      
(defun draw-board (key)
  (grayvec->board key))

(defun split-at-label (str)
  (let ((lastcomma-idx (position #\, str :from-end t)))
    (cons (subseq str 0 lastcomma-idx) (subseq str (1+ lastcomma-idx)))))

(defun tictactoe-val (str)
  (if (equalp str "positive") 1
      -1))

(defun tictactoe-hash-int (str hashtable &key (gray t))
  "Converts the tictactoe string into a base-3 number, and enters it into the hashtable."
  (let ((keyval (split-at-label str)))
    (setf (gethash (vector (tictactoe->int (car keyval) :gray gray)) hashtable)
          (tictactoe-val (cdr keyval)))))

(defun tictactoe-hash-vec (str hashtable)
  "Converts the tictactoe string into a list of integers over
-1,0,1 and enters them into the hashtable."
  (let ((k (car (split-at-label str)))
        (v (cdr (split-at-label str))))
  (setf (gethash
         (coerce (mapcar #'1- (eval (read-from-string (concatenate 'string "'(" (substitute #\1 #\b (substitute #\0 #\x (substitute #\2 #\o (substitute #\space #\, k)))) ")")))) 'vector) hashtable)
         (tictactoe-val v))))

(defun ttt-datafile->hashtable (filename &key (int t) (gray t))
  (format t "FILENAME: ~a~%" filename)
  (let ((ht (if int (make-hash-table :test 'equalp)
                (make-hash-table :test 'equalp)))
        (in (open filename :if-does-not-exist nil)))
    (when in
      (loop for line = (read-line in nil)
         while line do
           (if int (tictactoe-hash-int line ht :gray gray)
               (tictactoe-hash-vec line ht))
           (and *debug* (format t "READ|  ~a~%" line)))
      (close in))
    ht))

(defun show-all-boards (&optional (int 0))
  (when (<= int #3r222222222)
    (format t "~d = ~3r ~%~a~%" int int (int->board int)) 
    (show-all-boards (1+ int))))


(defun ttt-classification-report (crt &optional (ht *ht*))
  (format t "REPORT FOR ~a~%=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=~%" crt)
  (dbg 'on)
  (let ((seq (creature-eff crt))
        (correct 0)
        (incorrect 0)
        (failures '()))
    (loop for k being the hash-keys in ht do
         (let ((i (aref k 0))
               (f (car (execute-sequence seq :input k))))
           (format t "~%~a~%" (int->board i))
         (cond ((> (* (gethash k ht) f) 0)
                (format t "CORRECTLY CLASSIFIED ~a -> ~f~%~%" i f)
                (incf correct))
               ((< (* (gethash k ht) f) 0)
                (format t "INCORRECTLY CLASSIFIED ~a -> ~f~%~%" i f)
                (incf incorrect)
                (push k failures))
               (t (format t "WHO'S TO SAY? ~a -> ~f~%~%" i f)
                  (push k failures))))
         (hrule))
    (format t "FAILURES:~%")
    (hrule)
    (loop for fail in failures do
         (format t "~%CODE ~d~%~a~%" fail (int->board (aref fail 0))))
    (dbg 'off)
    (hrule)
    (format t "TOTAL CORRECT:   ~d~%TOTAL INCORRECT: ~d~%"
            correct incorrect)
    (hrule)
    failures)) ;; returns failures, as a convenience for retraining


(defun show-search-space (ht &key (upto #3r222222222))
  (let ((y #\@)
        (n #\.))
    (dotimes (i upto)
      (if (and (gethash (vector i) ht) (= (gethash (vector i) ht) 1))
          (format t "~c" y)
          (format t "~c" n)))))
