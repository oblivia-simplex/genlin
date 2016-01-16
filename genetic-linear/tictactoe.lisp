;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; Data reading functions
;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=


(defpackage :genetic.linear
  (:use :common-lisp))

(in-package :genetic.linear)


(defun tictactoe->int (xobstring)
  "Reads a csv string represenation of a tictactoe board, and returns an integer that uniquely represents that board. The integer is just the board read as a base-3 numeral, with b = 0, x = 1, o = 2."
  (let ((stripped (remove-if #'(lambda (x) (char= #\, x)) xobstring)))
    ;;(print stripped)    
    (read-from-string (concatenate 'string "#3r" (substitute #\0 #\b (substitute #\1 #\x (substitute #\2 #\o stripped)))))))

(defun int->tictactoe (int)
  (let ((base3 (format nil "~3,9,'0r" int)))
    (substitute #\b #\0 (substitute #\x #\1 (substitute #\o #\2 base3)))))

(defun fmt-board (string)
  (let* ((row " ~c | ~c | ~c ~%")
         (lin "---+---+---~%")
         (str (substitute #\space #\b string))
         (board (concatenate 'string row lin row lin row)))
    (format nil board (char str 0) (char str 1) (char str 2)
            (char str 3) (char str 4) (char str 5)
            (char str 6) (char str 7) (char str 8))))
    

(defun int->board (int)
  (format nil "~a" (fmt-board (int->tictactoe int))))

(defun list->board (lis)
  (fmt-board (concatenate 'string (substitute #\x -1 (substitute #\b 0 (substitute #\o 1 lis))))))
                                                      
(defun draw-board (key)
  (if (listp key) (list->board key)
      (int->board key)))

(defun split-at-label (str)
  (let ((lastcomma-idx (position #\, str :from-end t)))
    (cons (subseq str 0 lastcomma-idx) (subseq str (1+ lastcomma-idx)))))

(defun tictactoe-val (str)
  (if (equalp str "positive") 1
      -1))

(defun tictactoe-hash-int (str hashtable)
  "Converts the tictactoe string into a base-3 number, and enters it into the hashtable."
  (let ((keyval (split-at-label str)))
    (setf (gethash (vector (tictactoe->int (car keyval))) hashtable)
          (tictactoe-val (cdr keyval)))))

(defun tictactoe-hash-vec (str hashtable)
  "Converts the tictactoe string into a list of integers over
-1,0,1 and enters them into the hashtable."
  (let ((k (car (split-at-label str)))
        (v (cdr (split-at-label str))))
  (setf (gethash
         (coerce (mapcar #'1- (eval (read-from-string (concatenate 'string "'(" (substitute #\1 #\b (substitute #\0 #\x (substitute #\2 #\o (substitute #\space #\, k)))) ")")))) 'vector) hashtable)
         (tictactoe-val v))))

(defun datafile->hashtable (filename &key (int nil))
  (format t "FILENAME: ~a~%" filename)
  (let ((ht (if int (make-hash-table)
                (make-hash-table :test 'equal)))
        (in (open filename :if-does-not-exist nil)))
    (when in
      (loop for line = (read-line in nil)
         while line do
           (if int (tictactoe-hash-int line ht)
               (tictactoe-hash-list line ht))
           (format t "READ|  ~a~%" line))
      (close in))
    ht))

(defun show-all-boards (int)
  (when (<= int #3r222222222)
    (format t "~d = ~3r ~%~a~%" int int (int->board int)) 
    (show-all-boards (1+ int))))



