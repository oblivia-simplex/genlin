;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; Data reading functions
;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=


(defpackage :genlin
  (:use :common-lisp))

(in-package :genlin)


;; -- Tic-Tac-Toe Environment)

(let* ((.tictactoe-path. "/home/oblivia/Projects/genetic-exercises/genetic-linear/datasets/TicTacToe/tic-tac-toe-balanced.data")
       (.xob-digits. '((#\x . 2)
                       (#\o . 1)
                       (#\b . 0)))
       (.digits-xob. (mapcar #'(lambda (x) (cons (cdr x) (car x))) .xob-digits.)))
  

  (defun xobstring->numvec (xobstring)
    (let* ((stripped (remove-if #'(lambda (x) (char= #\, x)) xobstring))
           (vec (sub-map (concatenate 'vector stripped) .xob-digits.)))
      vec))
    
  (defun tictactoe->int (xobstring &key (gray t))
    "Reads a csv string represenation of a tictactoe board, and
returns an integer that uniquely represents that board. The integer is
just the board read as a base-3 numeral, with b = 0, x = 1, o = 2."
    (let ((vec (xobstring->numvec xobstring)))
      (if gray (gethash vec *degray-lookup*)
          (read-from-string
           (concatenate 'string "#3r"
                        (coerce (mapcar
                                 #'(lambda (x) (code-char (+ (char-code #\0) x)))
                                 (coerce vec 'list)) 'string))))))
  
  (defun int->tictactoe (int &key (gray t))
    (flet ((convert (n)
             (if gray (gethash n *gray-lookup*)
                 (coerce (mapcar #'(lambda (x) (- (char-code x) (char-code #\0)))
                                 (coerce
                                  (format nil "~3r" n) 'list)) 'vector))))
      (coerce (sub-map (convert int) .digits-xob.) 'string)))
          
  
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
    (fmt-board (concatenate 'string
                            (substitute #\x -1
                                        (substitute #\b 0
                                                    (substitute #\o 1
                                                                lis))))))

  (defun draw-board (key)
    (grayvec->board key))

  (defun tictactoe-val (str)
    (if (equalp str "positive") 1
        -1))

  (defun tictactoe-hash-int (str hashtable &key (gray t))
    "Converts the tictactoe string into a base-3 number, and enters it into the hashtable."
    (let ((keyval (split-at-label str)))
      (setf (gethash (vector (tictactoe->int (car keyval) :gray gray))
                     hashtable)
            (tictactoe-val (cdr keyval)))))

  (defun tictactoe-hash-vec (str hashtable)
    "Converts the tictactoe string into a list of integers over
-1,0,1 and enters them into the hashtable."
    (let ((k (car (split-at-label str)))
          (v (cdr (split-at-label str))))
      (setf (gethash
             (coerce (mapcar #'1- (eval (read-from-string (concatenate 'string "'(" (substitute #\1 #\b (substitute #\0 #\x (substitute #\2 #\o (substitute #\space #\, k)))) ")")))) 'vector) hashtable)
            (tictactoe-val v))))

  (defun ttt-datafile->hashtable (&key (filename .tictactoe-path.) (int t) (gray t))
    (format t "FILENAME: ~a~%" filename)
    (let ((ht (make-hash-table :test 'equalp))
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


  ;; this report function is turning into an awful piece of spaghetti code!
  (defun ttt-classification-report (&key (crt *best*) (ht) (out '0))
    (print-creature crt)
    (let ((seq (creature-eff crt))
          (correct 0)
          (incorrect 0)
          (failures '())
          (fitf (if (equalp '(0) out)
                    'FB1
                    'FB3)))
      (labels ((deneg (n)
                 (abs n)) ;; sometimes I just drop negative numbers.
               (val-idx (v)
                 (if (< v 0) 0 1))
               (fb3-correct (o v)
                 (< (deneg (elt o (val-idx v)))
                    (deneg (elt o (! (val-idx v))))))
               (fb1-correct (o v)
                 (> (* v (car o)) 0)))
        (loop for k being the hash-keys in ht using (hash-value v) do
             (let* ((i (elt k 0))
                    (output (execute-creature crt
                                              :input k
                                              :output out
                                              :debug t))
                    (sum (reduce #'+ (mapcar #'deneg output)))
                    (certainties
                     (mapcar #'(lambda (x) (* (divide x sum) 100)) output)))
               (format t "~%~a~%" (int->board i))
             (cond ((eq fitf 'FB1)
                    (cond ((fb1-correct output v)
                           (format t "CORRECTLY CLASSIFIED ~a -> ~f~%~%"
                                     i (car output))
                           (incf correct))
                          (t
                           (format t "INCORRECTLY CLASSIFIED ~a -> ~f~%~%"
                                   i (car output))
                           (incf incorrect)
                           (push k failures))))
                    ((eq fitf 'FB3)
                     (format t "X WINS:  ~f%~%X LOSES: ~f%~%"
                             (car certainties) (cadr certainties))
                     (cond ((fb3-correct output v)
                            (incf correct)
                            (format t "CORRECTLY CLASSIFIED~%"))
                           (t (incf incorrect)
                              (format t "INCORRECTLY CLASSIFIED: X ~s~%"
                                      (if (< v 0) 'LOST 'WON))
                              (push k failures))))))))
  
        (hrule)
        (format t "FAILURES:~%")
        (hrule)
        (loop for fail in failures do
             (format t "~%CODE ~d~%~a~%" fail (int->board (aref fail 0))))
        
        (hrule)
        (format t "TOTAL CORRECT:   ~d~%TOTAL INCORRECT: ~d~%"
                correct incorrect)
        (hrule)
        (values (cons correct incorrect)
                failures))) ;; returns failures, as a convenience for retraining
    

  (defun show-search-space (ht &key (upto #3r222222222))
    (let ((y #\@)
          (n #\.))
      (dotimes (i upto)
        (if (and (gethash (vector i) ht) (= (gethash (vector i) ht) 1))
            (format t "~c" y)
            (format t "~c" n)))))


  (defun deterministic-ttt-eval (xobstring &key (winfor #\o))
    "A deterministic, always-correct (barring bugs) tic-tac-toe evaluator,
which can be used to provide more training cases for the GP."
    (let* ((grid (xobstring->numvec xobstring))
           (streaks '((0 1 2)
                      (3 4 5)
                      (6 7 8)
                      (0 3 6)
                      (1 4 7)
                      (2 5 8)
                      (0 4 8)
                      (2 4 6)))
           (winner nil))
      (loop for streak in streaks do
           (let* ((in-streak
                  (remove-duplicates (mapcar #'(lambda (x) (elt grid x)) streak)))
                 (xobstreak (mapcar #'(lambda (y) (cdr (assoc y .digits-xob.)))
                                    in-streak)))
             (when (equal `(,winfor) xobstreak)
               (setf winner winfor )
               (return))))
      winner))
             

  (defun xobstring->csv (xobstring)
      (let ((positive (deterministic-ttt-eval xobstring))
            (xoblist (coerce xobstring 'list))
            (commas '(#\, #\, #\, #\, #\, #\, #\, #\, #\,)))
        (concatenate 'string (flatten (mapcar #'list xoblist commas))
                     (if positive "positive" "negative"))))

  (defun endgame-generator (&key (positive 100) (negative 100))
    (let ((poscount 0)
          (negcount 0)
          (results '()))
      (loop repeat #3r1000000000 do
           (let* ((int (random #3r1000000000))
                  (xobstring (int->tictactoe int))
                  (xcount (count #\x xobstring))
                  (ocount (count #\x xobstring)))
             (when (and (>= negcount negative)
                        (>= poscount positive))
               (return))
             (when (and (>= xcount 3) (< (abs (- xcount ocount)) 2))
               (cond ((deterministic-ttt-eval xobstring)
                      (incf poscount)
                      (if (<= poscount positive)
                          (push (xobstring->csv xobstring) results)))
                     (t (incf negcount)
                        (if (<= negcount negative)
                            (push (xobstring->csv xobstring) results)))))))
      results))

  (defun write-ttt-csv (&key (filename "generated-ttt.csv")
                          (positive 1000)
                          (negative 1000))
    (with-open-file (fd filename :direction :output)
                              
      (loop for line in (endgame-generator
                         :positive positive
                         :negative negative) do
           (write-line line fd))
      (close fd)))
    
    
) ;; end of tictactoe environment
