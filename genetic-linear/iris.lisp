;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; Data reading functions
;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

(defpackage :genetic.linear
  (:use :common-lisp))

(in-package :genetic.linear)

(let ((.iris-data-path. "/home/oblivia/Projects/genetic-exercises/genetic-linear/datasets/Iris/iris.data"))
  




  (defun parse-iris-key (k)
    (read-from-string (concatenate 'string "#("
                                   (substitute #\space #\, k) ")")))

  (defun iris-hash (line ht)
    (let* ((kv (split-at-label line))
           (raw-k (car kv))
           (raw-v (cdr kv))
           (val (cond ((search "setosa" raw-v) 0)
                      ((search "versicolor" raw-v) 1)
                      ((search "virginica" raw-v) 2)
                      (t (error (format nil "NO MATCH FOR ~a" raw-v)))))
           (key (parse-iris-key raw-k)))
      (setf (gethash key ht) val)))
  
  
  (defun iris-datafile->hashtable (filename &key (int nil))
    (format t "FILENAME: ~a~%" filename)
    (let ((ht (if int (make-hash-table)
                  (make-hash-table :test 'equal)))
          (in (open filename :if-does-not-exist nil)))
      (when in
        (loop for line = (read-line in nil)
           while line do
             (iris-hash line ht)
             (and *debug* (format t "READ|  ~a~%" line)))
        (close in))
      ht))

  (defun iris-data-string (vec)
    (format nil "SEPAL LENGTH: ~fcm~%SEPAL WIDTH ~fcm~%PETAL LENGTH: ~fcm~%PETAL WIDTH: ~fcm~%" (aref vec 0) (aref vec 1) (aref vec 2) (aref vec 3)))

  (defun iris-plot (vec)
    (let ((headings #("SEPAL LENGTH:" "SEPAL WIDTH: " "PETAL LENGTH:"
                      "PETAL WIDTH: ")))
      (loop for i from 0 to (1- (length vec)) collect
           (format t "~a ~f ~v@{~A~:*~}~%" (aref headings i) (aref vec i)
                   (round (* 7.5 (aref vec i))) #\#))))

  (defun iris-classification-report (crt &optional (ht *ht*))
    (format t "REPORT FOR ~a~%=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=~%" crt)
    (dbg 'on)
    (let ((seq (creature-eff crt))
          (correct 0)
          (incorrect 0)
          (failures '())
          (iris-names '(SETOSA VERSICOLOR VIRGINICA)))
      (loop for k being the hash-keys in ht using (hash-value v) do
           (let* ((output (mapcar #'abs (execute-sequence seq :input k)))
                  (sum (reduce #'+ output))
                  (certainties (loop for guess in output collect
                                    (* (div guess sum) 100)))
                  (success (= (elt output v)
                              (reduce #'max output))))
             (iris-plot k)
             (format t "~%")
             (loop for i from 0 to 2 do
                  (format t "~s: ~f%~%" (elt iris-names i)
                          (elt certainties i)))
             (cond (success
                    (format t "~%CORRECTLY CLASSIFIED AS ~s~%"
                            (elt iris-names v))
                    (incf correct))
                   ((not success)
                    (format t "~%INCORRECTLY CLASSIFIED. WAS ACTUALLY A ~s~%"
                            (elt iris-names v))
                    (incf incorrect)
                    (push k failures))))
           (hrule))
      (format t "                            ~@(~r~) Failures:~%"
              (length failures))
      (hrule)
      (loop for fail in failures do
           (format t "~%SPECIES: ~s, VECTOR: ~a~%"
                   (elt iris-names (gethash fail ht)) fail)
           (iris-plot fail))
      (dbg 'off)
      (hrule)
      (format t "TOTAL CORRECT:   ~d~%TOTAL INCORRECT: ~d~%"
              correct incorrect)
      (hrule)
      failures))
  )



