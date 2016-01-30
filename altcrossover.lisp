;; This buffer is for notes you don't want to save, and for Lisp evaluation.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer.

(defun crossover (p0 p1)
  (declare (type creature p0 p1))
  (declare (optimize (speed 1)))

  (let* (;;(lp0 (length (creature-seq p0)))
         ;;(lp1 (length (creature-seq p1)))
         ;;(ovum (creature-seq p0))
         ;;(sperm (creature-seq p1))
         (ovum (circular (coerce (creature-seq p0) 'list)))
         (sperm (circular (coerce (creature-seq p1) 'list)))
;;         (parents (sort (list p00 p01) #'(lambda (x y) (< (length x) (length y)))))
;;         (father (car parents))  ;; we trim off the car, which holds fitness
;;         (mother (cadr parents)) ;; let the father be the shorter of the two
;;         (r-align (random 2)) ;; 1 or 0
;;         (offset (* r-align (- (length mother) (length father))))
         (fzygote)
         (mzygote)
         (children)
         ;; (xx)
         ;; (xy)
         ;; (yx)
         ;; (yy)
         (ov-i (random (length (creature-seq p0))))
         (ov-j (random (length (creature-seq p0))))
         (sp-i (random (length (creature-seq p1))))
         (sp-j (random (length (creature-seq p1))))
         (genes0 (multiple-value-bind (xx xy)
                     (subcirc ovum ov-i ov-j) (cons xx xy)))
         (genes1 (multiple-value-bind (yx yy)
                     (subcirc sperm sp-i sp-j) (cons yx yy)))

    (setf fzygote (concatenate 'list (cdr genes0) (car genes1)))
    (setf mzygote (concatenate 'list (cdr genes1) (car genes0)))

    ;; half guessing here
    ;;(loop repeat ov-j do (pop fzygote))
    ;;(loop repeat sp-j do (pop mzygote))
    
    
    
    ;;    (format t "mother: ~a~%father: ~a~%daughter: ~a~%son: ~a~%"
    ;;            mother father daughter son)
    (setf children (list (make-creature
                          :seq (coerce (de-ring mzygote) 'vector))
                         (make-creature
                          :seq (coerce (de-ring fzygote) 'vector))))))
