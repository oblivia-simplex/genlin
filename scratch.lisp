
;; bad code repetition here. once it's working, merge it with
;; the main evolve loop, and parameterize away the difference between
;; evolving hives and evolving individuals. 
(defun evolve-hives (&key (hive-ring)
                       (h-rounds)
                       (h-smethod #'tournement!))
  ;; no real reason to use lexicase for hives
  (let ((h-stopwatch (make-stopwatch)))
    (funcall h-stopwatch 'set)
    (handler-case
        (loop for i from 1 to (* h-rounds *number-of-islands*) do
             (let ((hive (pop hive-ring)))
               (labels ((dispatcher ()
                          (when *parallel*
                            (sb-thread:grab-mutex
                             (hive-lock hive)))
                          (funcall h-smethod hive)
                          (when *parallel*
                            (sb-thread:release-mutex (hive-lock hive))))
                        (dispatch ()
                          (incf (hive-era hive))
                          (if *parallel*
                              (sb-thread:make-thread #'dispatcher)
                              (dispatcher))))
                 (dispatch) ;; main event
               
              ))))))


;;; alternative coverage algo
(defun islands->hives (island-ring)
  (let ((hive-ring (loop repeat *number-of-islands* collect
                        (make-hive :lock (sb-thread:make-mutex
                                          :name "hive lock")
                                   :logger (make-logger)
                                   :best (make-creature :fit 0)
                                   :era 0
                                   :of *number-of-islands*))))
    
    (loop
       for isle in (de-ring island-ring)
       for hive in hive-ring do
         (setf (hive-workers hive)
               (extract-population-from-coverage isle))
         (setf (hive-id hive) (island-id isle))
         (setf (hive-queens hive) (loop repeat *queen-population-size*
                                     collect
                                       (spawn-creature
                                        (+ *min-len*
                                           (random *max-start-len*))))))
    (circular hive-ring)))
;; (setf *stop* t)



;; (defun greedy-mutation (crt &key (fitfunc))
;;   ;; greedily searches for the single mutation that will increase fitness
;;   ;; the most.
;;   (let ((newseq (copy-seq seq))
;;         (scores '()))
;;     (when (length (> (creature-eff crt) 0))
;;       (loop for i from 0 to (1- (length newseq)) do
;;            (let ((oldinst (elt newseq i)))
;;              (when (member oldinst (creature-eff seq))
;;                (loop for newinst from 0 to (1- (expt 2 *wordsize*)) do
;;                     (setf (elt i newseq) newinst)
;;                     (acons (cons (copy-seq newseq) (funcall fitfunc newseq) scores)))
;;                (setf (elt newseq i) oldinst)))) ;; restore old instruction
;;       ;; then sort scores by max fit and take fittest.
;;       )))
