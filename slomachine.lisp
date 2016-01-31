(in-package :genlin)

;;; This copy only exists for the sake of carefree tinkering. Eventually,
;;; it will be merged with r-machine.lisp, the difference between the two
;;; coming down to a choice of parameters.

(defparameter *machine* :slomachine)

;;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;;; SLOMACHINE (Store/LOad-machine) is an alternative architecture to be
;;; used for the genetic programming engine in GENLIN. The most signficant
;;; difference between SLOMACHINE and R-MACHINE (its predecessor) is that
;;; SLOMACHINE introduces support for STO and LOD instructions, allowing
;;; a creature to inspect its own source code and load bytes of source
;;; into its registers with LOD (giving it a nutrious, autophagic diet of
;;; fresh constants) and to rewrite its own code segment with STO. This can
;;; be used either as a simple means of storing data for later, when writing
;;; to past instructions (instructions at indicies prior to the current
;;; programme counter) or as a control structure, when it is used to write
;;; to its own future.
;;;
;;; Preliminary trials have shown GPs running on SLOMACHINE --
;;; particularly with the tic-tac-toe data set -- to converge up to 40
;;; times faster than they did on R-MACHINE, and result in much
;;; simpler and more compact code. (On ttt, this meant a convergence by
;;; generation 200 or so, rather than generation 5000, when running in
;;; tournement mode.)
;;;
;;; On a smaller scale, it is somewhat slower than R-MACHINE, however,
;;; and REMOVE-INTRONS is unable to streamline the code before
;;; execution, since we may need to know what the input is before we
;;; can calculate the effective registers. This isn't a totally
;;; insurmountable obstacle, however, but an improved remove-introns
;;; algorithm remains to be written.
;;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; Declaim inline functions for speed
;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

(declaim (inline guard-val))

(declaim (inline MOV DIV MUL XOR CNJ DIS PMD ADD SUB MUL JLE)) 

;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; master update function for VM parameters
;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

;; --- Operations: these can be tweaked independently of the 
;; --- fields above, so long as their are at least (expt 2 *opf*)
;; --- elements in the *operations* vector. 

(defun MOV (&rest args)
  "Copy the contents in SRC to register DST."
  (declare (type (or null (cons rational)) args))
  (the rational (coerce (car args) 'rational)))

(defun DIV (&rest args)
  "A divide-by-zero-proof division operator."
  (declare (type (or null (cons rational)) args))
  (the rational (if (some #'zerop args) 0
                (/ (car args) (cadr args)))))
                          
(defun XOR (&rest args) ;; xor integer parts
  "Performs a bitwise XOR on SRC and DST, storing the result in DST."
  (declare (type (or null (cons rational)) args))
  (check-type args (cons rational))
  (the rational (coerce (logxor (floor (car args)) (floor (cadr args))) 'rational)))

(defun CNJ (&rest args)
  "Performs a bitwise AND on SRC and DST, storing the result in DST."
  (declare (type (or null (cons rational)) args))
  (check-type args (cons rational))
  (logand (floor (car args)) (floor (cadr args))))

(defun DIS (&rest args)
  "Performs a bitwise OR on SRC and DST, storing the result in DST."
  (declare (type (or null (cons rational)) args))
  (check-type args (cons rational))
  (the rational (lognot (logand  (lognot (floor (car args)))
                               (lognot (floor (cadr args)))))))

(defun PMD (&rest args)
  "Performs a protected SRC MOD DST, storing the result in DST."
  (declare (type (or null (cons rational)) args))
  (check-type args (cons rational))
  (the rational (if (some #'zerop args)
                (car args)
                (mod (car args) (cadr args)))))

(defun ADD (&rest args)
  "Adds the rational-valued contents of SRC and DST, storing result in DST."
  (declare (type (or null (cons rational)) args))
  (check-type args (cons rational))
  (the rational (+ (car args)
               (cadr args))))

(defun SUB (&rest args)
  "Subtracts DST from SRC, storing the value in DST."
  (declare (type (or null (cons rational)) args))
  (check-type args (cons rational))
  (- (car args) (cadr args)))

(defun MUL (&rest args)
  "Multiplies SRC and DST, storing the value in DST."
  (declare (type (or null (cons rational)) args))
  (check-type args (cons rational))
  (* (car args) (cadr args)))

(defun JLE (&rest args) ;; CONDITIONAL JUMP OPERATOR
  "Stub. Really just here for consistent pretty printing."
  (declare (type (or null (cons rational)) args))
  (check-type args (cons rational))
  (if (<= (car args) (cadr args))
      (1+ (caddr args))
      (caddr args)))

(defun LOD (&rest args) ;; CONDITIONAL JUMP OPERATOR
  "Stub. Really just here for consistent pretty printing."
  (declare (ignore args))
  (print "You shouldn't be seeing this.")
  0)

(defun STO (&rest args) ;; CONDITIONAL JUMP OPERATOR
  "Stub. Really just here for consistent pretty printing."
  (declare (ignore args))
  (print "You shouldn't be seeing this.")
  0)



(defparameter *operations*
  (concatenate 'vector
               (vector  #'DIV #'MUL #'SUB #'ADD   ;; basic operations    
                        #'PMD ;;                  ;; end of register ops
                        ;; --------------------
                        #'JLE #'LOD #'STO         ;; placeholders
                        ;; -------------------- 
                        #'XOR #'CNJ #'DIS #'MOV)   ;; extended operations 
               (loop repeat (expt 2 *opcode-bits*)
                  collect #'MOV))

"Instruction set for the virtual machine. Only the first
     2^*opcode-bits* will be used." )

(defparameter *reg-op-keys*
  '(:DIV :MUL :SUB :ADD :PMD :XOR :CNJ :DIS :MOV))

(defparameter *jmp-op-keys*
  '(:JLE))

(defparameter *store-op-keys*
  '(:STO))

(defparameter *load-op-keys*
  '(:LOD))

(defvar *operation-keys*)

(defun shuffle-operations ()
  (setf *operations* (coerce (shuffle (coerce *operations* 'list)) 'vector)))

;; this whole thing should be refactored out of slomachine and r-machine,
;; to a more central location
(defun update-dependent-machine-parameters ()

  (setf *operation-keys*
        (pairlis
         (mapcar #'(lambda (x) (intern x :keyword))
                 (mapcar #'func->string (coerce *operations* 'list)))
         (coerce *operations* 'list)))

;; Not quite ready yet. 
  
  (when *opstring*
    (setf *ops* (read-from-string
                 (concatenate 'string "("
                              (substitute #\space #\, *opstring*) ")"))))

  (when *ops*
    (setf *ops* (sort *ops* #'(lambda (x y) (cond ((and (member x *reg-op-keys*)
                                                   (member y *jmp-op-keys*))
                                              t)
                                             ((and (member x *reg-op-keys*)
                                                   (member y *load-op-keys*))
                                              t)
                                             ((and (member x *reg-op-keys*)
                                                   (member y *store-op-keys*))
                                              t)
                                             ((and (member x *jmp-op-keys*)
                                                   (member y *load-op-keys*))
                                              t)
                                             ((and (member x *jmp-op-keys*)
                                                   (member y *store-op-keys*))
                                              t)
                                             ((and (member x *load-op-keys*)
                                                   (member y *store-op-keys*))
                                              t)
                                             (t nil)))))
    (setf *operations*
          (concatenate 'vector
                       (mapcar #'(lambda (x) (cdr (assoc x *operation-keys*)))
                               *ops*)
                       (loop repeat (expt 2 *opcode-bits*) collect #'MOV))))
  
  (setf *REGIOPS* (length (remove-if-not #'(lambda (x) (member x *reg-op-keys*)) *ops*)))

  (setf *JUMPOPS* (+ *REGIOPS* (length *jmp-op-keys*)))

  (setf *LOADOPS* (+ *JUMPOPS* (length *load-op-keys*)))

  (setf *STOROPS* (+ *LOADOPS* (length *store-op-keys*)))
  
  (setf *destination-register-bits*
         (max (ceiling (log (how-many-output-registers?) 2))
              *destination-register-bits*))

   (setf *source-register-bits*
         (max (ceiling (log (+ (how-many-input-registers?)
                               (how-many-output-registers?)) 2))
              *source-register-bits*))
  
  (setf *wordsize*
        (+ *opcode-bits* *source-register-bits*
           *destination-register-bits* ))

  (setf *machine-fmt* (format nil "~~~D,'0b  ~~a" *wordsize*))
  
  (setf *max-inst*
    (expt 2 *wordsize*)) ;; upper bound on inst size
  
  (setf *opbits*
    (byte *opcode-bits* 0))
  
  (setf *srcbits*
    (byte *source-register-bits* *opcode-bits*))
  
  (setf *dstbits*
        (byte *destination-register-bits*
              (+ *source-register-bits* *opcode-bits*)))

  (setf *default-input-reg*
        (concatenate 'vector
               (loop for i from 1 to (- (expt 2 *source-register-bits*)
                                        (expt 2 *destination-register-bits*))
                  collect (expt -1 i))))

  (setf *default-registers*
        (concatenate 'vector #(0) (loop for i from 2 to
                                       (expt 2 *destination-register-bits*)
                                     collect (expt -1 i))))
  
  (setf *pc-idx*
        (+ (length *default-registers*) (length *default-input-reg*)))
  
  (setf *initial-register-state*
        (concatenate 'vector
                     *default-registers*
                     *default-input-reg*
                     #(0))) ;; PROGRAMME COUNTER

  (setf *input-start-idx* (length *default-registers*))

  (setf *input-stop-idx*
      (+ *input-start-idx* (length *default-input-reg*))))

;;(update-dependent-machine-parameters)

;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; Functions for extracting fields from the instructions
;; and other low-level machine code operations.
;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

(declaim (inline src? dst? op?))

(defun src? (inst)
  "Returns the source address."
  (declare (type (signed-byte 32) inst))
  (declare (type (cons (signed-byte 32)) *srcbits*))
  (the signed-byte (ldb *srcbits* inst)))

(defun dst? (inst)
  "Returns the destination address."
  (declare (type (signed-byte 32) inst))
  (declare (type (cons (signed-byte 32)) *dstbits*))
  (the signed-byte (ldb *dstbits* inst)))

(defun op? (inst)
  "Returns the actual operation, in the case of register ops, in 
the form of a function."
  (declare (type (signed-byte 32) inst))
  (declare (type (cons (signed-byte 32)) *opbits*))
  (declare (type (simple-array function) *operations*))
  (the function (aref *operations* (ldb *opbits* inst))))

(defun opc? (inst)
  "Returns the numerical opcode."
  (declare (type (signed-byte 32) inst))
  (declare (type (cons (signed-byte 32)) *opbits*))
  (the signed-byte (ldb *opbits* inst)))

 (defun jmp? (inst) ;; ad-hoc-ish...
   (declare (type fixnum inst))
   (the boolean (equalp (op? inst) #'JLE)))

;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

(defparameter =history=
  (let ((hist '()))
    (lambda (&optional (entry nil))
      (cond ((eq entry 'CLEAR) (setf hist '()))
            ((eq entry 'ALL) hist)
            (entry (push entry hist))
            (t (car hist))))))

(defun history-push-test (seq reg)
  (funcall =history= (cons seq reg)))

(defun history-flush ()
  (funcall =history= 'CLEAR))

(defun history-print (&optional len)
  (let ((h (funcall =history=)))
    (print (subseq h 0 len))))

(defun disassemble-history (&key (all 'all) (len 1) (static nil))
  "If passed a sequence in the static field, disassemble it without
any reference to register states. If not, disassemble the last len
entries in the history stack, tracking changes to the registers."
  (let* ((history (funcall =history= all))
         (story (if static
                    (coerce static 'list)
                    (reverse (subseq history 0 len)))))

    (loop for line in story do
         (let* ((registers (if static
                               (copy-seq *initial-register-state*)
                               (getf line :reg)))
                (inst (if static line (getf line :inst)))
                (addr)
                (data)
                (regnum))
           (format t *machine-fmt* ;; needs to be made flexible. 
                   inst
                   (inst->string inst))
           (when (not static) (format t "  (~3d ) " (getf line :pc))
                 (cond ((< (opc? inst) *REGIOPS*)
                        (format t " ;; now R~d = ~f; R~d = ~f~%" 
                                (src? inst)
                                (aref registers (src? inst))
                                (dst? inst)
                                (aref registers (dst? inst))))
                       ((< (opc? inst) *JUMPOPS*) 
                        (if (<= (aref registers (src? inst))
                                (aref registers (dst? inst)))
                            (format t " ;; R~d <= R~d, so incrementing %PC~%"
                                    (src? inst) (dst? inst))
                            (format t " ;; R~d > R~d, no jump~%"
                                    (src? inst) (dst? inst))))
                       ((< (opc? inst) *LOADOPS*)
                        (setf addr (mod (floor
                                         (aref registers (src? inst)))
                                        (length (getf line :seq)))
                              data (aref (getf line :seq) addr)
                              regnum (dst? inst))
                        (format t " ;; loading ~d from @~d into R~d~%"
                                data addr regnum))
                       ((< (opc? inst) *STOROPS*)
                        (setf addr (mod (floor (aref registers (dst? inst)))
                                        (length (getf line :seq)))
                              data (ldb (byte *wordsize* 0)
                                        (floor (aref registers
                                                     (src? inst))))
                              regnum (src? inst))
                        (format t " ;; storing ~d from R~d in @~d~%"
                                data regnum addr)
                        (when (>= addr (getf line :pc))
                          (format t "*** SEQUENCE REWRITING ITSELF ***~%")))
                       (t (format t "  WTF?"))))
           (if static
               (format t "~%"))))
  (if static (hrule))))

;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; The engine room
;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

(defun guard-val (val minval maxval)
  "Formats values for registers: guards against overflow, and type casts."
  (declare (type rational val minval maxval))
  (let ((sign (if (< val 0) -1 1)))
    (declare (type rational sign))
    (the rational (coerce (cond ((< (abs val) minval) (* sign minval))
                             ((> (abs val) maxval) (mod val maxval))
                             (t val))
                          'rational))))


  

;; Some anaphoric macros for quickly dispatching instructions
;; and clarifying code, without costly stack operations
;;
;; NB: variables prefixed with the % sigil are free, and should be bound
;; +++ in the environment into which these macros are injected (presumably,
;;     just execute-sequence).

(defmacro exec-reg-inst (inst)
  "Executes a register-manipulation instruction."
  `(setf (aref %reg (dst? ,inst))
         (guard-val (apply (op? ,inst) 
                           (list (aref %reg (src? ,inst))
                                 (aref %reg (dst? ,inst))))
                    *minval* *maxval*)))

(defmacro exec-jmp-inst (inst)
  "Executes a jump-type instruction (anaphorically modifies %pc,
according to operation (op? inst)."
  `(setf %pc
         (guard-val (apply (op? ,inst)
                           (list (aref %reg (src? ,inst))
                                 (aref %reg (dst? ,inst))
                                 %pc))
                    *minval* *maxval*)))

(defmacro exec-load-inst (inst)
  "Loads contents of memory at address indexed by SRC register into
DST register."
  `(setf (aref %reg (dst? ,inst))
         (aref %seq (mod (floor (aref %reg (src? ,inst))) (length %seq)))))

(defmacro exec-store-inst (inst)
  "Stores contents of DST register into memory address indexed by 
SRC register -- potentially corrupting code."
  `(setf (aref %seq (mod (floor (aref %reg (dst? ,inst))) (length %seq)))
         (ldb (byte *wordsize* 0) (floor (aref %reg (src? ,inst))))))

(defun execute-sequence (seq &key (registers *initial-register-state* )
                               (input *default-input-reg*) (output nil)
                               (debug nil))
  "Takes a sequence of instructions, seq, and an initial register
state vector, registers, and then runs the virtual machine, returning
the resulting value in the registers indexed by the integers in the
list parameter, output."
  (declare (type fixnum *input-start-idx* *pc-idx*)
           (inline src? dst? op?)
           (type (cons integer) output)
           (type (signed-byte 32)
                 *regiops* *jumpops*
                 *loadops* *storops*)
           (type (simple-array rational 1) input registers seq)
           (optimize (speed 2)))
  (flet ((save-state (inst reg seq pc)
           (declare (type (simple-array rational 1) reg)
                    (type fixnum inst pc)
                    (type function =history=))
           (funcall =history= (list :inst inst
                                    :reg reg
                                    :seq seq
                                    :pc pc))))
    (let ((%seq (copy-seq seq))
          (%reg (copy-seq registers))
          (%pc 0) ;; why bother putting the PC in the registers?
          (seqlen (length seq))
          (debugger (or debug *debug*)))
      (declare (type (simple-array rational 1) %reg)
               (type fixnum seqlen %pc)
               (type boolean debugger))
      ;; the input values will be stored in read-only %reg
      (setf (subseq %reg *input-start-idx*
                    (+ *input-start-idx* (length input))) input)
      (unless (zerop seqlen)
        (loop do
           ;; (format t "=== ~A ===~%" %reg)
           ;; Fetch the next instruction
             (let ((inst (aref %seq %pc)))
               (declare (type (signed-byte 32) inst))
               ;; Increment the programme counter
               (incf %pc)
               ;; Perform the operation and store the result in [dst]
               (cond ((< (opc? inst) *REGIOPS*) (exec-reg-inst inst))
                     ((< (opc? inst) *JUMPOPS*) (exec-jmp-inst inst))
                     ((< (opc? inst) *LOADOPS*) (exec-load-inst inst))
                     ((< (opc? inst) *STOROPS*) (exec-store-inst inst))
                     (t (exec-reg-inst inst))) ;; NOP or MOV
               ;; Save history for debugger
               (and debugger (save-state inst %reg %seq %pc)
                    (disassemble-history))
               ;; Halt when you've reached the end of the sequence
               (and (>= %pc seqlen)
                    (return)))))
      (and *debug* (hrule)) ;; pretty
      (mapcar #'(lambda (i) (aref %reg i)) output))))


;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; debugging functions
;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  
(defun print-registers (registers)
  (loop for i from 0 to (1- (length registers)) do
       (format t "R~d: ~6f ~c" i (aref registers i) #\TAB)
       (if (= 0 (mod (1+ i) 4)) (format t "~%")))
  (format t "~%"))

(defun inst->string (inst)
  (concatenate 'string
               (format nil "[~a  R~d, R~d]"
                       (func->string (op? inst))
                       (src? inst) (dst? inst))))

(defun disassemble-sequence (seq &key (registers *initial-register-state*)
                                   (input *default-input-reg*)
                                   (static nil))
  (hrule)
  (if static (disassemble-history :static seq)
      (execute-sequence seq :debug t :input input :registers registers))
  (hrule))
