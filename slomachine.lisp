(in-package :genlin)

;;; This copy only exists for the sake of carefree tinkering. Eventually,
;;; it will be merged with r-machine.lisp, the difference between the two
;;; coming down to a choice of parameters.

(defparameter *machine* :slomachine)



;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;;                           INSTRUCTION FIELDS
;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

;; --- Adjust these 3 parameters to tweak the instruction set. ---
;; --- The rest of the parameters will respond automatically   ---
;; --- but due to use of inlining for optimization, it may     ---
;; --- be necessary to recompile the rest of the programme.    ---

;; see if you can put the entire virtual machine in its own environment

(defparameter *opcode-bits* 3
  "The number of bits in an instruction used to determine the operation. 
     Can be set to 1, 2, or 3.")   ;; size

(defparameter *source-register-bits* 3
  "The number of bits used to calculate the source register in each
     instruction. 2^n readable registers will be allocated where n is the
     value of this parameter." )

(defparameter *destination-register-bits* 2
  "The number of bits used to calculate the destination register. If
     left smaller than *source-register-bits* then there will be 2^(n-m)
     read-only registers, where n is the value of *source-register-bits*
     and m is the value of this parameter." )

;;(defparameter *flag-bits* 2)

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


  ;; (declare (type (or null (cons rational)) args))
  ;; (check-type args (cons rational))
  ;; (if (<= (car args) (cadr args))
  ;;     (1+ (caddr args))
  ;;     (caddr args)))

;; we need to add a flag bit or two: one for jump instructions, one for
;; memory oriented instructions (as opposed to register-based). Two more
;; bits to the opcode, then.

;; let's aim for a 16-bit instruction as the standard.

;; load and stor are effectively MOV, with some type coercion.

;; (defun LOD (&rest args)
;;   "Copy the contents in MEM[src] to REG[dst]."
;;   (declare (type (or null (cons rational)) args))
;;   (check-type args (cons rational))
;;   (the integer (floor (car args))))

;; (defun STO (&rest args)
;;   "Copy the contents in REG[src] to MEM[dst]."
;;   (declare (type (or null (cons rational)) args))
;;   (check-type args (cons rational))
;;   (the integer (floor (car args))))

(defun shuffle-operations ()
  (setf *operations* (coerce (shuffle (coerce *operations* 'list)) 'vector)))

;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; Parameters for register configuration.
;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

(defparameter *output-reg* '())

(defparameter *maxval* (expt 2 16)) ;; max val that can be stored in reg

(defparameter *minval* (expt 2 -16)) ;; floor this to 0

;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; Dependent Virtual Machine Vars
;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

(defvar *default-input-reg*)
  
(defvar *default-registers*)

(defvar *pc-idx*)

(defvar *initial-register-state*)

(defvar *input-start-idx*)

(defvar *input-stop-idx*)

(defvar *wordsize*)

(defvar *max-inst*)

(defvar *opbits*)

(defvar *srcbits*)

(defvar *dstbits*)

(defvar *flgbits*)

(defvar *machine-fmt*)

(defun update-dependent-machine-parameters ()

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

  ;; (setf *flgbits*
  ;;       (byte *flag-bits*
  ;;             (+ *destination-registers-bits*
  ;;                *source-register-bits* *opcode-bits)))
  
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

(update-dependent-machine-parameters)

;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; Functions for extracting fields from the instructions
;; and other low-level machine code operations.
;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

(declaim (inline src? dst? op?))

(defun src? (inst)
  "Returns the source address."
  (declare (type (signed-byte 16) inst))
  (declare (type (cons (signed-byte 16)) *srcbits*))
  (the signed-byte (ldb *srcbits* inst)))

(defun dst? (inst)
  "Returns the destination address."
  (declare (type (signed-byte 16) inst))
  (declare (type (cons (signed-byte 16)) *dstbits*))
  (the signed-byte (ldb *dstbits* inst)))

(defun op? (inst)
  "Returns the actual operation, in the case of register ops, in 
the form of a function."
  (declare (type (signed-byte 16) inst))
  (declare (type (cons (signed-byte 16)) *opbits*))
  (declare (type (simple-array function) *operations*))
  (the function (aref *operations* (ldb *opbits* inst))))

(defun opc? (inst)
  "Returns the numerical opcode."
  (declare (type (signed-byte 16) inst))
  (declare (type (cons (signed-byte 16)) *opbits*))
  (the signed-byte (ldb *opbits* inst)))

;; ;; can i pass vecs by ref?
;; (defun flg? (inst)
;;   (declare (type fixnum inst))
;;   (declare (type (cons integer) *flgbits*))
;;   (the signed-byte (ldb *flgbits* inst)))

    

    
 (defun jmp? (inst) ;; ad-hoc-ish...
   (declare (type fixnum inst))
   (the boolean (equalp (op? inst) #'JLE)))

;; ;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

;; (defun enter-input (registers input)
;;   (let ((copy (copy-seq registers)))
;;     (setf (subseq copy *input-start-idx*
;;                   (+ *input-start-idx* (length input)))
;;           input)
;;     copy))


;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; Execution procedure
;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

;; encapsulate in its own let environment?

;; MESSY, and TOO SLOW. Try to delegate debugging output elsewhere. 


;; Thoughts on how to add a stack:
;; - the tricky part is getting the stack instructions (push, pop) and
;; the stack structure to communicate. We don't want the stack to get
;; corrupted by multithreaded activity, so we should enclose it in the
;; execute-sequence environment, you'd think, but then our operations
;; can't be defined where they are now. We'd have to find a way of treating
;; the ops as macros --- macros that may inject a free stack variable
;; into the execute-sequence environment -- but then we hae to change the
;; that that they're currently called.
;; another possibility is to let each "creature" maintain its own stack,
;; as a field in its struct. This is actually pretty close to the idea of
;; each process/thread having a stack of its own. 

;; Almost certainly not threadsafe. 
;; the problem is sidestepped, though, by restricting its use to
;; either debugging mode (which should be single-threaded) or
;; to print jobs that are wrapped in mutexes. 
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

(defun guard-val (val minval maxval)
  "Formats values for registers: guards against overflow, and type casts."
  (declare (type rational val minval maxval))
  (let ((sign (if (< val 0) -1 1)))
    (declare (type rational sign))
    (the rational (coerce (cond ((< (abs val) minval) (* sign minval))
                             ((> (abs val) maxval) (mod val maxval))
                             (t val))
                          'rational))))




(defparameter *operations*
  (vector  #'DIV #'MUL #'SUB #'ADD   ;; basic operations    
           #'PMD ;;                  ;; end of register ops
           ;; --------------------
           #'JLE #'LOD #'STO         ;; PLACEHOLDERS FOR JLE LOD STO
           ;; -------------------- 
           #'XOR #'CNJ #'MOV         ;; extended operations 
           #'MOV #'DIS)              ;; extras, unused by default.  
"Instruction set for the virtual machine. Only the first
     2^*opcode-bits* will be used." )

(defparameter *REGIOPS* 5) ;; all ops w code < 5 are register ops

(defparameter *JUMPOPS* (1+ (position #'JLE *operations*))) ;; all ops >= 5 < 6 are jump ops

(defparameter *LOADOPS* (1+ (position #'LOD *operations*))) ;; and so on

(defparameter *STOROPS* (1+ (position #'STO *operations*))) ;; you understand now. 

;; Some anaphoric macros for quickly dispatching instructions
;; and clarifying code, without costly stack operations
(defmacro exec-reg-inst (inst)
  `(setf (aref %reg (dst? ,inst))
         (guard-val (apply (op? ,inst) 
                           (list (aref %reg (src? ,inst))
                                 (aref %reg (dst? ,inst))))
                    *minval* *maxval*)))

(defmacro exec-jmp-inst (inst)
  `(setf %pc
         (guard-val (apply (op? ,inst)
                           (list (aref %reg (src? ,inst))
                                 (aref %reg (dst? ,inst))
                                 %pc))
                    *minval* *maxval*)))

(defmacro exec-load-inst (inst)
  "Loads contents of memory at address indexed by SRC register into
DST register."
  ;; Note: if we use the flag bits to signal REG, JUMP, LOAD, STOR
  ;; then we still have the opcode bits free for use here. Do we want
  ;; to use flag bits at all? Should we have store and load perform
  ;; operations on their arguments, too -- with MOV as the neutral op?
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
  (declare (type fixnum *input-start-idx* *pc-idx*))
  (declare (inline src? dst? op?))
  (declare (type (cons integer) output))
  (declare (type (signed-byte 16) *regiops* *jumpops*
                 *loadops* *storops*))
  (declare (type (simple-array rational 1) input registers seq))
  (declare (optimize (speed 2)))
  (flet ((save-state (inst reg seq pc)
           (declare (type (simple-array rational 1) reg))
           (declare (type fixnum inst pc))
           (declare (type function =history=))
           (funcall =history= (list :inst inst
                                    :reg reg
                                    :seq seq
                                    :pc pc))))
    (let ((%seq (copy-seq seq))
          (%reg (copy-seq registers))
          (%pc 0) ;; why bother putting the PC in the registers?
          (seqlen (length seq))
          (debugger (or debug *debug*)))
      (declare (type (simple-array rational 1) %reg))
      (declare (type fixnum seqlen %pc))
      (declare (type boolean debugger))
      ;; the input values will be stored in read-only %reg
      (setf (subseq %reg *input-start-idx*
                    (+ *input-start-idx* (length input))) input)
      (unless (zerop seqlen)
        (loop do
           ;; (format t "=== ~A ===~%" %reg)
           ;; Fetch the next instruction
             (let ((inst (aref %seq %pc)))
               (declare (type (signed-byte 16) inst))
               ;; Increment the programme counter
               (incf %pc)
               ;; Perform the operation and store the result in [dst]
               (cond ((< (opc? inst) *REGIOPS*) (exec-reg-inst inst))
                     ((< (opc? inst) *JUMPOPS*) (exec-jmp-inst inst))
                     ((< (opc? inst) *LOADOPS*) (exec-load-inst inst))
                     ((< (opc? inst) *STOROPS*) (exec-store-inst inst))
                     (t (error "UNRECOGNIZED OPCODE IN EXECUTE-SEQUENCE")))
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
