(in-package :genlin)

(defparameter *machine* :r-machine)
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
  "Increments the programme counter iff X < Y."
  (declare (type (or null (cons rational)) args))
  (check-type args (cons rational))
  (if (<= (car args) (cadr args))
      (1+ (caddr args))
      (caddr args)))

(defparameter *operations*
  (vector  #'DIV #'MUL #'SUB #'ADD   ;; basic operations    (2bit opcode)
           #'XOR #'PMD #'CNJ #'JLE   ;; extended operations (3bit opcode)
           #'MOV #'DIS)              ;; extras, unused by default.  
"Instruction set for the virtual machine. Only the first
     2^*opcode-bits* will be used." )

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

(defvar *machine-fmt*)

(defun update-dependent-machine-parameters ()

  (setf *wordsize*
    (+ *opcode-bits* *source-register-bits* *destination-register-bits*))

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

(update-dependent-machine-parameters)

;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; Functions for extracting fields from the instructions
;; and other low-level machine code operations.
;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

(declaim (inline src? dst? op?))

(defun src? (inst)
  (declare (type fixnum inst))
  (declare (type (cons integer) *srcbits*))
  (the unsigned-byte (ldb *srcbits* inst)))

(defun dst? (inst)
  (declare (type fixnum inst))
  (declare (type (cons integer) *dstbits*))
  (the unsigned-byte (ldb *dstbits* inst)))

(defun op? (inst)
  (declare (type fixnum inst))
  (declare (type (cons integer) *opbits*))
  (declare (type (simple-array function) *operations*))
  (the function (aref *operations* (ldb *opbits* inst))))

(defun jmp? (inst) ;; ad-hoc-ish...
  (declare (type fixnum inst))
  (the boolean (equalp (op? inst) #'JLE)))

;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

(defun enter-input (registers input)
  (let ((copy (copy-seq registers)))
    (setf (subseq copy *input-start-idx*
                  (+ *input-start-idx* (length input)))
          input)
    copy))


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
            (entry (push entry hist))
            (t hist)))))

(defun history-push-test (seq reg)
  (funcall =history= (cons seq reg)))

(defun history-flush ()
  (funcall =history= 'CLEAR))

(defun history-print (&optional len)
  (let ((h (funcall =history=)))
    (print (subseq h 0 len))))

(defun disassemble-history (&key (len 0) (all t) (static nil))
  "If passed a sequence in the static field, disassemble it without
any reference to register states. If not, disassemble the last len
entries in the history stack, tracking changes to the registers."
  (let* ((history (funcall =history=))
         (len (if all (length history) len))
         (story (if static
                    (coerce static 'list)
                    (reverse (subseq history 0 len)))))
    (loop for couplet in story do
         (let* ((registers (if static
                               (copy-seq *initial-register-state*)
                               (cdr couplet)))
                (inst (if static couplet (car couplet))))
             (format t *machine-fmt* ;; needs to be made flexible. 
                     inst
                     (inst->string inst))
             (if (not static) (format t ";; now R~d = ~f ;; PC = ~d~%"
                                      (dst? inst)
                                      (aref registers (dst? inst))
                                      (aref registers *pc-idx*))
                 (format t "~%")))))
  (if static (hrule)))

(defun guard-val (val minval maxval)
  "Formats values for registers: guards against overflow, and type casts."
  (declare (type rational val minval maxval))
  (let ((sign (if (< val 0) -1 1)))
    (declare (type fixnum sign))
    (the rational (coerce (cond ((< (abs val) minval) (* sign minval))
                             ((> (abs val) maxval) (* sign maxval))
                             (t val)) 'rational))))

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
  (declare (type (simple-array rational 1) input registers seq))
  (declare (optimize (speed 2)))
  (flet ((save-state (inst regs)
           (declare (type (simple-array *) regs))
           (declare (type integer inst))
           (declare (type function =history=))
           (funcall =history= (cons inst regs))))
    (let ((regs (copy-seq registers))
          (seqlen (length seq))
          (debugger (or debug *debug*)))
      (declare (type (simple-array rational 1) regs))
      (declare (type fixnum seqlen))
      (declare (type boolean debugger))
      ;; the input values will be stored in read-only regs
      (setf (subseq regs *input-start-idx*
                    (+ *input-start-idx* (length input))) input)
;;      (PRINT INPUT)
;;      (PRINT REGS)
      (unless (zerop seqlen)
        (loop do
           ;; (format t "=== ~A ===~%" regs)
           ;; Fetch the next instruction
             (let* ((inst (aref seq (aref regs *pc-idx*)))
                    ;; Determine the target register
                    (D (if (jmp? inst) *pc-idx* (dst? inst))))
               ;; Increment the programme counter
               (incf (aref regs *pc-idx*))
               ;; Perform the operation and store the result in [dst]
               (setf (aref regs D)
                     (guard-val (apply (op? inst) 
                                       (list (aref regs (src? inst))
                                             (aref regs (dst? inst))
                                             (aref regs *pc-idx*)))
                     *minval* *maxval*))
               ;; Save history for debugger
               (and debugger (save-state inst regs)
                    (disassemble-history :len 1 :all nil))
               
               (and (>= (aref regs *pc-idx*) seqlen)
                    (return)))))
      (and *debug* (hrule))
      (mapcar #'(lambda (i) (aref regs i)) output))))
