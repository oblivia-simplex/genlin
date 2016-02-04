(in-package :genlin)

;;; This copy only exists for the sake of carefree tinkering. Eventually,
;;; it will be merged with r-machine.lisp, the difference between the two
;;; coming down to a choice of parameters.

(defparameter *machine* :stackmachine)


;;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;;; STACKMACHINE differs from SLOMACHINE primarily in programme design.
;;; I've switched back from arrays to lists as the canonical code-sequence
;;; representation. I've also redesigned the way in which machine code
;;; instructions are formalized. Each instruction is now, again, a function,
;;; but a function that only takes one argument: the instruction itself,
;;; from which it extracts the necessary information using dst? src? and
;;; so on. This lets me be much more versatile with the way in which
;;; instructions operate, without having to reduce everything to the same
;;; procrustean signature, or wrap every call in an ugly, ad hoc case or
;;; cond table. Use of external lexical variables is made possible by
;;; incorporating a few free variables into these functions -- marked with
;;; the sigil '%', for easy reading. These include, at present, %seq, %reg
;;; and %stk. In order to run any of these functions, they need to be
;;; enclosed in a lexical environment in which these variables are bound,
;;; and bound to values of the expected types (%reg is an array, while
;;; %seq and %stk are lists). (Note: there's also %skip, which is a boolean.
;;; It's set to T when a jump instruction prompts a jump, and nil otherwise.)
;;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; Declaim inline functions for speed
;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

(declaim (inline guard-val))

(declaim (inline LOD STO MOV DIV MUL XOR CNJ DIS PMD ADD SUB MUL JLE)) 

(declaim (inline exec))
         
;;(declaim (inline execute-sequence)) ;; ballsy. can't believe it worked.

(declaim (inline dst? src? op? jmp?))

(declaim (inline src? dst? op?))

;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

(defun src? (inst)
  "Returns the source address."
  (declare (type (unsigned-byte 64) inst))
  (declare (type (cons (unsigned-byte 64)) *srcbits*))
  (declare (optimize (speed 3)))
  (the signed-byte (ldb *srcbits* inst)))

(defun dst? (inst)
  "Returns the destination address."
  (declare (type (unsigned-byte 64) inst))
  (declare (type (cons (unsigned-byte 64)) *dstbits*))
  (declare (optimize (speed 3)))
  (the signed-byte (ldb *dstbits* inst)))

(defun op? (inst)
  "Returns the actual operation, in the case of register ops, in 
the form of a function."
  (declare (type (unsigned-byte 64) inst))
  (declare (type (cons (unsigned-byte 64)) *opbits*))
  (declare (type (simple-array function) *operations*))
  (declare (optimize (speed 3)))
  (the function (aref *operations* (ldb *opbits* inst ))))

(defun opc? (inst)
  "Returns the numerical opcode."
  (declare (type (unsigned-byte 64) inst))
  (declare (type (cons (unsigned-byte 64)) *opbits*))
  (the (unsigned-byte 64) (ldb *opbits* inst)))

(defun jmp? (inst) ;; ad-hoc-ish...
  (declare (type (unsigned-byte 64) inst))
  (the boolean (= (opc? inst) 5))) ;; TEMPORARY STOPGAP



;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; master update function for VM parameters
;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

;; --- Operations: these can be tweaked independently of the 
;; --- fields above, so long as their are at least (expt 2 *opf*)
;; --- elements in the *operations* vector. 

;; NEW STRATEGY: USE FREE VARIABLES. USE THEM LIKE YOU STOLE THEM

(defvar %seq)

(defvar %reg)

(defvar %stk)

(defvar %pc)

(defvar %skip)

(defmacro grd* (expr)
  `(guard-val ,expr *minval* *maxval*))

(defun exec (inst)
  (declare (type (unsigned-byte 64) inst))
  (declare (optimize (speed 3)))
  (funcall (op? inst) inst))

(defun test-exec (inst)
  (let ((%reg *initial-register-state*)
        (%seq `(,inst))
        (%stk '()))
    (format t "SRC: R~D = ~F~%DST: R~D = ~F~%OP:  ~A~%"
            (src? inst) (elt %reg (src? inst))
            (dst? inst) (elt %reg (dst? inst))
            (func->string (op? inst)))
    (exec inst)))

(defun MOV (inst)
  "Copy the contents in SRC to register DST."
  (declare (type (unsigned-byte 64) inst))
  (setf (elt %reg (dst? inst)) (elt %reg (src? inst))))


(defun DIV (inst)
  (declare (type (unsigned-byte 64) inst))
  (setf (elt %reg (dst? inst))
        (grd* (divide (elt %reg (dst? inst))
                      (elt %reg (src? inst))))))

(defun XOR (inst) ;; xor integer parts
  "Performs a bitwise XOR on SRC and DST, storing the result in DST."
  (declare (type (unsigned-byte 64) inst))
  (setf (elt %reg (dst? inst))
        (logxor (floor (elt %reg (dst? inst)))
                (floor (elt %reg (src? inst))))))

(defun CNJ (inst)
  "Performs a bitwise AND on SRC and DST, storing the result in DST."
  (declare (type (unsigned-byte 64) inst))
  (setf (elt %reg (dst? inst))
        (logand (floor (elt %reg (dst? inst)))
                (floor (elt %reg (src? inst))))))

(defun DIS (inst)
  "Performs a bitwise OR on SRC and DST, storing the result in DST."
  (declare (type (unsigned-byte 64) inst))
  (setf (elt %reg (dst? inst))
        (logorc1 (floor (elt %reg (dst? inst)))
                 (floor (elt %reg (src? inst))))))

(defun PMD (inst)
  "Performs a protected SRC MOD DST, storing the result in DST."
  (declare (type (unsigned-byte 64) inst))
  (setf (elt %reg (dst? inst))
        (if (zerop (elt %reg (dst? inst)))
            0
            (mod (elt %reg (src? inst))
                 (elt %reg (dst? inst))))))

(defun ADD (inst)
  "Adds the rational-valued contents of SRC and DST, storing result in DST."
  (declare (type (unsigned-byte 64) inst))
  (setf (elt %reg (dst? inst))
        (+ (elt %reg (src? inst))
           (elt %reg (dst? inst)))))

(defun SUB (inst)
  "Subtracts DST from SRC, storing the value in DST."
  (declare (type (unsigned-byte 64) inst))
  (setf (elt %reg (dst? inst))
        (grd* (- (elt %reg (src? inst))
                 (elt %reg (dst? inst))))))

(defun MUL (inst)
  "Multiplies SRC and DST, storing the value in DST."
  (declare (type (unsigned-byte 64) inst))
  (setf (elt %reg (dst? inst))
        (grd* (* (elt %reg (src? inst))
                 (elt %reg (dst? inst))))))

(defun JLE (inst)
  "Stub. Really just here for consistent pretty printing."
  (declare (type (unsigned-byte 64) inst))
  (if (<= (elt %reg (src? inst)) (elt %reg (dst? inst)))
      (setf %skip T)))

;; changing lod/sto so that they operate on the stack instead
;; of on the code segment, so that we can recover intron detection
(defun LOD (inst)
  "Stub. Really just here for consistent pretty printing."
  (declare (type (unsigned-byte 64) inst))
  (setf (elt %reg (dst? inst))
        (if (null %stk)
            0
            (elt %stk (safemod (floor (elt %reg (src? inst)))
                               (length %stk))))))

(defun STO (inst)
  "Stub. Really just here for consistent pretty printing."
  (declare (type (unsigned-byte 64) inst))
  (let ((stuff (ldb (byte *wordsize* 0) (floor (elt %reg (src? inst))))))
    (if (null %stk)
        (push stuff %stk)
        (setf (elt %stk (safemod (floor (elt %reg (dst? inst)))
                                 (length %stk)))
              (ldb (byte *wordsize* 0) (floor (elt %reg (src? inst))))))))

(defun PRG (inst) ;; pop to register
  (declare (type (unsigned-byte 64) inst))
  (setf (elt %reg (dst? inst))
        (nil0 (pop %stk))))

(defun PSH (inst)
  (declare (type (unsigned-byte 64) inst))
  (push (elt %reg (src? inst)) %stk))

;; but PEX still introduces arbitrary code execution, which
;; prevents any sort of definitive intron detection -- but then,
;; does it matter? Is anything lost if introns are only approximately
;; introns, potentially activated by epigenetic factors?
;; Biology got away with it. 
(defun PEX (inst) ;; pop to execute
  (declare (ignore inst))
  (exec (floor (abs (nil0 (pop %stk))))))

(defun PIN (inst)
  (declare (type (unsigned-byte 64) inst))
  (push (elt %seq
             (safemod (floor (elt %reg
                                  (src? inst))) (length %seq)))
        %stk))

;; just go ahead and use free variables 
;; (defun PPR (&rest args)
;;   "

(defparameter *reg-ops-list*
  `(,#'ADD ,#'MUL ,#'SUB ,#'DIV
         ,#'PMD ,#'XOR ,#'CNJ ,#'DIS))

(defparameter *jump-ops-list*
  `(,#'JLE))

(defparameter *load-ops-list*
  `(,#'LOD))

(defparameter *store-ops-list*
  `(,#'STO))

(defparameter *stack-ops-list*
  `(,#'PSH ,#'PEX ,#'PRG ,#'PIN))

;; suggestion: prepare some combos that can be selected from at runtime.
;; (using command line options, e.g.)
(defparameter *slomachine-ops-list*
  (concatenate 'vector
               (subseq *reg-ops-list* 0 5)
               *jump-ops-list*
               *load-ops-list*
               *store-ops-list*))

(defparameter *operations*
  (remove-if #'null (concatenate 'vector
                                 *reg-ops-list*
                                 *load-ops-list*
                                 *store-ops-list*
                                 *stack-ops-list*
                                 *jump-ops-list*
                                 (loop repeat (expt 2 *opcode-bits*)
                                      collect #'MOV))))

                                 ;;*jump-ops-list*
                                 ;;*load-ops-list*
                                 ;;*store-ops-list*)))


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

;; Not quite ready yet. 
      
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

;; history is not threadsafe. does it matter? not much, since it's
;; only used in debugging mode, when *parallel* is set to nil. 
(defparameter =history=
  (let ((hist '())
        (warning t))          
    (lambda (&optional (entry nil))
      (when (and *parallel* warning)
        (format t "***  WARNING: HISTORY IS NOT THREADSAFE  ***~%")
        (format t "*** SET *PARALLEL* TO NIL WHEN DEBUGGING ***~%")
        (setf warning nil))
      (cond ((eq entry 'FLUSH) (setf hist '()))
            ((eq entry 'ALL) hist)
            (entry (push entry hist))
            (t (car hist))))))

(defun history-push-test (seq reg)
  (funcall =history= (cons seq reg)))

(defun history-flush ()
  (funcall =history= 'FLUSH))

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
           (when (not static) (format t "  (~d) " (getf line :pc))
                 (cond ((member (op? inst) *reg-ops-list*)
                        (format t " ;; now R~d = ~f; R~d = ~f~%" 
                                (src? inst)
                                (elt registers (src? inst))
                                (dst? inst)
                                (elt registers (dst? inst))))
                       ((member (op? inst) *jump-ops-list*)
                        (if (<= (elt registers (src? inst))
                                (elt registers (dst? inst)))
                            (format t " ;; R~d <= R~d, so skipping next~%"
                                    (src? inst) (dst? inst))
                            (format t " ;; R~d > R~d, no jump~%"
                                    (src? inst) (dst? inst))))
                       ((member (op? inst) *load-ops-list*)
                        (setf addr (mod (floor
                                         (elt registers (src? inst)))
                                        (length (getf line :seq)))
                              data (elt (getf line :seq) addr)
                              regnum (dst? inst))
                        (format t " ;; loading ~d from @~d into R~d~%"
                                data addr regnum))
                       ((member (op? inst) *store-ops-list*)
                        (setf addr
                              (mod (floor (elt registers (dst? inst)))
                                        (length (getf line :seq)))
                              data (ldb (byte *wordsize* 0)
                                        (floor (elt registers
                                                     (src? inst))))
                              regnum (src? inst))
                        (format t " ;; storing ~d from R~d in @~d~%"
                                data regnum addr)
                        (when (>= addr (getf line :pc))
                          (format t "*** SEQUENCE REWRITING ITSELF ***~%")))
                       ((member (op? inst) *stack-ops-list*)
                        (format t " ;; STK: ~A~%" (getf line :stk)))
                       (t (format t "  WTF?"))))
           (if static
               (format t "~%"))))
  (if static (hrule))))

;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; The engine room
;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=



(defun execute-sequence (seq &key (registers *initial-register-state* )
                               (input *default-input-reg*)
                               (stack nil)
                               (output nil)
                               (debug nil))
  "Takes a sequence of instructions, seq, and an initial register
state vector, registers, and then runs the virtual machine, returning
the resulting value in the registers indexed by the integers in the
list parameter, output."
  (declare (type fixnum *input-start-idx* *pc-idx*)
           (type (cons integer) output)
           (type (or null (cons (unsigned-byte 64))) seq)
           (type (or null cons) stack)
           (type (simple-array rational 1) input registers)
           (optimize (speed 3)))
  (flet ((save-state (inst &key reg seq stk skip pc)
           (declare (type (simple-array rational 1) reg)
                    (type boolean skip)
                    (type function =history=))
           (funcall =history= (list :inst inst
                                    :reg reg
                                    :seq seq
                                    :skip skip
                                    :stk stk
                                    :pc pc))))
    (let ((%seq (copy-seq seq)) ;; tmp: eventually switch to lists 
          (%reg (copy-seq registers))
          (%pc 0) ;; why bother putting the PC in the registers?
          (%skip nil)
          (%stk (copy-seq stack)) ;; let's add a stack! and get rid of LOD/STO
;;;          (seqlen (length seq))
          (debugger (or debug *debug*)))
      ;; so that we can have our junk DNA back.
      (declare (type (simple-array rational 1) %reg)
               (type boolean debugger)
               (type integer %pc))
      ;; the input values will be stored in read-only %reg
      (setf (subseq %reg *input-start-idx*
                    (+ *input-start-idx* (length input))) input)
      (loop for inst in %seq do
           (incf %pc) ;; not truly necessary. may suppress for speed. 
           (cond (%skip (setf %skip nil))
                 (T (exec inst) ;; this expression does all the work
                    (and debugger (save-state inst
                                              :reg %reg
                                              :seq %seq
                                              :skip %skip
                                              :stk %stk
                                              :pc %pc)
                         (disassemble-history)))))
      (and *debug* (hrule) (history-flush))
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























































































