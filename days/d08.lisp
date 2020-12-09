(defpackage :aoc2020.08
  (:use :aoc2020)
  (:shadowing-import-from
   :sb-assem . #.(aoc2020:external-symbols :sb-assem))
  (:export #:test))

(in-package :aoc2020.08)

(defun program (&optional (in 8))
  (let ((code (make-buffer)))
    (do-input-lines (line in (coerce code 'simple-vector))
      (scanner-bind ("%s %i" instruction operand) line
        (assert instruction)
        (buffer-push code (cons (case (char instruction 0)
                                  (#\n 'NOP)
                                  (#\a 'ACC)
                                  (#\j 'JMP))
                                operand))))))

(defun fetch (code pc)
  (if (array-in-bounds-p code pc)
      ;; replace by end instruction to detect loops
      (destructuring-bind (inst . n) (shiftf (aref code pc) '(END))
        (values inst n))
      ;; normal end of program
      (values 'END :ok)))

(import 'sb-vm::rax-tn)

(let ((code #((nop . 3) (acc . 2) (jmp . -2))))
  (let* ((asmstream (make-asmstream))
         (*asmstream* asmstream)
         (labels (map 'vector 
                      (lambda (i) (gen-label (princ-to-string i)))
                      code)))
    ;; actual assembly
    (assemble (:code 'nil)
      (inst xor :dword rax-tn rax-tn)
      (loop
        for i below (length code)
        for (op . n) across code
        for lab = (aref labels i)
        do (emit-label lab)
           (inst mov
                 :dword
                 (sb-x86-64-asm::rip-relative-ea lab) #xc3)
           (case op
             (nop )
             (acc (inst add :dword rax-tn n))
             (jmp (inst jmp (aref labels (+ i n)))))))

    ;; debugging
    (let ((segment (assemble-sections asmstream nil (make-segment))))
      (let ((vec (segment-contents-as-vector segment)))
        (sb-sys:with-pinned-objects (vec) 
          (sb-disassem:disassemble-memory (sb-sys:vector-sap vec)
                                          (length vec)))))))


(sb-sys:with-pinned-objects (vec) 
  (sb-disassem:disassemble-memory (sb-sys:vector-sap vec) (length vec)))


(defun assemble (code &aux (pc 0) (acc 0))

  (loop
    (multiple-value-bind (instruction n) (fetch code pc)
      (ecase instruction
        (END (return (values acc n)))
        (NOP (incf pc 1))
        (ACC (incf pc 1) (incf acc n))
        (JMP (incf pc n))))))

(defun run (code &aux (pc 0) (acc 0))
  (loop
    (multiple-value-bind (instruction n) (fetch code pc)
      (ecase instruction
        (END (return (values acc n)))
        (NOP (incf pc 1))
        (ACC (incf pc 1) (incf acc n))
        (JMP (incf pc n))))))

(defun part-1 (&optional (in 8))
  (run (program in)))

(defun part-2 (&optional (in 8))
  (flet ((swap (op) (ecase op (JMP 'NOP) (NOP 'JMP))))
    (let ((code (program in)))
      (dotimes (i (length code) :not-found)
        (destructuring-bind (inst . n) (aref code i)
          (unless (eq inst 'ACC)
            (let ((code (copy-seq code)))
              (setf (aref code i) (cons (swap inst) n))
              (multiple-value-bind (acc ok) (run code)
                (when ok
                  (return acc))))))))))

(define-test test
  (assert (= 1930 (part-1)))
  (assert (= 1688 (part-2))))
