(defpackage :aoc2020.08
  (:use :aoc2020)
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

(defun run (code &optional cb &aux (pc 0) (acc 0))
  (loop
    (multiple-value-bind (instruction n) (fetch code pc)
      (when cb (funcall cb pc instruction n))
      (ecase instruction
        (END (return (values acc n)))
        (NOP (incf pc 1))
        (ACC (incf pc 1) (incf acc n))
        (JMP (incf pc n))))))

(defun backmap (program)
  (let ((hash (make-hash-table)))
    (prog1 hash
      (labels ((mark (src dst)
                 (push src (gethash dst hash)))
               (visit (pc op n)
                 (when-let (next (case op ((NOP ACC) (1+ pc)) (JMP (+ pc n))))
                   (push pc (gethash next hash)))))
        (mark :start 0)
        (run program #'visit)))))

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

