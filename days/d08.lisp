(defpackage :aoc2020.08
  (:use :aoc2020)
  (:export #:test))

(in-package :aoc2020.08)

(defun instruction (line)
  (scanner-bind ("%s %i" inst arg) line
    (assert (and inst arg))
    (cons (case (char inst 0)
            (#\n 'nop)
            (#\a 'acc)
            (#\j 'jmp))
          arg)))

(defun program (&optional (in 8))
  (let ((code (make-buffer)))
    (do-input-lines (line in code)
      (buffer-push code (instruction line)))))

(defun run (code &aux (pc 0) (acc 0))
  (flet ((fetch ()
           (if (array-in-bounds-p code pc)
               (shiftf (aref code pc) '(stop))
               (return-from run (values acc t))))
         (jmp (n) (incf pc n)))
    (loop
      (destructuring-bind (op . arg) (fetch)
        (case op
          (stop (return acc))
          (nop (jmp 1))
          (acc (incf acc arg) (jmp 1))
          (jmp (jmp arg)))))))

(defun part-1 (&optional (in 8))
  (run (program in)))

(defun part-2 (&optional (in 8))
  (let ((code (program in)))
    (dotimes (i (length code) :not-found)
      (destructuring-bind (o . a) (aref code i)
        (unless (eq o 'acc)
          (let ((code (copy-seq code)))
            (setf (aref code i) (cons (case o (jmp 'nop) (nop 'jmp)) a))
            (multiple-value-bind (acc ok) (run code)
              (when ok
                (return acc)))))))))

(define-test test
  (assert (= 1930 (part-1)))
  (assert (= 1688 (part-2))))
