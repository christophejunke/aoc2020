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
  (let ((code (make-array 128 :adjustable t :fill-pointer 0)))
    (do-input-lines (line in code)
      (vector-push-extend (instruction line)
                          code
                          (array-total-size code)))))

(defun part-1 (&optional (in 8))
  (let ((code (program in)) (pc 0) (acc 0))
    (flet ((fetch () (shiftf (aref code pc) '(stop)))
           (jmp (n) (incf pc n)))
      (loop
        (destructuring-bind (op . arg) (fetch)
          (case op
            (stop (return acc))
            (nop (jmp 1))
            (acc (incf acc arg) (jmp 1))
            (jmp (jmp arg))))))))

(define-test test
  (assert (= 1930 (part-1))))

