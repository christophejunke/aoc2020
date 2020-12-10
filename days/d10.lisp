(defpackage :aoc2020.10
  (:use :aoc2020)
  (:export #:test))

(in-package :aoc2020.10)

(defun input ()
  (sort (z:collect 'list (z:scan-file (fetch-input 10))) #'<))

(defun diffs ()
  (nconc (loop
           for last = 0 then jolt
           for jolt in (input)
           collect (- jolt last))
         (list 3)))

(defun part-1 ()
  (loop
    with c = (vector 0 0 0 0 0)
    for (n . diff) in (run-length-encoding (diffs))
    do (incf (aref c diff) n)
    finally (return (* (aref c 1) (aref c 3)))))

(define-test test ()
  (assert (= 2176 (part-1))))
