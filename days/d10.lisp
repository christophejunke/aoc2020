(defpackage :aoc2020.10
  (:use :aoc2020)
  (:export #:test))

(in-package :aoc2020.10)

(defun input ()
  (sort (z:collect 'list (z:scan-file (fetch-input 10))) #'<))

(defun part-1 (&aux (ones 0) (threes 1))
  (loop
    with ones = 0 and threes = 1
    for last = 0 then jolt
    for jolt in (input)
    for diff = (- jolt last)
    do (case diff
         (1 (incf ones))
         (2)
         (3 (incf threes))
         (t (error "unexpected")))
    finally (return (* ones threes))))

(define-test test ()
  (assert (= 2176 (part-1))))
