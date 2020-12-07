(defpackage :aoc2020.06
  (:use :aoc2020)
  (:export #:solve
           #:test))

(in-package :aoc2020.06)

(defun char-idx (c)
  (- (char-code c) #.(char-code #\a)))

(defun mask (answer-string)
  (loop
    with mask = (make-array 26 :element-type 'bit)
    for c across answer-string
    do (setf (bit mask (char-idx c)) 1)
    finally (return mask)))

(defun solve (&aux (c1 0) (c2 0))
  (map-line-chunks 06
                   (lambda (group)
                     (loop
                       for answer-string in group
                       for m1 = (mask answer-string)
                       for m2 = (mask answer-string)
                       for p1 = m1 then (bit-ior m1 p1 p1)
                       for p2 = m2 then (bit-and m2 p2 p2)
                       finally (incf c1 (count 1 p1))
                               (incf c2 (count 1 p2)))))
  (values c1 c2))

(define-test test
  (multiple-value-bind (part-1 part-2) (solve)
    (assert (= part-1 6686))
    (assert (= part-2 3476))))

