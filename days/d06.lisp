(defpackage :aoc2020.06
  (:import-from :aoc2020.04
                aoc2020.04:map-line-chunks)
  (:use :aoc2020)
  (:export #:part-1
           #:test))

(in-package :aoc2020.06)

(defun char-idx (c)
  (- (char-code c) #.(char-code #\a)))

(defun mask (answer-string)
  (loop
    with mask = 0
    for c across answer-string
    do (setf (logbitp (char-idx c) mask) t)
    finally (return mask)))

(defun solve (&aux (c1 0) (c2 0))
  (map-line-chunks 06
                   (lambda (group)
                     (loop
                       for answer-string in group
                       for m = (mask answer-string)
                       for p1 = m then (logior p1 m)
                       for p2 = m then (logand p2 m)
                       finally (incf c1 (logcount p1))
                               (incf c2 (logcount p2)))))
  (values c1 c2))

(defun test ()
  (multiple-value-bind (part-1 part-2) (solve)
    (assert (= part-1 6686))
    (assert (= part-2 3476))))
