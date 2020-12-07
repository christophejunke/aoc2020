(defpackage :aoc2020.06
  (:import-from :aoc2020.04
                aoc2020.04:map-line-chunks)
  (:use :aoc2020)
  (:export #:solve
           #:test))

(in-package :aoc2020.06)

(defun char-idx (c)
  (- (char-code c) #.(char-code #\a)))

(defun letters (answer-string)
  (coerce (delete-duplicates answer-string) 'list))

(defun solve (&aux (c1 0) (c2 0))
  (map-line-chunks 06
                   (lambda (group)
                     (loop
                       for answer-string in group
                       for letters = (letters answer-string)
                       for p1 = letters then (union p1 letters)
                       for p2 = letters then (intersection p2 letters)
                       finally (incf c1 (length p1))
                               (incf c2 (length p2)))))
  (values c1 c2))

(defun test ()
  (multiple-value-bind (part-1 part-2) (solve)
    (assert (= part-1 6686))
    (assert (= part-2 3476))))
