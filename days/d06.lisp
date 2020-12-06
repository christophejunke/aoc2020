(defpackage :aoc2020.06
  (:import-from :aoc2020.04
                aoc2020.04:map-line-chunks)
  (:use :aoc2020)
  (:export #:part-1
           #:test))

(in-package :aoc2020.06)

(defun part-1 (&aux (counter 0))
  (flet ((char-idx (c) (- (char-code c) #.(char-code #\a))))
    (map-line-chunks 06 (lambda (group)
                          (let ((mask 0))
                            (dolist (answers group (incf counter (logcount mask)))
                              (loop 
                                for c across answers
                                do (setf (logbitp (char-idx c) mask) t))))))
    counter))

(defun test ()
  (assert (= (part-1) 6686)))
