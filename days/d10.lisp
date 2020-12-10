(defpackage :aoc2020.10
  (:use :aoc2020)
  (:export #:test))

(in-package :aoc2020.10)

(defun input ()
  (sort (z:collect 'list (z:scan-file (fetch-input 10))) #'<))

(defun diffs ()
  (with-buffer (buffer)
    (loop
      :for last := 0 :then jolt
      :for jolt :in (input)
      :do (buffer (- jolt last))
      :finally (buffer 3))))

(defun part-1 (&optional (rle (run-length-encoding (diffs))))
  (loop
    with c = (vector 0 0 0 0 0)
    for (n . diff) across rle
    do (incf (aref c diff) n)
    finally (return (* (aref c 1) (aref c 3)))))

;; https://twitter.com/Jas_Hughes/status/1336923190602297344?s=20
(defun part-2 (&optional (rle (run-length-encoding (diffs))))
  (reduce (lambda (product rle)
            (destructuring-bind (n . diff) rle
              (case diff
                (1 (* product (case n
                                (4 7)
                                (3 4)
                                (2 2)
                                (1 1))))
                (2 (error "unexpected"))
                (3 product))))
          rle
          :initial-value 1))

(define-test test ()
  (let ((rle (run-length-encoding (diffs))))
    (assert (= (part-1 rle) 2176))
    (assert (= (part-2 rle) 18512297918464))))
