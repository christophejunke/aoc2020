(defpackage :aoc2020.05
  (:use :aoc2020)
  (:export #:test
           #:part-1
           #:part-2))

(in-package :aoc2020.05)

(defun seat-id (string)
  (flet ((trans (c) (char "0011" (position c "FLBR"))))
    (parse-integer (map-into string #'trans string) :radix 2)))

(defun part-1 ()
  (with-input (i 5)
    (z:collect-max
     (z:map-fn t #'seat-id (z:scan-stream i #'read-line)))))

(defun part-2 ()
  (let ((seats (make-array (* 128 8) :element-type 'bit)))
    (do-input-lines (line 05 (values (1+ (search #*101 seats)) seats))
      (setf (aref seats (seat-id line)) 1))))

(defun dbg/as-grid (bitvector)
  (make-array (list 128 8) :element-type 'bit
                           :displaced-to bitvector))

(define-test test
  (assert (= (part-1) 980))
  (assert (= (part-2) 607)))
