(defpackage :aoc2020.05
  (:use :aoc2020)
  (:export #:test
           #:part-1
           #:part-2
           #:solve))

(in-package :aoc2020.05)

(defun seat-id (string)
  (flet ((trans (c) (aref "0011" (position c "FLBR"))))
    (parse-integer (map-into string #'trans string) :radix 2)))

(defun part-1 ()
  (with-input (i 5)
    (loop
      for line = (read-line i nil nil)
      while line
      maximize (seat-id line))))

(defun part-2 ()
  (let ((occupied (make-array (* 128 8) :element-type 'bit)))
    (do-input-lines (line 5)
      (setf (aref occupied (seat-id line)) 1))
    ;; find id of unoccupied
    (values
     ;; find unoccupied from first occupied
     (position 0 occupied :start (or (position 1 occupied) 0))
     (make-array (list 128 8) :element-type 'bit
                 :displaced-to occupied))))

(defun test-front-back-bsp ()
  (assert (= 44 (bsp (as-list "FBFBBFFRLR") 0 127))))

(defun test-samples ()
  (loop 
    for (s exp-id) in '(("BFFFBBFRRR" 567)
                        ("FFFBBBFRRR" 119)
                        ("BBFFBBFRLL" 820))
    do (assert (= (row-id s) exp-id))))

(defun test ()
  (test-front-back-bsp)
  (test-samples)
  (assert (= (part-1) 980))
  (assert (= (part-2) 607)))

