(defpackage :aoc2020.05
  (:use :aoc2020)
  (:export #:test
           #:part-1))

(in-package :aoc2020.05)

(defun bsp (list low high &optional (size (1+ (- high low))))
  (if (= size 1)
      (values low list)
      (let ((size (/ size 2)))
        (destructuring-bind (low-half . list) list
          (if low-half
              (bsp list low (1- (+ low size)) size)
              (bsp list (+ low size) high size))))))

(defun as-list (string)
  (map 'list (rcurry #'find "FL") string))

(defun decode-row-column (string)
  (multiple-value-bind (row rest) (bsp (as-list string) 0 127 128)
    (values row (bsp rest 0 7 8))))

(defun row-id (string)
  (multiple-value-bind (row col) (decode-row-column string)
    (values (+ col (* row 8)) row col)))

(defun part-1 ()
  (with-input (i 5)
    (loop
      for line = (read-line i nil nil)
      while line
      maximize (row-id line))))

(defun part-2 ()
  (let ((occupied (make-array (* 128 8) :element-type 'bit)))
    (do-input-lines (line 5)
      (setf (aref occupied (row-id line)) 1))
    ;; find id of unoccupied
    (values
     ;; find unoccupied from first occupied
     (position 0 occupied :start (or (position 1 occupied) 0)))))

(defun test-front-back-bsp ()
  (assert (= 44 (bsp (as-list "FBFBBFFRLR") 0 127))))

(defun test-samples ()
  (loop 
    for (s exp-row exp-col exp-id)
      in '(("BFFFBBFRRR" 70 7 567)
           ("FFFBBBFRRR" 14 7 119)
           ("BBFFBBFRLL" 102 4 820))
    do (multiple-value-bind (id row col) (row-id s)
         (assert (= row exp-row))
         (assert (= col exp-col))
         (assert (= id exp-id)))))

(defun test ()
  (test-front-back-bsp)
  (test-samples)
  (assert (= (part-1) 980))
  (assert (= (part-2) 607)))

