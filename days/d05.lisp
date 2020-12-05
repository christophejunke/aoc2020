(defpackage :aoc2020.05
  (:use :aoc2020))

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

(defun test-front-back-bsp ()
  (assert (= 44 (bsp (as-list "FBFBBFFRLR") 0 127))))

(defun decode-row-column (string)
  (multiple-value-bind (row rest) (bsp (as-list string) 0 127 128)
    (values row (bsp rest 0 7 8))))

(defun row-id (string)
  (multiple-value-bind (row col) (decode-row-column string)
    (values (+ col (* row 8)) row col)))

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

(defun part-1 ()
  (with-input (i 5)
    (loop
      for line = (read-line i nil nil)
      while line
      maximize (row-id line))))

(defun test ()
  (test-front-back-bsp)
  (test-samples))
