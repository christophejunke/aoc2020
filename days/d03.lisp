(defpackage :aoc2020.03
  (:use :aoc2020))

(in-package :aoc2020.03)

(defun input-grid ()
  (with-input (stream 3)
    (read-grid stream
               :element-type 'bit
               :transform (lambda (c) (position c ".#")))))

(defun count-trees (grid dx dy)
  (destructuring-bind (height width) (array-dimensions grid)
    (do ((y 0 (+ y dy))
         (x 0 (mod (+ x dx) width))
         (s 0 (+ s (aref grid y x))))
        ((>= y height) s))))

(defun part-1 (&optional (grid (input-grid)))
  (count-trees grid 3 1))

(defun part-2 (&optional (grid (input-grid)))
  (loop
     :for (dx dy) :in '((1 1) (3 1) (5 1) (7 1) (1 2))
     :for count = (count-trees grid dx dy)
     :for total = count :then (* total count)
     :finally (return total)))

(defun test (&optional (grid (input-grid)))
  (assert (= (part-1 grid) 276))
  (assert (= (part-2 grid) 7812180000)))
