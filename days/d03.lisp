(defpackage :aoc2020.03
  (:use :aoc2020))

(in-package :aoc2020.03)

(defvar *input-grid*
  (let (rows width (height 0))
    (do-input-lines (line 3 (make-array (list height width)
                                        :element-type 'boolean
                                        :initial-contents (nreverse rows)))
      (unless width (setf width (length line)))
      (incf height)
      (push (map 'list
                 (lambda (c) (ecase c (#\# t) (#\. nil)))
                 line)
            rows))))

(defun count-trees (grid dx dy)
  (destructuring-bind (height width) (array-dimensions *input-grid*)
    (do ((y 0 (+ y dy))
         (x 0 (mod (+ x dx) width))
         (s 0))
        ((>= y height) s)
      (when (aref grid y x)
        (incf s)))))

(defun part-1 ()
  (count-trees *input-grid* 3 1))

(defun test ()
  (assert (= (part-1) 276)))
