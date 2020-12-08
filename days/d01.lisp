(defpackage :aoc2020.01
  (:use :aoc2020)
  (:export #:test))

(in-package :aoc2020.01)

(defun part-1 ()
  (let ((entries (make-hash-table)))
    (do-input-lines (line 01)
      (let ((n (parse-integer line)))
        (setf (gethash n entries) t)))
    (block nil
      (maphash-keys (lambda (n)
                      (let ((other (- 2020 n)))
                        (when (gethash other entries)
                          (return (* n other)))))
                    entries)
      (error "Not found"))))

(defun input ()
  (let ((elements (make-buffer '(integer 0) 128)))
    (do-input-lines (line 01 elements)
      (buffer-push elements (parse-integer line)))))

(defun find-sum-3 (&optional (total 2020) (in (input)))
  (loop
     ;; First build a map from a sum S to a list of two indices in the
     ;; input such that elements at those indices add to S, in O(n^2).
     named :main
     with s = (length in)
     and h = (make-hash-table)
     for i below s
     for x = (aref in i)
     do (loop
           for j from (1+ i) below s
           for y = (aref in j)
           for a = (+ x y)
           do (push (list i j) (gethash a h)))
     finally
       ;; Then one more iteration over the input vector to see for
       ;; each Z if (- total z) exists in the map. If so, and if all
       ;; indices are different, then we have our solution.
       (loop
          for k below s
          for z = (aref in k)
          for r = (- total z)
          do (loop
                for (i j) in (gethash r h)
                when (/= i j k)
                do (return-from :main (values in i j k))))))

(defun part-2 ()
  (multiple-value-bind (vec i j k) (find-sum-3 2020 (input))
    (macrolet ((at (i) `(aref vec ,i)))
      (* (at i) (at j) (at k)))))

;; Alternatively, using map-combinations

(defun sum-n (n)
  (map-combinations (lambda (vec)
                      (when (= 2020 (reduce #'+ vec))
                        (return-from sum-n (reduce #'* vec))))
                    (input)
                    :length n))

(defun solve ()
  (values (sum-n 2)
          (sum-n 3)))

(define-test test
  (multiple-value-bind (p1 p2) (solve)
    (assert (= p1 889779))
    (assert (= p2 76110336))))
