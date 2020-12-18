(defpackage :aoc2020.17
  (:use :aoc2020))

(in-package :aoc2020.17)

(defun flat-grid (&optional name)
  (with-input (stream name)
    (read-grid stream :transform (rcurry #'position ".#"))))

(defun enough-coords (allocated actual)
  (reverse (replace allocated (reverse actual))))

(defun input (dim &optional name)
  (let ((grid (make-infinite-grid))
        (grid-coords (make-list dim :initial-element 0)))
    (do-array (coords value grid) (flat-grid name)
      (let ((c (enough-coords grid-coords coords)))
        (setf (gethash c (aoc2020.grids::.table grid)) (value))))))

(defun active-neighbours (grid coords neighbours-offsets &aux (sum 0))
  (dolist (offsets neighbours-offsets sum)
    (incf sum (iref grid (mapcar #'+ coords offsets) 0))))

(defun neighbourhood (dimension)
  (remove-if (lambda (c) (every #'zerop c))
             (apply #'map-product
                    'list
                    (make-list dimension :initial-element '(-1 0 1)))
             :count 1))

(defvar *neighbours*
  (mapcar (lambda (d) (cons d (neighbourhood d))) '(3 4)))

(defun new-state (grid coords cube neighbours-offsets)
  (let ((an (active-neighbours grid coords neighbours-offsets)))
    (ecase cube
      (0 (if (= an 3)    1 0))
      (1 (if (<= 2 an 3) 1 0)))))

(defun make-transformers (dimension)
  (let ((n (cdr (assoc dimension *neighbours*))))
    (assert n () "Unexpected dimension ~d" dimension)
    (values
     ;; expander
     (lambda (g c v)
       (setf (iref g c) v)
       (loop
         for o in n
         for d = (mapcar #'+ c o)
         do (unless (iref g d)
              (setf (iref g d) 0))))
     ;; transformer
     (lambda (g c v) (new-state g c v n)))))

(defun hdbg (g)
  (print (hash-table-alist (aoc2020.grids::.table g))))

(defun n-iterations (n d name &optional callback)
  (assert (>= n 0))
  (let* ((c 0) (g1 (input d name)) (g2 (copy-infinite-grid g1)))
    (multiple-value-bind (expander transformer) (make-transformers d)
      (loop
        (when callback
          (funcall callback (shiftf c (1+ c)) g1))
        (when (< (decf n) 0)
          (return g1))
        (fold-infinite-grid g1 (lambda (s k v)
                                 (declare (ignore s))
                                 (funcall expander g2 k v)))
        (map-into-infinite-grid g1 transformer g2)))))

(defun active-cubes (grid)
  (fold-infinite-grid grid
                      (lambda (g k v a)
                        (declare (ignore g k))
                        (+ v a))
                      0))

(defun run (dimensions name &optional (n 6))
  (active-cubes (n-iterations n dimensions name)))

(define-test test
  (= (run 3 "17-sample") 112)
  (= (run 4 "17-sample") 848)
  (= (run 3 17) 386)
  (= (run 4 17) 2276))
