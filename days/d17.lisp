(defpackage :aoc2020.17
  (:use :aoc2020))

(in-package :aoc2020.17)

(defun input (&optional (in 17))
  (with-input (s in)
    (read-grid s :transform (rcurry #'position ".#"))))

(defun make-iterator (fn g1 &optional g2)
  (let ((f #'identity))
    (declare (type function f))
    (labels ((init ()
               (prog1 g1
                 (setf f #'next)))
             (next ()
               (setq g2 (map-grid-into g2 fn g1))
               (prog1 g2
                 (rotatef g1 g2))))
      (setf f #'init)
      (lambda () (funcall f)))))

(defun translate (axes-offsets)
  (lambda (coords)
    (mapcar #'+ axes-offsets coords)))

(defmacro at (g c) `(apply #'aref ,g ,c))

(defun grid-n (&optional in dim (s 3) (n 10))
  (let* ((flat (input in))
         (dims (reverse
                (replace (make-list dim :initial-element 1)
                         (reverse (array-dimensions flat)))))
         ;; scale existing dimensions by S, or give default dimension N
         (cube-dim (mapcar (lambda (d) (if (= d 1) n (* s d))) dims)))
    (let ((cube (make-array cube-dim)))
      (flet ((offset (small big)
               (assert (<= small big))
               (truncate (- big small) 2)))
        (let ((move (translate (mapcar #'offset dims cube-dim))))
          (let* ((coords (make-list dim :initial-element 0))
                 (yx (last coords 2)))
            (do-array (y x) (flat :result (values cube move))
              (setf (first yx) y)
              (setf (second yx) x)
              (let ((coords (funcall move coords)))
                (setf (at cube coords) (aref flat y x))))))))))

(defun grid-1 (in &optional s n)
  (grid-n in 3 s n))

(defvar *neighbours-3d*
  (remove '(0 0 0)
          (map-product #'list '(-1 0 1) '(-1 0 1) '(-1 0 1))
          :test #'equalp))

(defvar *neighbours-4d*
  (remove '(0 0 0 0)
          (map-product #'list '(-1 0 1) '(-1 0 1) '(-1 0 1) '(-1 0 1))
          :test #'equalp))

(defun count-active-neighbours (g n &rest coords)
  (loop
    for offsets in n
    for new-coords = (mapcar #'+ offsets coords)    
    when (apply #'array-in-bounds-p g new-coords)
    sum (at g new-coords)))

(defun kernel-n (offsets)
  (lambda (g &rest coords)
    (let ((an (apply #'count-active-neighbours g offsets coords)))
      (if (= 1 (at g coords))
          (if (member an '(2 3))
              1 0)
          (if (= an 3)
              1 0)))))

(defun make-part-n-iterator (offsets &rest args)
  (make-iterator (kernel-n offsets) (apply #'grid-n args)))

(defun pr (g &rest c)
  (aref #(- @) (at g c)))

(defun test-sample ()
  (let ((iterator (make-part-n-iterator *neighbours-4d* "17-t1" 4 10 15)))
    (dotimes (x 7)
      (let ((g (funcall iterator)) (c 0))
        (do-array (&rest z) g
          (incf c (at g z)))
        ;; (print (map-grid-into nil #'pr g))
        (print (cons x c))
        (terpri)))))

(defun part-1 (&aux (c 0))
  (let ((iterator (make-part-1-iterator "17" 10 50)))
    (dotimes (x 7 c)
      (setf c 0)
      (let ((g (funcall iterator)))
        (do-array (&rest z) g
          (incf c (apply #'aref g z)))
        ;; (print (map-grid-into nil (lambda (g z y x) (aref #(- @) (aref g z y x))) g))
        ;; (print (cons x c))
        ;; (terpri)
        ))))

(defun part-2 (&aux (c 0))
  (let ((iterator (make-part-n-iterator *neighbours-4d* "17" 4 12 30)))
    (dotimes (x 7 c)
      (setf c 0)
      (let ((g (funcall iterator)))
        (do-array (&rest z) g
          (incf c (apply #'aref g z)))
        (print (cons x c))
        ;; (print (map-grid-into nil (lambda (g z y x) (aref #(- @) (aref g z y x))) g))
        ;; (print (cons x c))
        ;; (terpri)
        ))))


(define-test test
  (assert (= (part-1) 386)))
