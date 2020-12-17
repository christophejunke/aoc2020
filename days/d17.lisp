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

(defun translate (&key tz ty tx)
  (lambda (z y x)
    (list (+ z tz) (+ y ty) (+ x tx))))

(defmacro t-aref (a f &rest coords)
  `(apply #'aref ,a (funcall ,f ,@coords)))

(defun grid-1 (&optional in (sx 3) (sy sx) (sz 11))
  (let ((flat (input in)))
    (destructuring-bind (rows cols) (array-dimensions flat)
      (let ((gx (* sx cols))
            (gy (* sy rows))
            (gz sz))
        (let ((cube (make-array (list gz gy gx))))
          (flet ((offset (small big)
                   (assert (<= small big))
                   (truncate (- big small) 2)))
            (let ((move (translate :tx (offset cols gx)
                                   :ty (offset rows gy)
                                   :tz (offset 0    gz))))
              (do-array (y x) (flat :result (values cube move))
                (setf (t-aref cube move 0 y x) (aref flat y x))))))))))

(defvar *neighbours*
  (remove '(0 0 0)
          (map-product #'list '(-1 0 1) '(-1 0 1) '(-1 0 1))
          :test #'equalp))

(defun count-active-neighbours (g z y x)
  (loop
    for (dz dy dx) in *neighbours*
    for zz = (+ z dz)
    for yy = (+ y dy)
    for xx = (+ x dx)
    when (array-in-bounds-p g zz yy xx)
    sum (aref g zz yy xx)))

(defun kernel-1 (g z y x)
  (let ((an (count-active-neighbours g z y x)))
    (if (= 1 (aref g z y x))
        (if (member an '(2 3))
            1 0)
        (if (= an 3)
            1 0))))

(defun make-part-1-iterator (&rest args)
  (make-iterator #'kernel-1 (apply #'grid-1 args)))

(defun test-sample ()
  (let ((iterator (make-part-1-iterator "17-t1" 6 6 10)))
    (dotimes (x 7)
      (let ((g (funcall iterator)) (c 0))
        (do-array (&rest z) g
          (incf c (apply #'aref g z)))
        (print (map-grid-into nil (lambda (g z y x) (aref #(- @) (aref g z y x))) g))
        (print (cons x c))
        (terpri)))))

(defun part-1 ()
  (let ((iterator (make-part-1-iterator "17" 8 8 15)))
    (dotimes (x 7)
      (let ((g (funcall iterator)) (c 0))
        (do-array (&rest z) g
          (incf c (apply #'aref g z)))
        (print (map-grid-into nil (lambda (g z y x) (aref #(- @) (aref g z y x))) g))
        (print (cons x c))
        (terpri)))))
