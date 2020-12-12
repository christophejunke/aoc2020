(defpackage :aoc2020.12
  (:use :aoc2020)
  (:export #:test))

(in-package :aoc2020.12)

(defstruct (ship (:conc-name)) dir (pos 0) move-waypoint-p)

(defmacro defaction (name (pos dir move-dir) &body body)
  (labels ((visit (term)
             (typecase term
               (cons (union (visit (car term))
                            (visit (cdr term))))
               (keyword (list term)))))
    (let* ((args (mapcar (lambda (k) (list (list k (gensym)) 0)) (visit body)))
           (body (sublis (loop for ((k s) v) in args
                               collect (cons k s))
                         body)))
      (with-gensyms (ships s)
        `(defmacro ,name (,ships &key ,@args)
           `(dolist (,',s ,,ships)
              (with-accessors ((,',dir dir)
                               (,',pos pos)
                               (,',move-dir move-waypoint-p)) ,',s
                ,@(sublis ,(list* 'list
                                  (loop for ((k s) v) in args
                                        collect `(cons ',s ,s)))
                          ',body))))))))

(defaction move (p d md)
  (let ((v (complex (- :east :west) (- :north :south))))
    (if md (incf d v) (incf p v))))

(defaction forward (p d md)
  (incf p (* d :units)))

(defaction turn (p d md)
  (setf d (* d (expt #C(0 1) (/ (- :left :right) 90)))))

(defun execute-line (line ships)
  (prog1 ships
    (let ((n (parse-integer line :start 1)))
      (case (char line 0)
        (#\N (move    ships :north n))
        (#\S (move    ships :south n))
        (#\E (move    ships :east n))
        (#\W (move    ships :west n))
        (#\L (turn    ships :left n))
        (#\R (turn    ships :right n))
        (#\F (forward ships :units n))))))

(defun manhattan (complex)
  (+ (abs (realpart complex))
     (abs (imagpart complex))))

(defun navigate (input &rest ships)
  (mapcar (compose #'manhattan #'ship-pos)
          (fold-input-lines input #'execute-line ships)))

(define-test test
  (flet ((nav (input)
           (navigate input
                     (make-ship :move-waypoint-p () :dir 1)
                     (make-ship :move-waypoint-p t  :dir #C(10 1)))))
    (assert (equalp '(25 286) (nav "12-sample")))
    (assert (equalp '(415 29401) (nav 12)))))
