(defpackage :aoc2020.12
  (:use :aoc2020)
  (:export #:test))

(in-package :aoc2020.12)

(defstruct (ship (:conc-name)) dir (pos 0) move-waypoint-p)

(defmacro define-action (name (dir &optional
                               (pos (gensym))
                               (move-dir (gensym)))
                     &body body)
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



(define-action move (wpt pos move-waypoint)
  (let ((vec (complex (- :east :west)
                      (- :north :south))))
    (if move-waypoint
        (incf wpt vec)
        (incf pos vec))))

(define-action forward (dir pos)
  (incf pos (* dir :units)))

(define-action turn (dir)
  (setf dir (* dir (expt #C(0 1) (/ (- :left :right) 90)))))

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
