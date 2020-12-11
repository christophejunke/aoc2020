(defpackage :aoc2020.11
  (:use :aoc2020)
  (:export #:test))

(in-package :aoc2020.11)

;; detect changes

(define-condition change () ())

(defun notify-change (value)
  (prog1 value
    (signal 'change)))

(defmacro on-change-setf (c)
  `(lambda (&rest args) (declare (ignore args))
     (setf ,c t)))

;;; puzzle helpers

(defun map-far-neighbours (f g y x)
  (dolist (dy '(-1 0 1))
    (dolist (dx '(-1 0 1))
      (unless (= dx dy 0)
        (let ((xx x) (yy y))
          (loop
            (incf xx dx)
            (incf yy dy)
            (unless (array-in-bounds-p g yy xx)
              (return))
            (let ((cell (aref g yy xx)))
              (unless (eq 'floor cell)
                (funcall f cell)
                (return)))))))))

(defun at-least-n-occupied-neighbours (n neighbour-mapper grid y x &aux (c 0))
  (block nil
    (funcall neighbour-mapper
             (lambda (v)
               (when (eq v 'occupied)
                 (when (>= (incf c) n)
                   (return t))))
             grid y x)
    nil))

(declaim (inline transform-seat))

(defun transform-seat (grid y x nfn min-empty)
  (let ((seat (aref grid y x)))
    (or (case seat
          (empty
           (unless (at-least-n-occupied-neighbours 1 nfn grid y x)
             (notify-change 'occupied)))
          (occupied
           (when (at-least-n-occupied-neighbours min-empty nfn grid y x)
             (notify-change 'empty))))
        seat)))

(defun transform-seat-1 (grid y x)
  (transform-seat grid y x #'map-neighbours 4))

(defun transform-seat-2 (grid y x)
  (transform-seat grid y x #'map-far-neighbours 5))

(defun occupiedp (grid y x)
  (eq (aref grid y x) 'occupied))

(defun count-occupied (grid)
  (fold-grid grid (lambda (n g y x) (if (occupiedp g y x) (1+ n) n)) 0))

;;; update grid

(declaim (inline update))

(defun update (transform-seat input &optional output)
  (let (changes)
    ;; TRANSFORM-SEAT signals a CHANGE condition when some value changes
    (handler-bind ((change (on-change-setf changes)))
      (let ((output (map-grid-into output transform-seat input)))
        ;; only returns OUTPUT if some seat changed
        (and changes output)))))

;; fixpoint (pure/side-effects)

(defun update-fixpoint/double-buffering (transform-seat input)
  (let ((output (copy-array input)))
    (loop
      (unless (update transform-seat input output)
        (return input))
      (rotatef input output))))

(defun update-fixpoint/fresh-copy (transform-seat input)
  (loop
    :for p = nil :then g
    :for g = input :then (update transform-seat g)
    :while g
    :finally (return p)))

(defun update-fixpoint (transform-seat input &optional destructive)
  (if destructive
      (update-fixpoint/double-buffering transform-seat input)
      (update-fixpoint/fresh-copy transform-seat input)))

(defun part-1 (&optional (in (input)) (destructive t))
  (count-occupied (update-fixpoint #'transform-seat-1 in destructive)))

(defun part-2 (&optional (in (input)) (destructive t))
  (count-occupied (update-fixpoint #'transform-seat-2 in destructive)))

;; parsing

(defun tile (char)
  (ecase char
    (#\. 'floor)
    (#\L 'empty)
    (#\# 'occupied)))

(defun grid (file)
  (with-input (in file)
    (read-grid in :transform #'tile)))

;;  approx. 0.6 seconds of real time
(define-test test
  (assert (= 37 (part-1 (grid #P"11-sample") nil)))
  (assert (= 26 (part-2 (grid #P"11-sample") nil)))
  (assert (= 2316 (part-1 (grid #P"11"))))
  (assert (= 2128 (part-2 (grid #P"11")))))
