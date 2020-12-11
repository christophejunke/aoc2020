(defpackage :aoc2020.11
  (:use :aoc2020)
  (:export #:test))

(in-package :aoc2020.11)

(defun tile (char)
  (ecase char
    (#\. 'floor)
    (#\L 'empty)
    (#\# 'occupied)))

(defun input ()
  (with-input (in 11)
    (read-grid in :transform #'tile)))

(defun neighbours (g y x)
  (with-buffer (buffer)
    (dolist (dy '(-1 0 1))
      (dolist (dx '(-1 0 1))
        (unless (= dx dy 0)
          (let ((x (+ x dx)) (y (+ y dy)))
            (when (array-in-bounds-p g y x)
              (buffer (aref g y x)))))))))

(defun far-neighbours (g y x)
  (with-buffer (buffer)
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
                  (buffer cell)
                  (return))))))))))

(define-condition change () ())

(defun change (value)
  (prog1 value
    (signal 'change)))

(defmacro on-change-setf (c)
  `(lambda (&rest args) (declare (ignore args))
     (setf ,c t)))

;;;

(defun transform-seat-1 (grid y x)
  (let ((seat (aref grid y x))
        (neighbours (neighbours grid y x)))
    (or (case seat
          (empty (unless (find 'occupied neighbours)
                   (signal 'change)
                   'occupied))
          (occupied (when (>= (count 'occupied neighbours) 4)
                      (signal 'change)
                      'empty)))
        seat)))

(defun transform-seat-2 (grid y x)
  (let ((seat (aref grid y x))
        (neighbours (far-neighbours grid y x)))
    (or (case seat
          (empty (unless (find 'occupied neighbours)
                   (signal 'change)
                   'occupied))
          (occupied (when (>= (count 'occupied neighbours) 5)
                      (signal 'change)
                      'empty)))
        seat)))

(defun map-grid-into (target function source)
  (let ((target (or target (copy-array source))))
    (prog1 target
      (destructuring-bind (rows cols) (array-dimensions source)
        (dotimes (y rows)
          (dotimes (x cols)
            (setf (aref target y x) (funcall function source y x))))))))

(defun fold-grid (grid fold-fn accumulator)
  (destructuring-bind (rows cols) (array-dimensions grid)
    (dotimes (y rows accumulator)
      (dotimes (x cols)
        (setf accumulator (funcall fold-fn accumulator grid y x))))))

(defun update (transform-seat input &optional output)
  (let (changes)
    (handler-bind ((change (on-change-setf changes)))
      (let ((output (map-grid-into output transform-seat input)))
        (and changes output)))))

(defun dbg (grid)
  (prog1 grid
    (fresh-line)
    (write
     (map-grid-into (copy-array grid)
                    (lambda (g y x)
                      (case (aref g y x)
                        (empty '_)
                        (occupied '@)
                        (floor '/)))
                    grid)
     :pretty t
     :right-margin (round (* (second (array-dimensions grid)) 3)))))

(defun update-fixpoint/double-buffering (transform-seat input)
  (let ((output (copy-array input)))
    (loop
      (unless (update transform-seat input output)
        (return input))
      (rotatef input output))))

(defun update-fixpoint/immutable (transform-seat input)
  (loop
    for p = nil then g
    for g = input then (update transform-seat g)
    while g
    finally (return p)))

(defun update-fixpoint (transform-seat input &optional destructive)
  (if destructive
      (update-fixpoint/double-buffering transform-seat input)
      (update-fixpoint/immutable transform-seat input)))

(defun count-occupied (grid)
  (fold-grid grid
             (lambda (a g y x)
               (if (eq (aref g y x) 'occupied)
                   (1+ a)
                   a))
             0))

(defun part-1 (&optional (in (input)) (destructive t))
  (count-occupied (update-fixpoint #'transform-seat-1 in destructive)))

(defun part-2 (&optional (in (input)) (destructive t))
  (count-occupied (update-fixpoint #'transform-seat-2 in destructive)))

(defun sample ()
  (with-input (in "11-sample")
    (read-grid in :transform #'tile)))

(define-test test
  (assert (= 37 (part-1 (sample) nil)))
  (assert (= 26 (part-2 (sample) nil)))
  (assert (= 2316 (part-1)))
  (assert (= 2128 (part-2))))
