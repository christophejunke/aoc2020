(defpackage :aoc2020.11
  (:use :aoc2020 #:bricabrac.sdl2.event-loop)
  (:export #:test
           #:demo))

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
  (loop
    :for (dx dy) :in *neighbour-offsets*
    :do (let ((xx x) (yy y))
          (loop
            (incf xx dx)
            (incf yy dy)
            (unless (array-in-bounds-p g yy xx)
              (return))
            (let ((cell (aref g yy xx)))
              (unless (eq 'floor cell)
                (funcall f cell)
                (return)))))))

(declaim (inline %transform-seat))

;; TODO: propagate the CHANGE to neighbours? to detect if a *region* reached a fixpoint?

(defun %transform-seat (g y x neighbour-mapper eo-limit oe-limit)
  ;; neighbour-mapper: visit all the values surrounding a cell in a grid according to some definition.
  ;; eo-limit: empty-to-occupied limit
  ;; oe-limit: occupied-to-empty limit
  (flet ((at-least-n-occupied-neighbours (n)
           (prog ()
              (funcall neighbour-mapper
                       (let ((c 0))
                         (lambda (v)
                           (when (eq v 'occupied)
                             (when (>= (incf c) n)
                               (return t)))))
                       g y x))))
    (let ((seat (aref g y x)))
      (or (case seat
            (empty (unless (at-least-n-occupied-neighbours eo-limit)
                     (notify-change 'occupied)))
            (occupied (when (at-least-n-occupied-neighbours oe-limit)
                        (notify-change 'empty))))
          seat))))

(defun transform-seat-1 (grid y x)
  (%transform-seat grid y x #'map-neighbours 1 4))

(defun transform-seat-2 (grid y x)
  (%transform-seat grid y x #'map-far-neighbours 1 5))

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

(define-condition grid () ((grid :initarg :grid :accessor grid-of)))

(defun update-fixpoint/fresh-copy (transform-seat input)
  (loop
    :for p = nil :then g
    :for g = input :then (update transform-seat g)
    :for o = t then (not o)
    :while g
    :do (when o (signal 'grid :grid g))
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

(defparameter *colors*
  '((floor    . backg)
    (backg    0.1 0.1 0.1 0.1)
    (empty    0.3 0.3 0.3 1.0)
    (occupied 0.5 0.5 0.75 1.0)))

(defun color (c)
  (when-let (v (cdr (assoc c *colors*)))
    (if (symbolp v)
        (color v)
        v)))

(defun show-grid (window rows cols grid)
  (apply #'gl:clear-color (color 'backg))
  (gl:clear :color-buffer)
  (dotimes (y rows)
    (dotimes (x cols)
      (let ((cell (aref grid y x)))
        (apply #'gl:color (color cell))
        (gl:rect x y (1+ x) (1+ y)))))
  (gl:flush)
  (sdl2:gl-swap-window window))

(defun viz (&key (transform 'transform-seat-1)
              (title)
              (sleep/s 1.0)
              (in "11-sample")
              (scale 16)
              (margin 5))
  (let ((grid% (grid in))
        (title (or title
                   (format nil
                           "in=~a scale=~d transform=~a"
                           in
                           scale
                           transform))))
    (destructuring-bind (rows cols) (array-dimensions grid%)
      (flet ((scale (x) (* x scale)))
        (let* ((sleep/ms (round (* sleep/s 1000)))
               (width (scale (+ cols margin margin)))
               (height (scale (+ rows margin margin)))
               (grid grid%))
          (with-captured-bindings (rebind *colors* *neighbour-offsets*)
            (sdl2:with-everything (:window
                                   (w :title title
                                      :w width
                                      :h height)
                                   :gl gl)
              (rebind
               (labels
                   ((show () (show-grid w rows cols grid))
                    (display (c)
                      (setf grid (grid-of c))
                      (show)
                      (do-match-events
                          (:method :wait :timeout sleep/ms)
                        (:idle (return))
                        (:quit (throw :stop nil)))))
                 (sdl2:gl-make-current w gl)
                 (gl:matrix-mode :projection)
                 (gl:load-identity)
                 ;; ???? sure ???
                 (gl:ortho (- margin)
                           (+ cols margin)
                           (+ rows margin)
                           (- margin)
                           0 1)
                 (catch :stop
                   (flet ((pause ()
                            (do-match-events ()
                              (with-key-down-event (_)
                                (return))
                              (:quit (throw :stop nil)))))
                     (loop
                       (show)
                       (pause)
                       (handler-bind ((grid #'display))
                         (update-fixpoint transform grid nil))
                       (pause)
                       (setf grid grid%)))))))))))))

(defun demo ()
  ;;(viz :transform 'transform-seat-1 :sleep/s 0.03 :in 11 :scale 4)
  (viz :transform 'transform-seat-2 :sleep/s 0.03 :in 11 :scale 16))

;;  approx. 0.6 seconds of real time
(define-test test
  (assert (= 37 (part-1 (grid #P"11-sample") nil)))
  (assert (= 26 (part-2 (grid #P"11-sample") nil)))
  (assert (= 2316 (part-1 (grid #P"11"))))
  (assert (= 2128 (part-2 (grid #P"11")))))
