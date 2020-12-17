(in-package :aoc2020)

(defun read-grid (stream &key (element-type t) (transform #'identity))
  (loop
    :with grid = (make-array 256
                             :adjustable t
                             :element-type element-type
                             :fill-pointer 0)
    :for line = (read-line stream nil nil)
    :while line
    :for same-width = t :then (= width (length line))
    :for width = (length line)
    :count line :into height
    :do (assert same-width
                ()
                "rows with different width as expected (~d): ~a"
                width
                line)
        (map ()
             (lambda (c)
               (vector-push-extend (funcall transform c)
                                   grid
                                   (array-total-size grid)))
             line)
    :finally (return (make-array (list height width)
                                 :element-type element-type
                                 :displaced-to grid))))
(defun ignorable-symbol-p (s)
  (or (null s)
      (let ((s (string s)))
        (or (= 0 (length s))
            (char= #\_ (char s 0))))))

(defun lambda-list-array-dimension (args)
  (multiple-value-bind (req opt rest key aokp aux kp)
      (alexandria:parse-ordinary-lambda-list args)
    (declare (ignore aux))
    (and (not opt)
         (not rest)
         (not key)
         (not aokp)
         (not kp)
         (length req))))

(defun ignorable-variables (vars)
  (loop
    for v in vars
    for i = (ignorable-symbol-p v)
    for w = (if i (gensym "_") v)
    collect w into vars
    when i
    collect w into ignored
    finally (return (values vars ignored))))

(defmacro do-array% (d g a body as result)
  (let* ((bindings (loop
                     for res = (list result) then nil
                     for name = nil then (gensym "LOOP")
                     repeat d
                     collect (list name
                                   (gensym "C-")
                                   (gensym "D-")
                                   res)))
         (cvars (mapcar #'second bindings))
         (dvars (mapcar #'third bindings)))
    (multiple-value-bind (a ignore) (ignorable-variables a)
      (once-only (g)
        (with-gensyms (coords rest)
          (let ((main
                  `(destructuring-bind (,@dvars &rest ,rest)
                       (array-dimensions ,g)
                     (declare (ignore ,rest))
                     ,@(reduce (lambda (nvdr inner)
                                 (destructuring-bind (n v d r) nvdr
                                   `((loop
                                       :named ,n :for ,v fixnum
                                       :below ,d :do ,@inner
                                       ,@(and r `(:finally (return ,@r)))))))
                               bindings
                               :from-end t
                               :initial-value
                               `((let ((,coords (list ,@cvars)))
                                   (declare (dynamic-extent ,coords))
                                   (destructuring-bind ,a ,coords
                                     (declare (ignore ,@ignore))
                                     ,@body)))))))
            (if as
                `(let ((,as ,g)) ,main)
                main)))))))

(defmacro do-array ((&rest args) grid &body body)
  (destructuring-bind (grid &key result dimension as) (ensure-list grid)
    (let ((dimension (or dimension (lambda-list-array-dimension args))))
      (if (numberp dimension)
          `(do-array% ,dimension ,grid ,args ,body ,as ,result)
          (once-only (grid)
            (with-gensyms (coords cursor rec-fn dims axis c)
              `(let* ((,dims (array-dimensions ,grid))
                      (,coords (copy-list ,dims))
                      ,@(and as `((,as ,grid))))
                 ,@(when dimension
                     `((assert (= (length ,dims) ,dimension))))
                 (block nil
                   (labels ((,rec-fn (,dims ,cursor)
                              (cond
                                (,dims (destructuring-bind (,axis . ,dims) ,dims
                                         (dotimes (,c ,axis)
                                           (setf (car ,cursor) ,c)
                                           (,rec-fn ,dims (cdr ,cursor)))))
                                (t (destructuring-bind (,@args) ,coords
                                     ,@body)))))
                     (,rec-fn ,dims ,coords))
                   ,result))))))))

(defun map-grid-into (target function source)
  (let ((target (or target (copy-array source))))
    (do-array (&rest coords) (source :result target)
      (setf (apply #'aref target coords)
            (apply function source coords)))))

(defun fold-grid (grid fold-fn accumulator)
  (do-array (&rest coords) (grid :result accumulator)
    (setf accumulator (apply fold-fn accumulator grid coords))))

(defvar *neighbour-offsets*
  '((-1 -1)(+0 -1)(+1 -1)
    (-1 +0)       (+1 +0)
    (-1 +1)(+0 +1)(+1 +1)))

(defun map-neighbours (f g y x &key (offsets *neighbour-offsets*))
  (loop :for (dx dy) :in offsets
        :do (let ((x (+ x dx)) (y (+ y dy)))
             (when (array-in-bounds-p g y x)
               (funcall f (aref g y x))))))

(defun map-neighbours* (f g coords &key (offsets *neighbour-offsets*))
  (loop :for offset :in offsets
        :do (let ((coords (mapcar #'+ coords (reverse offset))))
             (when (apply #'array-in-bounds-p g coords)
               (funcall f (apply #'aref g coords))))))
