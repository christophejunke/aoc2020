(in-package :aoc2020)

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
