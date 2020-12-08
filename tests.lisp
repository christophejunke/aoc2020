(in-package :aoc2020)

(defmacro define-test (name &body body)
  `(progn
     (defun ,name ()
       ,@body)
     (eval-when (:compile-toplevel :execute)
       (setf (get ',name 'test) t)
       (setf (get ',name 'dirty) t))))

(defun test-all (&optional (force nil))
  (with-standard-io-syntax
    (let ((*print-pretty* t)
          (*print-right-margin* most-positive-fixnum)
          (*print-escape* nil)
          (pass t))
      (loop
        for day from 0 upto 25
        for package = (find-package (format nil "AOC2020.~2,'0d" day))
        do 
           (do-external-symbols (s package nil)
             (when-let ((f (and (get s 'test)
                                (fboundp s)
                                (symbol-function s))))
               (when (or force (get s 'dirty))
                 (multiple-value-bind (v e) (ignore-errors (values (funcall f)))
                   (declare (ignore v))
                   (if e (setf pass nil) (setf (get s 'dirty) nil))
                   (format *trace-output*
                           "~&> ~2,'0d~:[~;~:*~{ ~a: ~a~}~]~%"
                           day (if e (list s e))))))))
      pass)))
