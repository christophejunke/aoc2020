(in-package :aoc2020)

(defun test-all ()
  (with-standard-io-syntax
    (let ((*print-pretty* t)
          (*print-right-margin* most-positive-fixnum)
          (*print-escape* nil))
      (loop
         for day from 1 upto 25
         for package = (find-package (format nil "AOC2020.~2,'0d" day))
         while (do-symbols (s package nil)
                 (when (eq package (symbol-package s))
                   (return t)))
         do (format *trace-output*
                    "~&~2,'0d ~:[ok~;fail ~:*~a~]~%"
                    day
                    (when-let* ((s (find-symbol "TEST" package))
                                (f (symbol-function s)))
                      (handler-case (funcall f) (error (e) e))))))))
