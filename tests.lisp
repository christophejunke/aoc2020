(in-package :aoc2020)

(defun aoc-package (day)
  (find-package (format nil "AOC2020.~2,'0d" day)))

(defun aoc-packages ()
  (loop
    for day from 0 upto 25
    collect (aoc-package day)))

(defmacro define-test (name &body body)
  `(progn
     (defun ,name ()
       ,@body)
     (eval-when (:compile-toplevel :execute)
       (setf (get ',name 'test) t)
       (setf (get ',name 'dirty) t))))

(defun test-all-in-packages (packages &optional (force nil))
  (let ((pass t))
    (dolist (package (ensure-list packages) pass)
      (do-external-symbols (s package)
        (when-let ((f (and (get s 'test)
                           (fboundp s)
                           (symbol-function s))))
          (when (or force (get s 'dirty))
            (let ((e (nth-value 1 (ignore-errors (funcall f)))))
              (if e
                  (setf pass nil)
                  (setf (get s 'dirty) nil))
              (with-standard-io-syntax
                (let ((*package* #.(find-package :cl)))
                  (format *trace-output*
                          "~&> ~(~s~)~:[~;~:*~a~]~%"
                          s e))))))))))

(defun test-all (&optional (force nil))
  (test-all-in-packages (aoc-packages) force))
