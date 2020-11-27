(in-package #:aoc-2020)

(defvar *input-base*
  (merge-pathnames "aoc/2020/inputs/*.txt" (user-homedir-pathname)))

(defun input-file (name)
  (merge-pathnames (make-pathname :name name) *input-base*))

(defmacro with-input ((stream name) &body body)
  `(with-open-file (,stream (input-file ,name))
     ,@body))
