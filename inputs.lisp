(in-package #:aoc-2020)

(defvar *input-base*
  (merge-pathnames "aoc/2020/inputs/*.txt" (user-homedir-pathname)))

(defun input-file (name)
  (typecase name
    (number (input-file (format nil "~2,'0d" name)))
    (string (input-file (make-pathname :name name)))
    (pathname (merge-pathnames name *input-base*))))

(defmacro with-input ((stream name) &body body)
  `(with-open-file (,stream (input-file ,name))
     ,@body))
