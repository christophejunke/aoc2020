(in-package :aoc2020)

(defvar *input-base*
  (merge-pathnames "inputs/*.txt" (asdf:system-source-directory "aoc2020")))

(defun input-file (name)
  (typecase name
    (number (input-file (format nil "~2,'0d" name)))
    (string (input-file (make-pathname :name name)))
    (pathname (merge-pathnames name *input-base*))))

(defmacro with-input ((stream name) &body body)
  `(with-open-file (,stream (input-file ,name))
     ,@body))

(defmacro do-input-lines ((line name &optional result) &body body)
  (with-gensyms (stream)
    `(with-input (,stream ,name)
       (loop :for ,line := (read-line ,stream nil nil)
             :while ,line
             :do (progn ,@body)
             :finally (return ,result)))))

(defun fold-input-lines (input function &optional accumulator)
  (do-input-lines (line input accumulator)
    (setf accumulator (funcall function line accumulator))))

(defun slurp-line (input)
  (with-input (s input)
    (read-line s)))

