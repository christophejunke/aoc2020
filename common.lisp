(defpackage :aoc2020
  (:use . #1=(:cl :alexandria :ppcre :trivia
              :named-readtables :aoc2020.fetch))
  (:export #:fold-input-lines
           #:do-input-lines
           #:with-input
           #:slurp-line
           #:int
           #:word
           #:letter
           .
           #.(aoc2020.utils:external-symbols . #1#)))

(in-package :aoc2020)

(when-let ((package (find-package :series)))
  (rename-package package "Z"))

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

(define-parse-tree-synonym int
    (:register
     (:sequence
      (:greedy-repetition 0 1 (:char-class #\- #\+))
      (:greedy-repetition 1 nil :digit-class))))

(define-parse-tree-synonym letter
    (:register :word-char-class))

(define-parse-tree-synonym word
    (:register
     (:sequence
      :word-boundary
      (:greedy-repetition 1 nil :word-char-class)
      :word-boundary)))
