(defpackage #:aoc2020.utils 
  (:use :cl :alexandria)
  (:export #:external-symbols
           #:do-input-lines
           #:with-input))

(in-package aoc2020.utils)

(defun external-symbols (&rest packages &aux symbols)
  (dolist (package packages symbols)
    (do-external-symbols (s (find-package package))
      (push s symbols))))
