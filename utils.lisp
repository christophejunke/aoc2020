(defpackage #:aoc2020.utils 
  (:use :cl :alexandria)
  (:export #:external-symbols
           #:defpackage/enum
           #:make-buffer
           #:buffer-push))

(in-package aoc2020.utils)

(defun external-symbols (&rest packages &aux symbols)
  (dolist (package packages symbols)
    (do-external-symbols (s (find-package package))
      (push s symbols))))

(defmacro defpackage/enum (enum-name &body body)
  (assert body)
  (destructuring-bind (doc . body) body
    (check-type doc string)
    (loop
      with p = (gensym (string enum-name))
      for index from 0
      for clause in body
      for (name doc . plist) = (ensure-list clause)
      for symb = (gensym "S")
      collect name into to-export
      collect `(let ((,symb (intern ,(string name) ,p)))
                 (setf (symbol-value ,symb) ,index)
                 (setf (documentation ,symb 'variable) ,doc)
                 (setf (symbol-plist ,symb) (list ,@plist)))
      into actions
      finally (return
                `(progn
                   (defpackage ,enum-name
                     (:documentation ,doc)
                     (:use)
                     (:export ,@to-export))
                   (let ((,p (find-package ',enum-name)))
                     (assert ,p)
                     ,@actions))))))

(defun make-buffer (&optional (element-type t) (size 128))
  (make-array (max 1 size)
              :element-type element-type
              :fill-pointer 0
              :adjustable t))

(defun buffer-push (buffer value)
  (vector-push-extend value buffer (array-total-size buffer)))
