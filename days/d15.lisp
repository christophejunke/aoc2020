(defpackage :aoc2020.15
  (:use :aoc2020)
  (:export #:test))

(in-package :aoc2020.15)

(defvar *input* '(15 12 0 14 3 1))

(defun make-env% (list &optional (turn 1) result)
  (if list
      (make-env% (rest list)
                 (1+ turn)
                 (acons (car list) turn result))
      result))

(defun make-env (list)
  (let ((env (make-env% list)))
    (values (alist-hash-table env) (cdar env))))

(defun play-until (list target-turn)
  (multiple-value-bind (env turn) (make-env list)
    (let (past-turn last-spoken)
      (loop
        (let ((value (if past-turn (- turn past-turn) 0)))
          (incf turn)
          (shiftf past-turn (gethash value env) turn)
          (setq last-spoken value))
        (when (>= turn target-turn)
          (return last-spoken))))))

(defun part-1 ()
  (play-until *input* 2020))

(define-test test
  (assert (= 436 (play-until '(0 3 6) 2020)))
  (assert (= 249 (part-1))))

(defun part-2 ()
  (play-until *input* 30000000))

(define-test test-part-2
  (assert (= 41687 (part-2))))

