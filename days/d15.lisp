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

(defmacro last-spoken-value (form)
  `(nth-value 1 ,form))

(defun play-until (list target-turn)
  (multiple-value-bind (env turn) (make-env list)
    (let (past-turn last-spoken)
      (loop
        (let ((value (if past-turn (- turn past-turn) 0)))
          (incf turn)
          (shiftf past-turn (gethash value env) turn)
          (setq last-spoken value))
        (when (>= turn target-turn)
          (return (values env last-spoken)))))))

(defun part-1 ()
  (last-spoken-value (play-until *input* 2020)))

(define-test test
  (assert (equalp (play-until '(0 3 6) 10)
                  '((0 . 10) (4 . 9) (0 . 8) (1 . 7) (3 . 6)
                    (3 . 5) (0 . 4) (6 . 3) (3 . 2) (0 . 1))))
  (assert (= 436 (last-spoken-value (play-until '(0 3 6) 2020))))
  (assert (= 249 (part-1))))

(defun part-2 ()
  (last-spoken-value (play-until *input* 30000000)))

(define-test test-part-2
  (assert (= 41687 (part-2))))

