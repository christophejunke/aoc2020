(defpackage :aoc2020.15
  (:use :aoc2020)
  (:export #:test))

(in-package :aoc2020.15)

(defvar *input* '(15 12 0 14 3 1))

(defun make-env (list &optional (turn 1) result)
  (if list
      (make-env (rest list)
                (1+ turn)
                (acons (car list) turn result))
      result))

(defun value-turn (env value)
  (cdr (assoc value env)))

(defun last-spoken-value (env)
  (destructuring-bind ((value . turn) . _) env
    (declare (ignore _))
    (values value turn)))

(defun speak (env turn value)
  (values (acons value turn env)
          turn
          (value-turn env value)))

(defun play (env past-turn)
  (multiple-value-bind (last-spoken turn) (last-spoken-value env)
    (declare (ignore last-spoken))
    (if past-turn
        (speak env (1+ turn) (- turn past-turn))
        (speak env (1+ turn) 0))))

(defun play-until (list target-turn)
  (let ((env (make-env list)) (turn 0) (past-turn))
    (loop
      (multiple-value-setq (env turn past-turn) (play env past-turn))
      (when (>= turn target-turn)
        (return env)))))

(defun part-1 ()
  (last-spoken-value (play-until *input* 2020)))

(define-test test
  (assert (equalp (play-until '(0 3 6) 10)
                  '((0 . 10) (4 . 9) (0 . 8) (1 . 7) (3 . 6)
                    (3 . 5) (0 . 4) (6 . 3) (3 . 2) (0 . 1))))
  (assert (= 436 (last-spoken-value (play-until '(0 3 6) 2020))))
  (assert (= 249 (part-1))))
