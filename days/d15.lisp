(defpackage :aoc2020.15
  (:use :aoc2020)
  (:export #:test))

(in-package :aoc2020.15)

(defvar *input* '(15 12 0 14 3 1))

(defstruct (env (:constructor make-env%)) hash last turn)

(defun make-env (list)
  (loop
    :with h = (make-hash-table)
    :with last :and turn
    :for v in list
    :for i from 1
    :do (setf last v)
    :do (setf turn i)
    :do (setf (gethash v h) (cons i nil))
    :finally (return (make-env% :hash h :last last :turn turn))))

(defun last-spoken-value (env)
  (values (env-last env)
          (env-turn env)
          (cdr (gethash (env-last env) (env-hash env)))))

(defun speak (env turn value)
  (setf (env-turn env) turn)
  (setf (env-last env) value)
  (let ((cell (ensure-gethash value (env-hash env) (cons nil nil))))
    (setf (cdr cell) (car cell))
    (setf (car cell) turn))
  (values env turn))

(defun play (env)
  (multiple-value-bind (last-spoken turn past-turn) (last-spoken-value env)
    (declare (ignore last-spoken))
    (if past-turn
        (speak env (1+ turn) (- turn past-turn))
        (speak env (1+ turn) 0))))

(defun play-until (list target-turn)
  (let ((env (make-env list)) (turn 0))
    (loop
      (multiple-value-setq (env turn) (play env))
      (when (>= turn target-turn)
        (return env)))))

(defun part-1 ()
  (last-spoken-value (play-until *input* 2020)))

(defun part-2 ()
  (last-spoken-value (play-until *input* 30000000)))

(define-test test
  (assert (= 436 (last-spoken-value (play-until '(0 3 6) 2020))))
  (assert (= 249 (part-1))))

;; unexported (long time)
(define-test test-part-2
  (assert (= 41687 (part-2))))
