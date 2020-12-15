(defpackage :aoc2020.15
  (:use :aoc2020)
  (:export #:test))

(in-package :aoc2020.15)

(defvar *input* '(15 12 0 14 3 1))

(defun play-until (list target-turn)
  (declare (type fixnum target-turn))
  (let* ((env (z:collect-hash (z:scan 'list list)
                              (z:scan-range :from 1
                                            :type 'fixnum)))
         (turn (length list)) (past-turn turn) (last-spoken 0))
    (declare (optimize (speed 3) (debug 0) (safety 1))
             (type fixnum turn past-turn last-spoken))
    (loop
      (setf last-spoken (- turn past-turn))
      (incf turn)
      (shiftf past-turn (gethash last-spoken env turn) turn)
      (when (>= turn target-turn)
        (return last-spoken)))))

(defun part-1 ()
  (play-until *input* 2020))

(defun part-2 ()
  (play-until *input* 30000000))

(define-test test
  (assert (= 436 (play-until '(0 3 6) 2020)))
  (assert (= 249 (part-1))))

(define-test test-part-2
  (assert (= 41687 (part-2))))

