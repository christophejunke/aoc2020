(defpackage :aoc2020.12
  (:use :aoc2020)
  (:export #:test))

(in-package :aoc2020.12)

;; #C(0 1)
;;
;; N
;; ^
;; |
;; |____> E  #C(1 0)

(define-constant +east+  #C(+1 +0))
(define-constant +west+  #C(-1 +0))
(define-constant +north+ #C(+0 +1))
(define-constant +south+ #C(+0 -1))

(define-constant +left+  #C(+0 +1))
(define-constant +right+ #C(+0 -1))

(defun parse-line (line)
  (scanner-bind ("%c%d" c n) line
    (flet ((mov (v) (list 'mov (* v n)))
           (rot (v) (list 'rot v))
           (fwd (v) (list 'fwd v)))
      (case c
        (#\N (mov +north+))
        (#\S (mov +south+))
        (#\E (mov +east+))
        (#\W (mov +west+))
        (#\L (rot +left+))
        (#\R (rot +right+))
        (#\F (fwd n))))))

(defun input (&optional (in 12))
  (with-input (stream in)
    (z:collect 'vector
      (z:map-fn t #'parse-line (z:scan-stream stream #'read-line)))))

(defstruct ship (dir +east+) (pos 0))

(defun manhattan (complex)
  (+ (abs (realpart complex))
     (abs (imagpart complex))))

(defun update (ship command)
  (with-accessors ((dir ship-dir) (pos ship-pos)) ship
    (destructuring-bind (cmd &optional val) command
      (case cmd
        (fwd (incf pos (* dir val)))
        (rot (setf dir (* dir val)))
        (mov (incf pos val))))))

(define-test example
  (let ((ship (make-ship))
        (commands (input "12-sample")))
    (map () (curry #'update ship) commands)
    (assert (= (manhattan (ship-pos ship)) 25))))
