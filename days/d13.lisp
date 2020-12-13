(defpackage :aoc2020.13
  (:use :aoc2020)
  (:export #:test))

(in-package :aoc2020.13)

(defun parse-bus-line (line)
  (loop 
    for offset from 0
    for string in (split #\, line)
    for value = (ignore-errors (parse-integer string))
    when value
      collect value into vals
      and collect offset into offs
    finally (return (values (coerce vals 'vector) offs))))

(defun input ()
  (with-input (in 13)
    (let ((line (parse-integer (read-line in))))
      (multiple-value-bind (vals offs) (parse-bus-line (read-line in))
        (values line vals offs)))))

(defun time-to-next-departure (time period)
  ;; next multiple of PERIOD after TIME ...
  (let ((next-start (* (ceiling time period) period)))
    ;; ... relative to TIME
    (- next-start time)))

(defun part-1 ()
  (multiple-value-bind (origin buses) (input)
    (flet ((waiting-time (bus) (time-to-next-departure origin bus)))
      (let ((earliest (extremum buses #'< :key #'waiting-time)))
        (* earliest (waiting-time earliest))))))

;; EXTENDED EUCLIDEAN ALGORITHM
;; https://en.wikipedia.org/wiki/Extended_Euclidean_algorithm

(defun %euclide (r1 u1 v1 r2 u2 v2)
  (if (= 0 r2)
      (list r1 u1 v1)
      (let ((d (floor r1 r2)))
        (%euclide r2 u2 v2 
                  (- r1 (* r2 d))
                  (- u1 (* u2 d))
                  (- v1 (* v2 d))))))

(defun euclide (a b)
  (%euclide a 1 0 b 0 1))

;; BÉZOUT'S IDENTITY
;; https://en.wikipedia.org/wiki/B%C3%A9zout%27s_identity

;; check if "ax + by = c"
(defun check-equation (a x b y c)
  (= c (+ (* a x) (* b y))))

(defun bezout (a b c &optional (k 0 kp))
  (destructuring-bind (d u v) (euclide a b)
    (flet ((equation (k)
             `(cons (/ (- ,(* c u) (* ,k ,b)) ,d)
                    (/ (+ ,(* c v) (* ,k ,a)) ,d))))
      (cond
        ;; return a function of K
        ((not kp) (coerce `(lambda (k) ,(equation 'k)) 'function))
        ;; return the equation, with K the symbol associated with K factor
        ((symbolp k) (equation k))
        ;; otherwise, compute value with K, a number
        (t (eval (equation k)))))))

;; e.g. 12x + 42y = 30
;;
;; (bezout 12 42 30 0)
;; => (-15 . 5)
;;
;; (check-equation 12 -15 42 5 30)
;; => T

;; CHINESE REMAINDER THEOREM
;; https://en.wikipedia.org/wiki/Chinese_remainder_theorem#Computation
;; (nb. for this puzzle the gcd of all periods is 1)

(defun n-product (vals)
  (reduce #'* vals))

(defun n (vals i)
  (aref vals i))

(defun ^n (vals i &optional (p (n-product vals)))
  (/ p (aref vals i)))

(defun ei (ni ^ni)
  (destructuring-bind (ui . vi) (bezout ni ^ni 1 0)
    (declare (ignore ui))
    (* (mod vi ni) ^ni)))

(defun e (vals i p)
  (ei (n vals i) (^n vals i p)))

(defun all-eis (vals p)
  (loop
    for i below (length vals)
    collect (e vals i p)))

(defun solve (values offsets)
  (let* ((product (n-product values))
         (all-eis (all-eis values product)))
    (mod (reduce #'+ (mapcar #'* all-eis offsets)) product)))

(defun part-2 ()
  (multiple-value-bind (_ values offsets) (input)
    (declare (ignore _))
    (solve values (mapcar #'- offsets))))

(define-test test
  (assert (= (part-1) 370))
  (assert (= (part-2) 894954360381385)))
