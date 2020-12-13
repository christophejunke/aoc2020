(defpackage :aoc2020.13
  (:use :aoc2020)
  (:export #:test))

(in-package :aoc2020.13)

(defun input ()
  (with-input (in 13)
    (values (parse-integer (read-line in))
            (delete nil
                    (mapcar (lambda (s)
                              (ignore-errors
                               (parse-integer s)))
                            (split #\, (read-line in)))))))

(defun part-1 ()
  (destructuring-bind (earliest bus)
      (multiple-value-bind (time buses) (input)
        (extremum (loop
                    for bus in buses
                    collect (list (- (* (1+ (floor time bus))
                                        bus) 
                                     time)
                                  bus))
                  #'<
                  :key #'first))
    (* earliest bus)))

(define-test test
  (assert (= 370 (part-1))))

(defun parse-2 (line)
  (loop 
    for offset from 0
    for string in (split #\, line)
    for value = (ignore-errors (parse-integer string))
    when value
      collect value into vals
      and collect offset into offs
    finally (return 
              (values (coerce vals 'vector) offs))))

(defun input-2 ()
  (with-input (in 13)
    (read-line in)
    (parse-2 (read-line in))))

(input-2)

(defparameter *sample*
  (parse-2 "7,13,x,x,59,x,31,19"))

(defun n (vals &optional (index 0 ip))
  (if ip
      (aref vals index)
      (reduce #'* vals)))

(n (parse-2 "7,13,x,x,59,x,31,19"))

(defun ^n (vals i &optional (n (n vals)))
  (/ n (aref vals i)))

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

(defun ei (vals n i)
  (let ((^ni (^n vals i n))
        (ni (aref vals i)))
    (destructuring-bind (gcd u vi) (euclide ni ^ni)
      (declare (ignore gcd u))
      (* (mod vi ni) ^ni))))

(defun all-ei (vals)
  (loop 
    with n = (n vals)
    for i below (length vals)
    collect (ei vals n i)))

(defun solve (vals offs &aux (n (n vals)) (eis (all-ei vals)))
  (values (mod (loop
                 for e in eis
                 for o in offs
                 sum (* e o))
               n)
          n
          eis))

t + column_index modulo bus_id = 0

(multiple-value-bind (vals offs) (parse-2 "7,13,x,x,59,x,31,19")
  (solve vals (mapcar #'- offs)))

(multiple-value-bind (vals offs) (input-2)
  (solve vals (mapcar #'- offs)))
;  => 894954360381385
; 1026798965750753
; (141627443551828 888042348757408 117162481036536 631876286615848
;  108084101657974 446434332935110 592487131673411 275482649347763
;  905999087427135)
                                        ; => 1068781, 3162341, (903526 243257 1875965 306033 2995902)

 ; => 23, (-3 5 7)

(list (n #(3 5 7) 0)
      (^n #(3 5 7) 0))



;; ax + by = c
;;
;; 7x = 13y + 1
;; 7x - 13y = 1
;;
(euclide 7 -13)
 ; => (1 2 1)

(defun gensol (a b)
  (destructuring-bind (g x0 y0) (euclide a b)
    (assert (= g 1) () "gcd(x,y) should be 1")
    (lambda (k)
      (list (+ x0 (* b k))
            (- y0 (* a k))))))

(defun equ (a b c)
  (lambda (x y)
    (+ (* a x)
       (* b y)
       (- c))))

(funcall (equ 7 -13 1) 2 1)

(loop
  with (a b c) = '(7 -13 1)
  with f = (equ a b c)
  with g = (gensol a b)
  for k below 10
  for (x y) = (funcall g k)
  collect (list x y (funcall f x y))) 

(loop 
  for k from 0 to 10 
  for x = (+ 2 (* 13 k))
  for y = (+ -1 (* (- 7) k))
  for s = (+ (* 7 x) (- (* 13 y) 1))
  collect (list x (* 7 x)
                y (- (* 13 y) 1)
                s (mod s (lcm 7 13))))

(loop 
  for (a b) on (nth-value 1 (input))
  while b
  collect (gcd a b))
 ; => (1 1 1 1 1 1 1 1)

(defvar *sample*
  '((7 . 0) (13 . 1) (59 . 2) (31 . 3) (19 . 4)))
()
   7 * a + 0
= 13 * b + 1
= 59 * c + 2
= 31 * d + 3
= 19 * e + 4

7a -13b -59c -31d -19e = (+ 4 3 2 1) = 10

(reduce #'gcd (mapcar #'car *sample*))

(reduce #'max (mapcar #'car *sample*))

py + 59z = 10

(gensol 7 10)

(euclide 1 19) 1
0

(defun solve-2 (a b c &optional (k 0 kp))
  (destructuring-bind (d u v) (euclide a b)
    (flet ((equation (k)
             `(list (/ (- ,(* c u) (* ,k ,b)) ,d)
                    (/ (+ ,(* c v) (* ,k ,a)) ,d))))
      (cond
        ((eq k t) (equation 'k))
        (kp (eval (equation k)))
        (t (coerce `(lambda (k) ,(equation 'k)) 'function))))))

;; => (20 10)

(defun test (a b c &optional (k 0))
  (zerop
   (apply (equ a b c) (funcall (solve-2 a b c) k))))

(test 7 -13 1)
 ; => T

7a -13b -59c -31d -19e = 10

(gcd 7 -13 -59 -31) == 1
y - 19e = 10

(funcall (solve-2 1 -19 10 t) 0)


10x1 + 15x2 + 6x3 = 73
(gcd 10 15) ; => 5 (3 bits, #x5, #o5, #b101)
5y + 6x3 = 73

(defun solve-n (s xs k)
  (assert xs)
  (destructuring-bind (x . xs) xs
    (if (rest xs)
        (let ((gcd-xs (apply #'gcd xs)))
          (destructuring-bind (sy sx) (funcall (solve-2 gcd-xs x s) k)
            (cons sx (solve-n (* sy gcd-xs) xs k))))
        (solve-2 x (first xs) s k))))

(solve-n 73 '(10 15 6) 0)
 ; => (76 -231 463)

(+ (* 76 10) (* -231 15) (* 463 6))
 ; => 73 (7 bits, #x49, #o111, #b1001001)

(solve-n 18 '(7 -13 -59 -31 -19) 100)



(list (solve-2 7 -13 1 t)
      (solve-2 7 -59 4 t)
      (solve-2 7 -31 6 t)
      (solve-2 7 -19 7 t))

((lambda (k)
   (list
    (LIST (/ (- 2 (* K -13)) 1)
          (/ (+ 1 (* K 7)) 1))
    (LIST (/ (- 68 (* K -59)) 1)
          (/ (+ 8 (* K 7)) 1))
    (LIST (/ (- 54 (* K -31)) 1)
          (/ (+ 12 (* K 7)) 1))
    (LIST (/ (- 56 (* K -19)) -1)
          (/ (+ 21 (* K 7)) -1))))
 0)

(loop
  for k from 0 below 1000000
  for (b7-0 b-13) = (solve-2 7 -13 1 k) 
  for (b7-1 b-59) = (solve-2 7 -59 4 k) 
  for (b7-2 b-31) = (solve-2 7 -31 6 k) 
  for (b7-3 b-19) = (solve-2 7 -19 7 k)
  when (= b7-0 b7-1 b7-2 b7-3)
    collect k)

(loop 
  for k below 10000000
  for (a c) = (solve-2 7 -59 4 k)
  until (eql a 1068781)
  finally (return k))

(apply #'lcm '(2 68 54 -56 1 8 12 -21))
 ; => 25704 (15 bits, #x6468)
 ; => 25704 (15 bits, #x6468)

(list
 (/ (+ 1068781 0) 7)
 (/ (+ 1068781 1) 13)
 (/ (+ 1068781 4) 59)
 (/ (+ 1068781 6) 31)
 (/ (+ 1068781 7) 19))



(* 130 7) ; => 910 (10 bits, #x38E)
7a -13b -59c -31d -19e = 18


(let ((s1 (solve-2 5 6 73)))
  
  )

(loop for (n . o) in *sample*
      do
         (format t "~&;; ~5d + ~d " n o)
         (loop 
           for c below 500
           do (princ (if (= 0 (mod (- c o) n)) #\X #\-)))
         (terpri))

;;     7 + 0 X------X------X------X------X------X------X------X------X------X------X------X------X------X------X------X------X------X------X------X------X------X------X------X------X------X------X------X------X------X------X------X------X------X------X------X------X------X------X------X------X------X------X------X------X------X------X------X------X------X------X------X------X------X------X------X------X------X------X------X------X------X------X------X------X------X------X------X------X------X------X------X--
;;    13 + 1 -X------------X------------X------------X------------X------------X------------X------------X------------X------------X------------X------------X------------X------------X------------X------------X------------X------------X------------X------------X------------X------------X------------X------------X------------X------------X------------X------------X------------X------------X------------X------------X------------X------------X------------X------------X------------X------------X------------X----
;;    59 + 2 --X----------------------------------------------------------X----------------------------------------------------------X----------------------------------------------------------X----------------------------------------------------------X----------------------------------------------------------X----------------------------------------------------------X----------------------------------------------------------X----------------------------------------------------------X-------------------------
;;    31 + 3 ---X------------------------------X------------------------------X------------------------------X------------------------------X------------------------------X------------------------------X------------------------------X------------------------------X------------------------------X------------------------------X------------------------------X------------------------------X------------------------------X------------------------------X------------------------------X------------------------------X
;;    19 + 4 ----X------------------X------------------X------------------X------------------X------------------X------------------X------------------X------------------X------------------X------------------X------------------X------------------X------------------X------------------X------------------X------------------X------------------X------------------X------------------X------------------X------------------X------------------X------------------X------------------X------------------X------------------X-



(loop for x in (nth-value 1 (input))
      for i from 0
      collect (cons i x))

;; ((0 . 29) (1 . 37) (2 . 631) (3 . 13) (4 . 19) (5 . 23) (6 . 383) (7 . 41) (8 . 17)


