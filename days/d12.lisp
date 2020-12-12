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

(defmacro move (&rest args)
  `(list 'mov (compass ,@args)))

(defmacro compass (&key (east 0) (north 0) (south 0) (west 0))
  `(complex (- ,east ,west) (- ,north ,south)))

(defmacro turn (&key (left 0) (right 0))
  `(list 'rot (expt #C(0 1) (/ (- ,left ,right) 90))))

(defmacro forward (n)
  `(list 'fwd ,n))

(defun parse-line (line)
  (scanner-bind ("%c%d" c n) line
    (case c
      (#\N (move :north n))
      (#\S (move :south n))
      (#\E (move :east n))
      (#\W (move :west n))
      (#\L (turn :left n))
      (#\R (turn :right n))
      (#\F (forward n)))))

(defun input (&optional (in 12))
  (map-input in :transform #'parse-line))

(defstruct ship (dir +east+) (pos 0))

(defmacro define-update (name (pos dir val) &body clauses)
  (assert (equalp '(fwd mov rot) (sort (mapcar #'car clauses) #'string<))
          ()
          "clauses should exactly cover FWD ROT and MOV")
  (with-gensyms (ship command cmd)
    `(defun ,name (,ship ,command)
       (prog1 ,ship
         (with-accessors ((,dir ship-dir) (,pos ship-pos)) ,ship
           (destructuring-bind (,cmd &optional ,val) ,command
             (ecase ,cmd
               ,@clauses)))))))

(define-update up1 (pos dir val)
  (fwd (incf pos (* dir val)))
  (rot (setf dir (* dir val)))
  (mov (incf pos val)))

;; the waypoint is in fact the ship's DIR slot
(define-update up2 (pos wpt val)
  (fwd (incf pos (* wpt val)))
  (rot (setf wpt (* wpt val)))
  (mov (incf wpt val)))

;; solve

(defun manhattan (complex)
  (+ (abs (realpart complex))
     (abs (imagpart complex))))

(defun navigate (updater in ship)
  (manhattan (ship-pos (reduce updater (input in) :initial-value ship))))

(defun part-1 (&optional (in 12))
  (navigate #'up1 in (make-ship :dir (compass :east 1))))

(defun part-2 (&optional (in 12))
  (navigate #'up2 in (make-ship :dir (compass :north 1 :east 10))))

(define-test test
  (assert (= (part-1 #P"12-sample") 25))
  (assert (= (part-2 #P"12-sample") 286))
  (assert (= (part-1) 415))
  (assert (= (part-2) 29401)))
