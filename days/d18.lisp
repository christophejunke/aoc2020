(defpackage :aoc2020.18
  (:use :aoc2020)
  (:export #:test))

(in-package :aoc2020.18)

(defrule int (+ (digit-char-p character))
  (:text t)
  (:function parse-integer))

(defrule op (and #\space (or #\+ #\*) #\space)
  (:function second)
  (:text t)
  (:function intern))

(defrule par (and #\( exp #\))
  (:function second))

(defrule atom (or par int))

(defrule binop (and exp op atom)
  (:destructure (e op a)
    `(,op ,e ,a)))

(defrule exp (or binop atom))

;; part 2

(defrule apar (and #\( aexp #\))
  (:function second))

(defrule aatom (or apar int))
(defrule add/atom (or add aatom))

(defrule add (and add/atom " + " add/atom)
  (:destructure (lhs add rhs)
    (declare (ignore add))
    `(+ ,lhs ,rhs)))

(defrule mult (and aexp " * " aexp)
  (:destructure (lhs mult rhs)
    (declare (ignore mult))
    `(* ,lhs ,rhs)))

(defrule aexp (or mult add aatom))

(defun calc-1 (line)
  (eval (parse 'exp line)))

(defun calc-2 (line)
  (eval (parse 'aexp line)))

(defun part-n (calc)
  (fold-input-lines 18 (lambda (line acc) (+ acc (funcall calc line))) 0))

(defun part-1 ()
  (part-n #'calc-1))

(defun part-2 ()
  (part-n #'calc-2)))

(define-test test
  (assert (= (calc-1 "2 * 3 + (4 * 5)") 26))
  (assert (= (calc-1 "5 + (8 * 3 + 9 + 3 * 4 * 3)") 437))
  (assert (= (calc-1 "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))") 12240))
  (assert (= (calc-1 "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2") 13632))
  (assert (= (calc-2 "1 + (2 * 3) + (4 * (5 + 6))") 51))
  (assert (= (calc-2 "2 * 3 + (4 * 5)") 46))
  (assert (= (calc-2 "5 + (8 * 3 + 9 + 3 * 4 * 3)") 1445))
  (assert (= (calc-2 "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))") 669060))
  (assert (= (calc-2 "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2") 23340))
  (assert (= (part-1) 131076645626))
  (assert (= (part-2) 109418509151782)))
