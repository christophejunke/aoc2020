(defpackage :aoc2020.18
  (:use :aoc2020)
  (:export #:test-solve
           #:test-parsing
           #:test-associativity))

(in-package :aoc2020.18)

(in-readtable :fare-quasiquote)

(defun intern-operator (op)
  (intern op :aoc2020.18))

(defrule int (+ (digit-char-p character))
  (:text t)
  (:function parse-integer))

(defrule op (and #\space (or #\+ #\* #\^) #\space)
  (:function second)
  (:text t)
  (:function intern-operator))

(defrule par (and #\( exp #\))
  (:function second))

(defrule atom (or par int))

(defrule atom+ (and atom (+ (and op atom)))
  (:destructure (lhs next) (cons lhs next)))

(defrule atom* (or atom+ atom))

;; EXP is either a NUMBER
;;     or a list (E0 O1 E1 { On En })
;;  where E0 to En are EXP
;;    and O1 to On are operators

(defrule exp atom*
  (:destructure (lhs &rest ops)
                (list* lhs (mappend #'identity ops))))

;; Group terms by priorities and associativity

(defun group-terms (expr right-bind-p)
  (flet ((recurse (e) (group-terms e right-bind-p)))
    (ematch expr
      ((type number) expr)
      ((list a) (recurse a))
      ((list a o b) (list (recurse a) o (recurse b)))
      ((list* a o1 b o2 rest)
       (if (funcall right-bind-p o1 o2)
           (recurse `(,a ,o1 (,b ,o2 ,@rest)))
           (recurse `((,a ,o1 ,b) ,o2 ,@rest)))))))

(defun parse-line (line <)
  (group-terms (parse 'exp line) <))

(defun right-bind-p (right-assoc-p priority)
  (lambda (o1 o2)
    (if (eq o1 o2)
        (funcall right-assoc-p o1)
        (< (funcall priority o1)
           (funcall priority o2)))))

;;; EVAL

(defun e (expr)
  (ematch expr
    ((type number) expr)
    (`(,x + ,y) (+ (e x) (e y)))
    (`(,x * ,y) (* (e x) (e y)))
    (`(,x ^ ,y) (expt (e x) (e y)))))

;;; SUM VALUES

(defun calc-1 (line)
  (e (parse-line line (constantly nil))))

(defun calc-2 (line)
  (flet ((right-bind-p (o1 o2) (and (eq o1 '*) (eq o2 '+))))
    (e (parse-line line #'right-bind-p))))

(defun sum-line (calc)
  (lambda (line acc) (+ acc (funcall calc line))))

(defun part-n (calc)
  (fold-input-lines 18 (sum-line calc) 0))

;;; TESTS

(defmacro check-parsing (string ast)
  `(assert (equalp (parse 'exp ,string) ',ast)))

(defmacro check-group (< exp expected)
  `(assert (equalp (group-terms (parse 'exp ,exp) ,<)
                   ',expected)))

(define-test test-parsing
  (check-parsing "5 + (8 * 3 + 9 + 3 * 4 * 3)"
                 (5 + (8 * 3 + 9 + 3 * 4 * 3)))
  (check-parsing "5 + 2 ^ 4"
                 (5 + 2 ^ 4)))

(define-test test-associativity
  (let ((binder (right-bind-p (lambda (o) (member o '(^)))
                              (lambda (o) (rank o '(* + ^))))))
    (check-group binder
                 "5 + (8 * 3 + 9 + 3 * 4 * 3)"
                 (5 + (8 * ((((3 + 9) + 3) * 4) * 3))))
    (check-group binder
                 "3 + 4 + 5 + 2 ^ 5 ^ 6"
                 (((3 + 4) + 5) + (2 ^ (5 ^ 6))))))

(define-test test-solve
  (assert (= (calc-1 "2 * 3 + (4 * 5)") 26))
  (assert (= (calc-1 "5 + (8 * 3 + 9 + 3 * 4 * 3)") 437))
  (assert (= (calc-1 "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))") 12240))
  (assert (= (calc-1 "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2") 13632))
  (assert (= (calc-2 "1 + (2 * 3) + (4 * (5 + 6))") 51))
  (assert (= (calc-2 "2 * 3 + (4 * 5)") 46))
  (assert (= (calc-2 "5 + (8 * 3 + 9 + 3 * 4 * 3)") 1445))
  (assert (= (calc-2 "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))") 669060))
  (assert (= (calc-2 "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2") 23340))
  (assert (= (part-n #'calc-1) 131076645626))
  (assert (= (part-n #'calc-2) 109418509151782)))
