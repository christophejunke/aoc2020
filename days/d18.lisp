(defpackage :aoc2020.18
  (:use :aoc2020)
  (:export #:test-solve
           #:test-parsing
           #:test-associativity))

(in-package :aoc2020.18)

(in-readtable :fare-quasiquote)

;; COMMON AST

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

;; EXP: a number or (A O1 B O2 C ...) where A B C are EXP
(defrule exp atom*
  (:destructure (lhs &rest ops)
                (list* lhs (mappend #'identity ops))))

(defmacro check-parsing (string ast)
  `(assert (equalp (parse 'exp ,string) ',ast)))

(define-test test-parsing
  (check-parsing "5 + (8 * 3 + 9 + 3 * 4 * 3)"
                 (5 + (8 * 3 + 9 + 3 * 4 * 3)))
  (check-parsing "5 + 2 ^ 4"
                 (5 + 2 ^ 4)))

(defun group-by-priority (expr priority<)
  (flet ((recurse (e) (group-by-priority e priority<)))
    (ematch expr
      ((type number) expr)
      ((list a) (recurse a))
      ((list a o b) (list (recurse a) o (recurse b)))
      ((list* a o1 b o2 rest)
       (if (funcall priority< o1 o2)
           (recurse `(,a ,o1 (,b ,o2 ,@rest)))
           (recurse `((,a ,o1 ,b) ,o2 ,@rest)))))))

(defun priority< (right-assoc-p priority)
  (lambda (o1 o2)
    (if (eq o1 o2)
        (funcall right-assoc-p o1)
        (< (funcall priority o1)
           (funcall priority o2)))))

(defun rank (x sequence)
  (or (position-if (lambda (v)
                     (typecase v
                       (symbol (eq x v))
                       (sequence (find x v))))
                   sequence)
      (error "~a not found in ~a" x sequence)))

(defmacro check-group (< exp expected)
  `(assert (equalp (group-by-priority (parse 'exp ,exp) ,<)
                   ',expected)))

(define-test test-associativity
  (let ((p< (priority< (lambda (o) (member o '(^)))
                       (lambda (o) (rank o '(* + ^))))))
    (check-group p<
                 "5 + (8 * 3 + 9 + 3 * 4 * 3)"
                 (5 + (8 * ((((3 + 9) + 3) * 4) * 3))))
    (check-group p<
                 "3 + 4 + 5 + 2 ^ 5 ^ 6"
                 (((3 + 4) + 5) + (2 ^ (5 ^ 6))))))

(defun e (expr)
  (ematch expr
    ((type number) expr)
    (`(,x + ,y) (+ (e x) (e y)))
    (`(,x * ,y) (* (e x) (e y)))))

(defun calc-1 (line)
  (e (group-by-priority (parse 'exp line) (constantly nil))))

(defun calc-2 (line)
  (flet ((priority< (o1 o2) (and (eq o1 '*) (eq o2 '+))))
    (e (group-by-priority (parse 'exp line) #'priority<))))

(defun part-n (calc)
  (fold-input-lines 18 (lambda (line acc) (+ acc (funcall calc line))) 0))

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
