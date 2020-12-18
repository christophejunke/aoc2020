(defpackage :aoc2020.18
  (:use :aoc2020)
  (:export #:test-solve
           #:test-parsing
           #:test-associativity
           #:test-as-lisp))

(in-package :aoc2020.18)

;;; PRATT

(defun unnext-char (c)
  (when c (unread-char c)))

(defun next-char ()
  (read-char *standard-input* nil nil))

(defun expect (c)
  (let ((nc (next-char)))
    (unless (eql c nc)
      (error "expected char ~a but read ~a instead" c nc))))

(defun peek-or-end (&optional (v t))
  (peek-char v *standard-input* nil nil))

(defun is (char)
  (lambda (c) (and (eql c char) c)))

(defun peek-read-if (test)
  (when-let* ((c (peek-or-end nil))
              (r (funcall test c)))
    (prog1 r
      (read-char))))

(defun parse/integer (&optional (token nil))
  (if-let (v (peek-read-if #'digit-char-p))
    (parse/integer (if token (+ (* token 10) v) v))
    token))

(defun parse/paren ()
  (when (peek-read-if (is #\())
    (prog1 (parse/exp)
      (expect #\)))))

(defun peek/operator (&aux (c (peek-char)))
  (case c
    (#\+ (values '+ 3 4 #'next-char))
    (#\* (values '* 1 2 #'next-char))))

(defun parse/exp (&optional (min-bp 0))
  (unless (eql #\) (peek-char t))
    (let ((lhs (or (parse/integer)
                   (parse/paren)
                   (error "NO"))))
      (loop
        (case (peek-or-end)
          ((#\) nil) (return lhs)))
        (multiple-value-bind (op lbp rbp commit) (peek/operator)
          (unless op
            (error "no operator"))
          (when (< lbp min-bp)
            (return lhs))
          (funcall commit)
          (let ((rhs (or (parse/exp rbp)
                         (error "no rhs!"))))
            (setf lhs `(,op ,lhs ,rhs))))))))

(with-input-from-string (*standard-input* "125 + 7 * 8 * 8 + 3")
  (parse/exp))

;; (* (* (+ 125 7) 8) (+ 8 3))

;;; EVAL

(defun i (expr)
  (ematch expr
    ((type number) expr)
    (`(,x + ,y) `(+ ,(i x) ,(i y)))
    (`(,x * ,y) `(* ,(i x) ,(i y)))
    (`(,x ^ ,y) `(expt ,(i x) ,(i y)))))

(defun e (expr)
  (ematch expr
    ((type number) expr)
    (`(,x + ,y) (+ (e x) (e y)))
    (`(,x * ,y) (* (e x) (e y)))
    (`(,x ^ ,y) (expt (e x) (e y)))))

(defun fuse-p (op)
  (member op '(+ *)))

(defun simplify (expr)
  (flet ((fuse (op1 term)
           (multiple-value-bind (term op2) (simplify term)
             (if (and (eq op1 op2) (fuse-p op1))
                 (rest term)
                 (list term)))))
    (ematch expr
      ((type number) expr)
      (`(,op ,lhs ,rhs) (values `(,op ,@(fuse op lhs)
                                      ,@(fuse op rhs))
                                op)))))

(defun as-lisp (expr)
  (simplify (i expr)))

(defvar *lisp<*
  (right-bind-p (lambda (o) (member o '(^)))
                (lambda (o) (rank o '(+ * ^)))))

(defmacro check-as-lisp (in out)
  `(assert (equalp (as-lisp (parse-line ,in *lisp<*)) ',out)))

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

(define-test test-as-lisp
  (check-as-lisp "5 + 7 + 8 + 1 + 3"
                 (+ 5 7 8 1 3))
  (check-as-lisp "5 ^ 7 ^ 8 ^ 1 ^ 3"
                 (expt 5 (expt 7 (expt 8 (expt 1 3)))))
  (check-as-lisp "5 + 7 * 8 * 9"
                 (+ 5 (* 7 8 9))))

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
