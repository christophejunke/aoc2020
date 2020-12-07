(defpackage :aoc2020.07
  (:use :aoc2020)
  (:export #:test))

(in-package :aoc2020.07)

(defrule empty ""
  (:constant nil))

(defrule no-bag "no other bags"
  (:constant nil))

(defrule word (+ (alpha-char-p character))
  (:text t))

(defrule color (and word " " word)
  (:text t))

(defrule quantity (+ (digit-char-p character))
  (:text t)
  (:function parse-integer))

(defrule bag (and quantity " " color " " (or "bags" "bag"))
  (:lambda (ast) (list (first ast) (third ast))))

(defrule bags* (or (and ", " bags) (and ""))
  (:function second))

(defrule bags (and bag bags*)
  (:destructure (bag bags) (cons bag bags)))

(defrule sentence (and color " bags contain " (or no-bag bags) ".")
  (:lambda (ast) (cons (first ast) (third ast))))

(defun sentence (s)
  (parse 'sentence s))

(defun rules (&optional (in 7))
  (let ((forward (make-hash-table :test #'equal))
        (backward (make-hash-table :test #'equal)))
    (do-input-lines (line in (values forward backward))
      (destructuring-bind (bag . bags) (sentence line)
        (setf (gethash bag forward) bags)
        (loop for (_ b) in bags do (push bag (gethash b backward)))))))

(defun part-1 (&optional (in 7))
  (multiple-value-bind (forward backward) (rules in)
    (let ((roots (make-hash-table :test #'equal)))
      (labels ((visit (color)
                 (dolist (p (gethash color backward))
                   (visit p))
                 (when (gethash color forward)
                   (setf (gethash color roots) t))))
        (visit "shiny gold"))
      (1- (hash-table-count roots)))))

(defun part-2 (&optional (in 7))
  (multiple-value-bind (forward backward) (rules in)
    (declare (ignore backward))
    (labels ((visit (bag)
               (1+ (loop
                     for (n b) in (gethash bag forward)
                     sum (* n (visit b))))))
      (1- (visit "shiny gold")))))

(define-test test
  (assert (= 4 (part-1 "07-t1")))
  (assert (= 121 (part-1)))
  (assert (= 126 (part-2 "07-t2")))
  (assert (= 3805 (part-2))))
