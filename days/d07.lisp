(defpackage :aoc2020.07
  (:use :aoc2020)
  (:export #:test))

(in-package :aoc2020.07)

(defrule word (+ (alpha-char-p character))
  (:text t))

(defrule color (and word ws word)
  (:text t))

(defrule quantity (+ (digit-char-p character))
  (:text t)
  (:function parse-integer))

(defrule ws " "
  (:text t))

(defrule bag (and quantity ws color ws (or "bags" "bag"))
  (:lambda (list) (list (first list) (third list))))

(defrule bags (and bag (or (and ", " bags) (and "")))
  (:destructure (head tail) (cons head (second tail))))

(defrule yes (and color ws "bags contain" ws bags ".")
  (:lambda (items) (list* (first items) (fifth items))))

(defrule no (and color ws "bags contain no other bags.")
  (:lambda (items) (list (first items))))

(defrule sentence (or yes no))

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
