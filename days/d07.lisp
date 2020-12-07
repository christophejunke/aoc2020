(defpackage :aoc2020.07
  (:use :aoc2020)
  (:export #:test))

(in-package :aoc2020.07)

(defrule word (+ (alpha-char-p character))
  (:text t))

(defrule color (and word ws word)
  (:text t))

(defrule digit (digit-char-p character)
  (:function digit-char-p))

(defrule bag (or "bags" "bag")
  (:constant :bag))

(defrule ws " "
  (:text t))

(defrule desc (and digit ws color ws bag)
  (:function (lambda (list)
               (vector (first list) (third list)))))

(defrule content (or (and desc ", " content) desc)
  (:function (lambda (in)
               (mapcar (lambda (u) (coerce u 'list))
                       (etypecase in
                         (vector (list in))
                         (cons (cons (first in) (third in))))))))

(defrule yes (and color ws "bags contain" ws content ".")
  (:function (lambda (items) (list* (first items) (fifth items)))))

(defrule no (and color ws "bags contain no other bags.")
  (:function (lambda (items) (list (first items)))))

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
      (remhash "shiny gold" roots)
      (hash-table-count roots))))

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
