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

(defun rules (direction &optional (in 7))
  (let ((h (make-hash-table :test #'equal)))
    (do-input-lines (line in h)
      (destructuring-bind (bag . bags) (sentence line)
        (ecase direction
          (:forward (setf (gethash bag h) bags))
          (:backward
           (loop :for (_ b) :in bags :do (push bag (gethash b h)))))))))

(defun part-1 (&optional (in 7) &aux (h (rules :backward in)))
  (labels ((visit (c)
             (reduce #'union
                     (mapcar #'visit (gethash c h))
                     :initial-value (list c))))
    (1- (length (visit "shiny gold")))))

(defun part-2 (&optional (in 7) &aux (h (rules :forward in)))
  (labels ((visit (c)
             (1+ (loop :for (n b) :in (gethash c h)
                       :sum (* n (visit b))))))
    (1- (visit "shiny gold"))))

(define-test test
  (assert (= 4 (part-1 "07-t1")))
  (assert (= 121 (part-1)))
  (assert (= 126 (part-2 "07-t2")))
  (assert (= 3805 (part-2))))

