(defpackage :aoc2020.09
  (:use :aoc2020)
  (:export #:test))

(in-package :aoc2020.09)

(defun sum-p (value &rest elements)
  (block nil
    (map-combinations (lambda (v)
                        (destructuring-bind (a b) v
                          (unless (= a b)
                            (when (= value (+ a b))
                              (return nil)))))
                      elements
                      :length 2)
    value))

(defmacro part-1-expand (size stream)
  (let ((item (gensym "NEXT"))
        (prev (loop repeat size collect (gensym "PREV"))))
    `(z:collect-first
      (z:choose
       (z:mapping
        (((,@prev ,item) (z:chunk ,(1+ size) 1 (z:scan-stream ,stream))))
        (sum-p ,item ,@prev))))))

(defun part-1 ()
  (with-input (s 9)
    (part-1-expand 26 s)))

(defvar *part-1* 393911906)

(define-test test
  (with-input-from-string (in "35 20 15 25 47 40 62 55 65 95 102 117 150 182 127 219 299 277 309 576")
    (assert (= 127 (part-1/size 5 in))))
  (assert (= *part-1* (part-1))))
