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

(defun part-2-input ()
  (with-input (in 9)
    (z:collect '(vector fixnum)
      (z:choose
       (z:mapping (((v) (z:scan-stream in)))
                  (and (< v *part-1*) v))))))

(defun part-2 ()
  (let ((all (part-2-input)))
    (let ((window (make-array 3
                              :element-type 'fixnum
                              :displaced-to all)))
      (flet ((adjust (size offset)
               (setf window
                     (adjust-array window
                                   size
                                   :element-type 'fixnum
                                   :displaced-to all
                                   :displaced-index-offset offset))))
        (loop
          for size from 3 below (length all)
          do (loop
               for offset from 0 below (- (length all) size)
               do (adjust size offset)
                  (when (= *part-1* (reduce #'+ window))
                    (return-from part-2
                      (+ (reduce #'min window)
                         (reduce #'max window))))))))))


(define-test test
  (with-input-from-string (in "35 20 15 25 47 40 62 55 65 95 102 117 150 182 127 219 299 277 309 576")
    (assert (= 127 (part-1-expand 5 in))))
  (assert (= *part-1* (part-1)))
  (assert (=  59341885 (part-2))))
