(defpackage :aoc2020.09
  (:use :aoc2020)
  (:export #:test))

(in-package :aoc2020.09)

;; a bit like day 1, but there is not much to factor in common

(defun sum-of-two (value &rest elements)
  "Return VALUE if it cannot be a sum of two values in ELEMENTS"
  (prog1 nil
    (map-combinations (lambda (v)
                        (destructuring-bind (a b) v
                          (unless (= a b)
                            (when (= value (+ a b))
                              (return-from sum-of-two t)))))
                      elements
                      :length 2)))

;; (chunk M N Z) iterates over a serie Z by step N. For each
;; position P that is a multiple of N (the column), it produces M
;; values at this position (the rows). Each row represents a
;; continuous chunk of Z of size M, at a position P. For example,
;; let Z be this serie:
;;
;;    (z:make-series 'a 'b 'c 'd 'e 'g 'h 'i)
;;    => #Z(A B C D E G H I)
;;
;; Let's build the series of chunks of 4 elements by steps of 2:
;;
;;    (z:chunk 4 2 Z)
;;
;;     => #Z(A C E)
;;        #Z(B D G)
;;        #Z(C E H)
;;        #Z(D G I)
;;
;; Visually, this corresponds to the chunks in brackets:
;;
;;     ([A B C D] _ _ _ _)
;;     (_ _ [C D E G] _ _)
;;     (_ _ _ _ [E G H I])
;;
;; For PART-1, the sequence of numbers is split into chunks of
;; 25+1=26 series by step 1 to have a sliding window of 25
;; elements followed by the expected total.
;;
;; MAPPING iterates over all the series simultaneously, and binds
;; a variable for each value for each stream. The body of MAPPING
;; tells how to combine those values:
;;
;;     (mapping (((x1 ... xn total) <chunk>))
;;       (unless (sum-of-two total x1 .. xn) total))
;;
;; The value produced by each chunk is NIL if there is a pair that
;; sums to TOTAL (the normal case). If no such pair of numbers
;; exist, it returns TOTAL instead.
;;
;; Finally, this sequence Z is filtered as follow:
;;
;;     (collect-first (choose Z))
;;
;; CHOOSE only retains non-nil elements from Z, and collect-first
;; stops the iteration at the first value in a stream.

;; PART-1-EXPAND is a macro because for efficient compilation, it
;; is not possible to use MULTIPLE-VALUE-CALL to collect all the
;; series produces by CHUNK, and because CHUNK needs literal
;; arguments. Instead, the macro introduces 26 variables. It
;; should be possible to define our own SERIES iterator that uses
;; a circular buffer instead (see other git branches if I have
;; time).

(defmacro part-1-expand (size stream)
  (let ((item (gensym "NEXT"))
        (prev (loop repeat size collect (gensym "PREV"))))
    `(z:collect-first
      (z:choose
       (z:mapping
        (((,@prev ,item) (z:chunk ,(1+ size) 1 (z:scan-stream ,stream))))
        (unless (sum-of-two ,item ,@prev) ,item))))))

(defun part-1 ()
  (with-input (s 9)
    (part-1-expand 26 s)))

(defvar *part-1* 393911906)

(defun part-2-input (&aux (max *part-1*))
  (with-input (in 9)
    (z:collect '(vector fixnum)
      ;; only the numbers below max because the sequence of positive numbers
      ;; is strictly increasing
      (z:until-if (lambda (v) (> v max))
                  (z:scan-stream in)))))

(defun part-2 (&aux (all (part-2-input)) (max *part-1*))
  (let ((window (make-window all :size 3)))
    (flet ((+/early-stop (x y &aux (s (+ x y)))
             "Sum, but stop early if current sum goes above MAX"
             (prog1 s
               (when (> s max)
                 (throw :stop nil)))))
      (loop
        for size from 3 below (length all)
        do (loop
             for offset from 0 below (- (length all) size)
             do (adjust-window window :size size :offset offset)
                (catch :stop
                  (when (= (reduce #'+/early-stop window) max)
                    (return-from part-2
                      (+ (reduce #'min window)
                         (reduce #'max window))))))))))


(define-test test
  (with-input-from-string (in "35 20 15 25 47 40 62 55 65 95 102 117 150 182 127 219 299 277 309 576")
    (assert (= 127 (part-1-expand 5 in))))
  (assert (= *part-1* (part-1)))
  (assert (=  59341885 (part-2))))
