(defpackage :aoc2020.14
  (:use :aoc2020)
  (:export #:test))

(in-package :aoc2020.14)

(in-readtable :fare-quasiquote)

(defun mask (string)
  (flet ((b (tc) (map 'string (lambda (c) (if (char= c tc) #\1 #\0)) string)))
    `(mask
      ,(parse-integer (b #\0) :radix 2)
      ,(parse-integer (b #\1) :radix 2)
      ,(parse-integer (b #\X) :radix 2))))

(defun parse-mask (line)
  (scanner-bind ("mask = %36s" b) line
    (mask b)))

(defun parse-assignment (line)
  `(set ,@(scan-as-list "mem[%d] = %d" line)))

(defun input (in)
  (with-input (stream in)
    (flet ((decode (line) (or (parse-mask line) (parse-assignment line))))
      (map-input in :type 'list :transform #'decode))))

(defun part-1 (&optional (in 14))
  (loop
    with mem = (make-hash-table)
    and m0 = (dpb -1 (byte 36 0) 0)
    and m1 = 0
    for cmd in (input in)
    do (ematch cmd
         (`(mask ,i0 ,i1 ,_)
           (setf m0 i0 m1 i1))
         (`(set ,a ,v)
           (setf (gethash a mem)
                 (logior m1 (logandc2 v m0)))))
    finally
       (return
         (reduce #'+ (hash-table-values mem)))))

(defun map-floating-bits (f v indices)
  (if indices
      (let ((i (pop indices)))
        (map-floating-bits f (dpb 0 (byte 1 i) v) indices)
        (map-floating-bits f (dpb 1 (byte 1 i) v) indices))
      (funcall f v)))

(defun part-2 (&optional (in 14))
  (loop
    with mem = (make-hash-table)
    with mask-1 = 0 and mask-x = nil
    for cmd in (input in)
    do (ematch cmd
         (`(mask ,_ ,i1 ,ix)
           (setf mask-1 i1)
           (setf mask-x
                 (loop
                   for index below (integer-length ix)
                   when (logbitp index ix) collect index)))
         (`(set ,a ,v)
           (let ((address (logior mask-1 a)))
             (map-floating-bits (lambda (a) (setf (gethash a mem) v))
                                address
                                mask-x))))
    finally (return (reduce #'+ (hash-table-values mem)))))

(define-test test
  (assert (= 165 (part-1 "14-1")))
  (assert (= 208  (part-2 "14-2")))
  (assert (= 17028179706934 (part-1)))
  (assert (= 3683236147222 (part-2))))
