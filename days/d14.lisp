(defpackage :aoc2020.14
  (:use :aoc2020)
  (:export #:test))

(in-package :aoc2020.14)

(in-readtable :fare-quasiquote)

(defun parse-mask (line)
  (scanner-bind ("mask = %36s" mask) line
    (flet ((bits (c-true)
             (flet ((b (c) (if (char= c c-true) #\1 #\0)))
               (map 'string #'b mask))))
      `(mask
        ,(parse-integer (bits #\0) :radix 2)
        ,(parse-integer (bits #\1) :radix 2)
        ,(loop
           :with last := (1- (length mask))
           :for i :upto last
           :when (char= (char mask (- last i)) #\X)
           :collect i)))))

(defun parse-assignment (line)
  (scanner-bind ("mem[%d] = %d" address value) line
    `(set ,address ,value)))

(defun solve (in f)
  (with-input (stream in)
    (flet ((decode (line)
             (or (parse-mask line)
                 (parse-assignment line)
                 (error "bad input: ~s" line))))
      (let ((mem (make-hash-table)))
        (do-input-lines (line in (fold-hash-values mem #'+ 0))
          (funcall f mem (decode line)))))))

(defun map-floating-bits (f v indices)
  (if indices
      (let ((i (pop indices)))
        (map-floating-bits f (dpb 0 (byte 1 i) v) indices)
        (map-floating-bits f (dpb 1 (byte 1 i) v) indices))
      (funcall f v)))

(defun part-1 (input)
  (let ((mask-1 0) (mask-0 (mask-field (byte 36 0) -1)))
    (solve input
           (lambda (mem command)
             (ematch command
               (`(mask ,m0 ,m1 ,_)
                 (setf mask-0 m0
                       mask-1 m1))
               (`(set ,a ,v)
                 (setf (gethash a mem)
                       (logior mask-1
                               (logandc2 v mask-0)))))))))

(defun part-2 (input)
  (let ((mask-1 0) (mask-x nil))
    (solve input
           (lambda (mem command)
             (ematch command
               (`(mask ,_ ,m1 ,mx)
                 (setf mask-1 m1
                       mask-x mx))
               (`(set ,a ,v)
                 (let ((base-address (logior mask-1 a)))
                   (flet ((put-v-at (a) (setf (gethash a mem) v)))
                     (map-floating-bits #'put-v-at
                                        base-address
                                        mask-x)))))))))

(define-test test
  (assert (= 165 (part-1 "14-1")))
  (assert (= 208 (part-2 "14-2")))
  (assert (= 17028179706934 (part-1 14)))
  (assert (= 3683236147222 (part-2 14))))
