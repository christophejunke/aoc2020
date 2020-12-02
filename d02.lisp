(defpackage :aoc2020.02
  (:use :aoc2020)
  (:export #:part-1
           #:part-2
           #:solve))

(in-package :aoc2020.02)

(defun parse-entry (line)
      ('(:sequence int "-" int " " letter ": " word) line :sharedp t)
  (register-groups-bind ((#'parse-integer v1 v2) (#'first-elt letter) password)
    (values v1 v2 letter password)))

;; functional

(defun part-1 ()
  (fold-input-lines 02
                    (lambda (line count)
                      (multiple-value-bind (min max c p) (parse-entry line)
                        (if (<= min (count c p) max)
                            (1+ count)
                            count)))
                    0))

;; imperative

(defun part-2 (&aux (counter 0))
  (do-input-lines (line 02 counter)
    (multiple-value-bind (pos1 pos2 c p) (parse-entry line)
      (flet ((match (u) (char= c (char p (1- u)))))
        (when (xor (match pos1) (match pos2))
          (incf counter))))))

;; with SERIES package

(defun solve ()
  (flet ((is (u) (if u 1 0))
         (at (c p u) (char= c (char p (1- u)))))
    (declare (inline is at))
    ;; open input for puzzle 2
    (with-input (stream 2)
      ;; fold a serie with init/step functions
      (z:collect-fn
       ;; return two integers: one counter for each part of the puzzle
       '(values integer integer)
       ;; init function: counters are zero
       (lambda () (values 0 0))
       ;; step function, compute next counter from old counters, for each line
       (lambda (c1 c2 line)
         (multiple-value-bind (v1 v2 c p) (parse-entry line)
           (values (+ c1 (is (<= v1 (count c p) v2)))
                   (+ c2 (is (xor (at c p v1)
                                  (at c p v2)))))))
       ;; scan lines from the stream
       (z:scan-stream stream #'read-line)))))
