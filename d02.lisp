(defpackage :aoc2020.02
  (:use :aoc2020)
  (:export #:part-1
           #:part-2))

(in-package :aoc2020.02)

(defun first-char (string)
  (char string 0))

(defun parse-entry (line)
  (register-groups-bind ((#'parse-integer v1 v2) (#'first-char letter) password)
      ('(:sequence int "-" int " " letter ": " word) line :sharedp t)
    (values v1 v2 letter password)))

(defun part-1 ()
  (fold-input-lines 02
                    (lambda (line count)
                      (multiple-value-bind (min max c p) (parse-entry line)
                        (if (<= min (count c p) max)
                            (1+ count)
                            count)))
                    0))

(defun part-2 (&aux (counter 0))
  (do-input-lines (line 02 counter)
    (multiple-value-bind (pos1 pos2 c p) (parse-entry line)
      (flet ((match (u) (char= c (char p (1- u)))))
        (when (xor (match pos1) (match pos2))
          (incf counter))))))
