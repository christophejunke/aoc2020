(defpackage :aoc2020.00 (:use :aoc2020))
(in-package :aoc2020.00)

;; testing basic input
(do-input-lines (line "00-test")
  (assert (string= line "test")))

;; testing fold-input-lines
(labels ((direction (string)
           (aref #(#c(0 -1) #c(1 0) #C(0 1) #C(-1 0))
                 (position (char string 0) "NESW")))
         (fold-line (line position)
           (register-groups-bind ((#'direction direction)
                                  (#'parse-integer steps))
               ('(:sequence letter int) line)
             (+ position (* direction steps)))))
  (assert (= (fold-input-lines "00-fold" #'fold-line 0)
             #C(30 20))))
