(in-package :aoc2020)

(defpackage :z
  (:use :series)
  (:export . #.(external-symbols :series)))

(unless *kernel*
  (setf *kernel*
        (make-kernel (length (aoc-packages))
                     :name "aoc2020"
                     :bindings nil)))
