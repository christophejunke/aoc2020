(in-package :aoc2020)

(unless *kernel*
  (setf *kernel*
        (make-kernel (length (aoc-packages))
                     :name "aoc2020"
                     :bindings nil)))
