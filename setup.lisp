(in-package :aoc2020)

(when-let ((package (find-package :series)))
  (rename-package package "Z"))

(unless *kernel*
  (setf *kernel* (make-kernel (length (aoc-packages))
                              :name "aoc2020"
                              :bindings nil)))
