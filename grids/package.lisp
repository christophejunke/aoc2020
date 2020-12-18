(defpackage :aoc2020.grids
  (:use :cl :alexandria)
  (:export
   ;; LISP ARRAY TRAVERSAL
   #:do-array

   ;; INFINITE GRIDS
   #:make-infinite-grid
   #:copy-infinite-grid
   #:fold-infinite-grid
   #:map-into-infinite-grid
   #:iref))
