(defsystem "aoc2020"
  :depends-on ("alexandria" "cl-ppcre" "optima")
  :serial t
  :components ((:file "utils")
               (:file "common")
               (:file "d00")))
