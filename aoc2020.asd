(defsystem "aoc2020"
  :depends-on ("alexandria"
               "cl-ppcre"
               "series"
               "trivia"
               "trivia.quasiquote")
  :serial t
  :components ((:file "utils")
               (:file "common")
               (:file "d00")))
