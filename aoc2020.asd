(defsystem "aoc2020"
  :depends-on ("alexandria"
               "cl-ppcre"
               "trivia"
               "trivia.quasiquote")
  :serial t
  :components ((:file "utils")
               (:file "common")
               (:file "d00")))
