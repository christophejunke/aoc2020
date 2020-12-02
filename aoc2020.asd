(defsystem "aoc2020"
  :depends-on ("alexandria"
               "cl-ppcre"
               "drakma"
               "series"
               "trivia"
               "trivia.quasiquote"
               "uiop")
  :serial nil
  :components ((:file "utils")
               (:file "fetch")
               (:file "common" :depends-on ("utils" "fetch"))
               (:module #:DAYS
                :depends-on ("common")
                :pathname "days"
                :serial nil
                :components ((:file "d00")
                             (:file "d01")
                             (:file "d02")
                             (:file "d03")
                             (:file "d04")
                             (:file "d05")
                             (:file "d06")
                             (:file "d07")
                             (:file "d08")
                             (:file "d09")
                             (:file "d10")
                             (:file "d11")
                             (:file "d12")
                             (:file "d13")
                             (:file "d14")
                             (:file "d15")
                             (:file "d16")
                             (:file "d17")
                             (:file "d18")
                             (:file "d19")
                             (:file "d20")
                             (:file "d21")
                             (:file "d22")
                             (:file "d23")
                             (:file "d24")
                             (:file "d25")))))
