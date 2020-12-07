(defsystem :aoc2020
  :depends-on (:aoc2020.requirements)
  :serial nil
  :components ((:file "utils")
               (:file "fetch")
               (:file "package")
               (:file "setup" :depends-on ("utils" "fetch" "package"))
               (:file "inputs" :depends-on ("setup"))
               (:file "lexer" :depends-on ("setup"))
               (:file "scanner" :depends-on ("lexer"))
               (:file "tests" :depends-on ("scanner"))
               (:file "grids" :depends-on ("package"))
               (:module #:DAYS
                :depends-on ("tests" "inputs")
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
