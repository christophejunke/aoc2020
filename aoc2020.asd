(defsystem :aoc2020
  :depends-on (:aoc2020.requirements)
  :serial nil
  :components ((:file "utils")
               (:file "fetch")
               (:module #:GRIDS
                :depends-on ()
                :pathname "grids"
                :serial t
                :components ((:file "package")
                             (:file "utils")
                             (:file "macros")
                             (:file "api")))
               (:file "package" :depends-on ("utils" "fetch" #:grids))
               (:module #:PARSING
                :depends-on ("package")
                :pathname ""
                :serial t
                :components ((:file "lexer")
                             (:file "scanner")))

               (:file "inputs" :depends-on ("package" "setup"))
               (:file "tests" :depends-on ("package"))
               (:file "setup" :depends-on ("package" "tests"))
               (:module #:DAYS
                :depends-on ("setup" "inputs" #:parsing)
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
