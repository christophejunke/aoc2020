(defpackage :aoc2020
  (:use . #1=(:cl :alexandria :ppcre :trivia
              :named-readtables :esrap :lparallel
                  :aoc2020.fetch :aoc2020.utils))
  (:export #:fold-input-lines
           #:do-input-lines
           #:with-input
           #:slurp-line
           #:map-line-chunks
           #:map-input
           #:int
           #:word
           #:letter
           #:decode-format
           #:scanner-bind
           #:scan-as-values
           #:scan-as-list
           #:read-grid
           #:map-grid-into
           #:fold-grid
           #:*neighbour-offsets*
           #:map-neighbours
           #:define-test
           #:test-all
           .
           #.(aoc2020.utils:external-symbols . #1#)))
