(defpackage :aoc2020
  (:use . #1=(:cl :alexandria :ppcre :trivia
              :named-readtables :aoc2020.fetch))
  (:export #:fold-input-lines
           #:do-input-lines
           #:with-input
           #:slurp-line
           #:int
           #:word
           #:letter
           #:decode-format
           #:scanner-bind
           #:test-all
           .
           #.(aoc2020.utils:external-symbols . #1#)))
