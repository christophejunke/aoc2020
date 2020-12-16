(require 'asdf)

(let ((asd (merge-pathnames "aoc2020.asd" *load-pathname*)))
  (unless (probe-file asd)
    (error "cannot find asd?: ~s" asd))
  (asdf:load-asd asd)
  (asdf:compile-system :aoc2020 :verbose t :force t))

