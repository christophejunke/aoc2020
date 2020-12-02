# AdventOfCode 2020

See https://adventofcode.com/2020

Twitter thread: https://twitter.com/chrsnjk/status/1332126685819449344

# Overview

- I'll be using the Common Lisp language (next year maybe I'll try
  another one)

- Each day will be associated with a different branch (or tag, once it
  needs not move anymore), i.e. `d01`, `d02`, etc. as well as a tag
  that identifies the starting point for each day, e.g. `d02.start`,
  etc. Commits between a `dN` and the next `dN+1.start` are
  refactorings, etc. Commits between `dN.start` and `dN` are all
  associated to the puzzle for that day.

- One package per day, named `aoc2020.dd` where `dd` is the day (e.g.
  01, 02, ..). Typically puzzles have two parts, so each package is
  going to export symbols `part-1` and `part-2` as functions.

- I'll use libraries if necessary, as a way to show what can be done
  in actual programs

- I'll comment the code to explain difficulties, but some knowledge of
  Lisp is assumed

- To avoid unhealthy behaviours and because I don't have much spare
  time, and also because it's supposed to be fun, I won't be forcing
  myself to post a solution each day, I'll allow to let the challenge
  end in January if necessary.

# Install

Clone the repository in `~/quicklisp/local-projects/` and call
`(ql:quickload :aoc2020)`.

In the source directory, calling `make` will test loading the code
with `sbcl`, which is useful to guard against discrepancies between
the source code and the changes made in the live environment.
