all: test-from-scratch

test-from-scratch :
	sbcl --noinform --eval '(ql:quickload :aoc2020)' --quit

