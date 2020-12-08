all: quicksbcl test-from-scratch

quicksbcl: Makefile aoc2020.requirements.asd
	@sbcl \
	--noinform \
	--no-userinit \
	--eval '(load #p"~/quicklisp/setup.lisp")' \
	--eval '(ql:register-local-projects)' \
	--eval '(ql:quickload :aoc2020.requirements)' \
	--eval '(sb-ext:save-lisp-and-die "quicksbcl" :executable t)'

test-from-scratch :
	@./quicksbcl \
	--noinform \
	--eval '(ql:quickload :aoc2020 :silent t)' \
	--eval '(aoc2020:test-all)' \
	--quit
