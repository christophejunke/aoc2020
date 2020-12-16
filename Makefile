all: quicksbcl test-from-scratch

# precompile all dependencies and save the resulting image
quicksbcl: Makefile aoc2020.requirements.asd
	@sbcl \
	--noinform \
	--no-userinit \
	--eval '(load #p"~/quicklisp/setup.lisp")' \
	--eval '(ql:register-local-projects)' \
	--eval '(ql:quickload :aoc2020.requirements)' \
	--eval "(sb-ext:save-lisp-and-die \"$@\" :executable t)"

test-from-scratch : quicksbcl
	@./quicksbcl \
	--noinform \
	--no-userinit \
	--non-interactive \
	--eval '(ql:quickload :aoc2020 :silent t)' \
	--eval '(or (aoc2020:test-all) (sb-ext:quit :unix-status -1))' \
	--quit

# recompile verbosely for warnings
compile: quicksbcl
	@./quicksbcl \
	--noinform \
	--no-userinit \
	--non-interactive \
	--load "compile.lisp" \
	--quit
