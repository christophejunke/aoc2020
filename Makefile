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

test-from-scratch :
	@./quicksbcl \
	--noinform \
	--no-userinit \
	--eval '(ql:quickload :aoc2020 :silent t)' \
	--eval '(or (aoc2020:test-all) (sb-ext:quit :unix-status -1))' \
	--quit

# NON-PORTABLE, this is mostly to have an example of how to run it
#
# inside the toplevel, you can query:
#
#    [eclipse 1]: findall((C,D), candidate(C, D), L).
#
# this should return all C,D pairs for which candidate(C,D) holds.
#
# By loading library FD (finite domains), you can manipulate
# constraints where the domain is a set of finite values.
#
#    [eclipse 2]: lib(fd).
#    [eclipse 3]: V :: [a,b,c], W :: [b,c,d], V = W.
#
#    V = V{[b, c]}
#    W = V{[b, c]}
#    Yes (0.00s cpu)
#
prolog :
	~/bin/eclipse/bin/x86_64_linux/eclipse -b days/d16.pl
