default: lifter

lifter lifter-interactive:
	sbcl --load src/compile.lisp

clean:
	touch src/*.lisp
	rm -rf lifter lifter-interactive

.PHONY: lifter lifter-interactive clean

