default: lifter

lifter:
	sbcl --non-interactive	--load src/compile.lisp

clean:
	touch src/*.lisp
	rm -rf lifter

.PHONY: lifter clean

