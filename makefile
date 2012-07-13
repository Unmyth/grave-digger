default: lifter

lifter:
	sbcl --load src/compile.lisp

clean:
	touch src/*.lisp
	rm -rf lifter

.PHONY: lifter clean

