default: lifter

lifter lifter-interactive lifter-n:
	sbcl --non-interactive --load src/compile.lisp
	sbcl --non-interactive --load src/compile-game.lisp
	sbcl --non-interactive --load src/compile-nearest.lisp

clean:
	touch src/*.lisp
	rm -rf lifter lifter-interactive lifter-n

.PHONY: lifter lifter-interactive lifter-n clean

