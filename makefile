default: lifter

lifter lifter-interactive lifter-n:
	sbcl --load src/compile.lisp
	sbcl --load src/compile-game.lisp
	sbcl --load src/compile-nearest.lisp

clean:
	touch src/*.lisp
	rm -rf lifter lifter-interactive lifter-n

.PHONY: lifter lifter-interactive lifter-n clean

