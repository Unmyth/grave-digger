default: lifter lifter-interactive lifter-n

lifter:
	sbcl --non-interactive --load src/compile-game.lisp

lifter-interactive:
	sbcl --non-interactive --load src/compile.lisp

lifter-n:
	sbcl --non-interactive --load src/compile-nearest.lisp

clean:
	touch src/*.lisp
	rm -rf lifter lifter-interactive lifter-n

.PHONY: lifter lifter-interactive lifter-n clean

