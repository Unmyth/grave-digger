default: lifter

lifter : src/heap.fasl
	sbcl --eval '(progn (load "src/heap.fasl") (sb-ext:save-lisp-and-die "lifter" :executable t :toplevel (quote main) :purify t))'

src/heap.fasl: src/heap.lisp
	sbcl --eval '(progn (compile-file "src/heap.lisp") (quit))'

clean:
	rm -f src/*.fasl

.PHONY: clean default


