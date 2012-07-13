default: lifter

lifter : src/heap.fasl src/map.fasl src/simple-map.fasl
	sbcl --eval '(progn (load "src/heap.fasl") (load "src/map.fasl") (load "src/simple-map.fasl") (sb-ext:save-lisp-and-die "lifter" :executable t :toplevel (quote map-from-stdio) :purify t))'

src/heap.fasl: src/heap.lisp
	sbcl --eval '(progn (compile-file "src/heap.lisp") (quit))'

src/map.fasl: src/map.lisp
	sbcl --eval '(progn (compile-file "src/map.lisp") (quit))'

src/simple-map.fasl: src/simple-map.lisp
	sbcl --eval '(progn (load "src/map.fasl") (compile-file "src/simple-map.lisp") (quit))'

clean:
	rm -f src/*.fasl

.PHONY: clean default


