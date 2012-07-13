default: lifter

lifter:
	sbcl --non-interactive	\
		 --eval "(progn (load \"src/digger.system\") (asdf:operate 'asdf:compile-op :digger) \
						(sb-ext:save-lisp-and-die \"lifter\" :executable t :toplevel (quote main) :purify t))"

clean:
	rm -rf lifter

.PHONY: lifter clean


