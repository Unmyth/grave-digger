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

PACKAGE_FILES=install PACKAGES-TESTING src/*.lisp src/*.system README makefile
PACKAGE_CMD=tar -czf icfp-95466816.tgz 

src-package:
	$(PACKAGE_CMD) $(PACKAGE_FILES)

package: lifter
	$(PACKAGE_CMD) $(PACKAGE_FILES) lifter

.PHONY: lifter lifter-interactive lifter-n clean package

