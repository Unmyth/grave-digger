(require 'asdf)
(load "src/digger.system")
(asdf:operate 'asdf:compile-op :digger)
(sb-ext:save-lisp-and-die "lifter-interactive" :executable t :toplevel (quote main-manual) :purify t)
