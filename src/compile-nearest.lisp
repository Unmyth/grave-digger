(require 'asdf)
(load "src/digger.system")
(asdf:operate 'asdf:compile-op :digger)
(sb-ext:save-lisp-and-die "lifter-n" :executable t :toplevel (quote main-nearest) :purify t)

