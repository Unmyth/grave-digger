(require 'asdf)
;;(proclaim '(optimize (speed 3) (safety 0) (debug 0)))
(load "src/digger.system")
(asdf:operate 'asdf:compile-op :digger)
(sb-ext:save-lisp-and-die "lifter" :executable t :toplevel (quote main-game) :purify t)

