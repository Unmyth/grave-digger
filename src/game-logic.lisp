(defvar *lambda-cost* 25)
(defvar *lift-exit-bonus* 50)
(defvar *abort-exit-bonus* 25)

;;(defvar *got-timeout-signal* nil)

;;(require :sb-posix)
;;(require :sb-sys)
;;
;;(defun install-handler ()
;;    (sb-sys:enable-interrupt
;;        sb-posix:sigint
;;        (lambda (signo context info)
;;            (declare (ignore signo context info))
;;            (setf *got-timeout-signal* t))))
                

(defun max-possible-estimation (game-state)
    (let ((max-score (* *total-lambdas* (+ *lift-exit-bonus* *lambda-cost*))))
        (- max-score (gs-cur-score game-state))))

(defvar *best-state* nil)

(defun is-terminal-state? (state) (case (gs-state state) (('aborted 'win) t)))

(defun simple-termination-fn (state)
    (when (and (is-terminal-state? state)
               (> (gs-cur-score state) (gs-cur-score *best-state*)))
        (setf *best-state* state))
    nil)

(defun produce-continuations (state)
    (unless (is-terminal-state? state) ;; states with 'a in the end do not produce continuations
        (mapcar (lambda (command) (update-game-state state command))
                '(l r u d w a))))

(defun play-a-game ()
    (let ((initial-state (multiple-value-bind (field rob-pos) (map-from-stdio)
                            (make-game-state 
                                :field field
                                :robot-pos rob-pos))))
        (do-search initial-state :termination-fn #'simple-termination-fn
                                 :continuations-fn #'produce-continuations
                                 :estimation-fn #'max-possible-estimation)
        (if *best-state*
            (format t "~A" (gs-path *best-state*))
            (format t "A")))) 
