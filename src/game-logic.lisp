(defvar *lambda-cost* 25)
(defvar *lift-exit-bonus* 50)

;;(declaim (optimize (speed 3) (safety 0) (debug 0)))

;;(require :sb-posix)
;;(require :sb-sys)
;;
;;(defun install-handler ()
;;    (sb-sys:enable-interrupt
;;        sb-posix:sigint
;;        (lambda (signo context info)
;;            (declare (ignore signo context info))
;;            (setf *got-timeout-signal* t))))
                

(defun max-possible-estimation (game-state visited-positions)
    (let ((max-score (* *total-lambdas* (+ *lift-exit-bonus* *lambda-cost*))))
      (+ (- max-score (gs-cur-score game-state)
            (* 0 (gs-moved-stones game-state)))
         (if (and visited-positions
                  (gethash (gs-robot-pos game-state) visited-positions))
             (* 0 (gethash (gs-robot-pos game-state) visited-positions))
             0))))

(defun is-winning-state? (state) (case (gs-state state) (win t)))
(defun is-terminal-state? (state) (case (gs-state state) ((aborted win lost) t)))

(defun simple-termination-fn (state)
    (when (and (is-terminal-state? state)
               (or (null *best-state*)
                   (> (gs-cur-score state) (gs-cur-score *best-state*))))
        (setf *best-state* state))
    ;;(is-winning-state? state)
    nil)

(defun produce-continuations (state)
    (unless (is-terminal-state? state) ;; states with 'a in the end do not produce continuations
        (append 
         (if (> (+ (gs-num-beards state) (gs-cur-razors state))
                0)
             (list (update-game-state state 's))
             nil)
         (mapcar (lambda (command) (update-game-state state command))
                 '(l r u d w a)))))

(defun play-a-game (initial-state)
    (do-search initial-state :termination-fn #'simple-termination-fn
                             :continuations-fn #'produce-continuations
                             :estimation-fn #'max-possible-estimation))
