(defvar *lambda-cost* 25)
(defvar *lift-exit-bonus* 50)
(defvar *abort-exit-bonus* 25)

;;(declaim (optimize (speed 3) (safety 0) (debug 0)))

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

(defun is-winning-state? (state) (case (gs-state state) (win t)))
(defun is-terminal-state? (state) (case (gs-state state) ((aborted win lost) t)))

(defun simple-termination-fn (state)
    (when (and (is-terminal-state? state)
               (or (null *best-state*)
                   (> (gs-cur-score state) (gs-cur-score *best-state*))))
        (setf *best-state* state))
    (is-winning-state? state))

(defun produce-continuations (state)
    (unless (is-terminal-state? state) ;; states with 'a in the end do not produce continuations
        (mapcar (lambda (command) (update-game-state state command))
                '(l r u d w a))))

(defun distance (a b)
	(+ (abs (- (pos-x a) (pos-x b)))
	   (abs (- (pos-y a) (pos-y b)))))

;; TODO: some normalization here
(defun control-points-estimation (state)
    (let* ((points      (gs-control-points state))
           (next-point  (car points))
           (rob-pos     (gs-robot-pos state)))
        (if (null next-point)
            (let ((max-score (* *total-lambdas* (+ *lift-exit-bonus* *lambda-cost*))))
                (- max-score (gs-cur-score state)))
            (if (equal rob-pos next-point)
                (progn
                    (pop (gs-control-points state))
                    (control-points-estimation state))
                (distance rob-pos next-point)))))

(defun play-a-game (initial-state)
    (do-search initial-state :termination-fn #'simple-termination-fn
                             :continuations-fn #'produce-continuations
                             :estimation-fn #'max-possible-estimation)
    (if *best-state*
        (format t "~A~%final state is:~A~%Iters num is: ~A~%"
                    (make-path-string *best-state*)
                    *best-state*
                    *iters-count*)
        (format t "A~%"))) 
