;;(declaim (optimize (speed 3) (safety 0) (debug 0)))

(defun distance (a b)
	(+ (abs (- (pos-x a) (pos-x b)))
	   (abs (- (pos-y a) (pos-y b)))))

;; TODO: some normalization here
;;(defun control-points-estimation (state)
;;    (let* ((points      (gs-control-points state))
;;           (next-point  (car points))
;;           (rob-pos     (gs-robot-pos state)))
;;        (if (null next-point)
;;            (let ((max-score (* *total-lambdas* (+ *lift-exit-bonus* *lambda-cost*))))
;;                (- max-score (gs-cur-score state)))
;;            (if (equal rob-pos next-point)
;;                (progn
;;                    (pop (gs-control-points state))
;;                    (control-points-estimation state))
;;                (distance rob-pos next-point)))))

(defmacro aif (cond true false)
    `(let ((it ,cond))
        ,true
        ,false))

(defvar *very-big-cost* 9999999)

(defun distance-from-path (pos path)
    (car (sort (mapcar (lambda (p) (distance pos p)) path ) #'<)))

(defun distance-penalty (state path)
    (* 2 (distance-from-path (gs-robot-pos state) path)))

(defvar *border-iters-num* 10000)

(defun nearest-lambda-iteration (state lambdas)
	(format t "nearest-lambda-iteration on state: ~A" state)
    (if lambdas
        (let* ((wave (routes-vector-from-map (gs-field state) (gs-robot-pos state)))
               (sorted-lambdas (sort lambdas #'< :key (lambda (pos) (aif (wave-cost wave (pos-x pos) (pos-y pos)) it *very-big-cost*)))))
          (some (lambda (target)
                    ;; todo: check that lambda still exists
                    (let* ((rest-of-lambdas (remove target lambdas))
                           (path (wave-path wave target))
                           (iters-num 0)
                           (target-state
                             (do-search
                                state
                                :termination-fn (lambda (s) (or (equalp (gs-robot-pos s) target)
                                                                (> iters-num *border-iters-num*)))
                                :estimation-fn (lambda (s visited-pos)
                                                 (declare (ignore visited-pos))
                                                 (incf iters-num)
                                                 (+ (max-possible-estimation s nil)
                                                    (distance-penalty s path)))
                                :continuations-fn #'produce-continuations ;;TODO remove states too far from path here
                               )))
                        (when target-state
                            (if rest-of-lambdas
                                (nearest-lambda-iteration target-state rest-of-lambdas)
                                target-state))))
                sorted-lambdas))
        state))

(defun get-lambda-positions (state)
	(let* ((f (gs-field state))
		   (w (map-width f))
		   (h (map-height f))
		   (lambdas nil))
		(loop for x from 0 to (1- w) do
			(loop for y from 0 to (1- h) do
				(when (eq (at-pos f x y) 'lambda)
					(push (make-pos :x x :y y) lambdas))))
		lambdas))

(defun play-with-nearest-lambdas (state)
	(let ((final-state (nearest-lambda-iteration state (get-lambda-positions state))))
		(if final-state
			(format t "Final path: ~A~%Final state: ~A~%" (make-path-string final-state) final-state)
			(format t "SHI~%"))))
