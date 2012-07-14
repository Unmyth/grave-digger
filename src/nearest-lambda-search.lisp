(defun distance (a b)
	(+ (abs (- (pos-x a) (pos-x b)))
	   (abs (- (pos-y a) (pos-y b)))))

(defun find-path (state pos)
	(do-search state :termination-fn (lambda (s) (equalp (gs-robot-pos s) pos))
					 :continuations-fn #'produce-continuations
					 :estimation-fn (lambda (s) (distance (gs-robot-pos s) pos))))

(defun find-nearest-lambda (state lambdas)
	(let ((rp (gs-robot-pos state))
		  (nearest-dist nil)
		  (nearest nil))
		(dolist (p lambdas)
			(let ((p-dist (distance p rp)))
				(when (or (null nearest-dist)
					  	  (< p-dist nearest-dist))
					(setf nearest-dist p-dist
						  nearest p))))
		nearest))

(defun nearest-lambda-iteration (state lambdas)
	(format t "Iteration on state: ~A" state)
	(if lambdas
		(let* ((nearest (find-nearest-lambda state lambdas))
			   (rest    (remove nearest lambdas)))
			(if (eq (at-pos (gs-field state) (pos-x nearest) (pos-y nearest)) 'lambda)
				(let ((new-state (find-path state nearest)))
					(if new-state
						(nearest-lambda-iteration new-state rest)
						(nearest-lambda-iteration state rest)))
				(nearest-lambda-iteration state rest)))
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
