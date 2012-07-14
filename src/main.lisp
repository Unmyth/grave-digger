(defun read-initial-state ()
    (multiple-value-bind (field rob-pos) (map-from-stdio)
        (make-game-state 
            :field field
            :robot-pos rob-pos)))


(defun main-manual ()
    (let ((gs (read-initial-state)))
      (format t "~A~%Score: ~A~%" (map-to-string (gs-field gs)) 0)
      (read-command gs)))

(defun main-game () (play-a-game (read-initial-state)))
(defun main-nearest () (play-with-nearest-lambdas (read-initial-state)))
