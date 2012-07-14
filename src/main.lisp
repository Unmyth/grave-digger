(defun main ()
    (let ((gs (multiple-value-bind (field rob-pos)
                (map-from-stdio)
              (make-game-state 
                :field field
                :robot-pos rob-pos
                :cur-score 0
                :cur-lambdas 0
                :state 'in-progress))))
      (format t "~A~%Score: ~A~%" (map-to-string (gs-field gs)) 0)
      (read-command gs)))
