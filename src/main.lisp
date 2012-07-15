(declaim (special *map-metadata*))

(defmacro read-meta-value (var sym)
  (let ((val (gensym)))
    `(let ((,val (assoc ',sym *map-metadata*)))
       (when ,val
         (setf ,var (second ,val))))))

(defun interpret-metadata ()
  (read-meta-value *map-water-level* water)
  (read-meta-value *map-flooding* flooding)
  (read-meta-value *map-waterproof* waterproof))

(defun read-initial-state ()
    (multiple-value-bind (field rob-pos) (map-from-stdio)
      (interpret-metadata)
      (make-game-state 
       :field field
       :need-to-be-updated (init-need-to-be-updated field)
       :robot-pos rob-pos

       :water-level (+ *map-water-level* 1)
       :flooding-counter *map-flooding*
       :cur-waterproof *map-waterproof*)))


(defun main-manual ()
    (let ((gs (read-initial-state)))
      (format t "~A~%Score: ~A~%" (map-to-string (gs-field gs)) 0)
      (read-command gs)))

(defun main-game () (play-a-game (read-initial-state)))
(defun main-nearest () (play-with-nearest-lambdas (read-initial-state)))
