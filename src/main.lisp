(defun read-initial-state ()
    (multiple-value-bind (field rob-pos) (map-from-stdio)
        (make-game-state 
            :field field
            :need-to-be-updated (init-need-to-be-updated field)
            :robot-pos rob-pos)))


(defun main-manual ()
    (let ((gs (read-initial-state)))
      (format t "~A~%Score: ~A~%" (map-to-string (gs-field gs)) 0)
      (read-command gs)))

(defun main-game () (play-a-game (read-initial-state)))
(defun main-nearest () (play-with-nearest-lambdas (read-initial-state)))
;;(defun main-nearest () ;;(play-with-nearest-lambdas (read-initial-state)))
;;    (let ((h (create-heap #'<)))
;;        (labels ((-> (el) (heap-insert h el)
;;                          (format t "Heap after ~A insert: ~A~%" el h)))
;;            (-> 10)
;;            (-> 20)
;;            (-> 30)
;;            (loop while (not (heap-empty-p h)) do
;;                (let ((top (heap-remove h)))
;;                    (format t "Heap after removal of ~A: ~A~%" top h))))))
