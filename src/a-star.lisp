(defstruct pos x y)
(defstruct (game-state (:conc-name gs-))
    field
    robot-pos
    path
    estimation)

(defun game-state-hash (gs)
    (tree-map-hash (gs-field gs)))

(defun game-state-eq (a b)
    (tree-map-equals a b))

(defun estimate-cost (gs pos)
    )

(defun possible-moves (gs)
    )

(defun do-move (gs move) ;; move 'left 'right 'up 'down 'wait
    )

(defun search-path-to (state ;; game-state
                       destination) ;; pos
    (let ((closed-states     (make-generic-map #'game-state-hash #'game-state-eq))
          (open-states       (create-heap (lambda (a b) (< (gs-estimation a) (gs-estimation b))))))
          ;;(best-known-scores (make-generic-map #'game-state-hash #'game-state-eq)))
    
        (setf (gs-estimation state) (estimate-cost state destination))
        (heap-insert open-states state)
        (loop
            ;; failure
            (when (heap-empty-p open-states)
                (return-from search-path-to nil))

            (let ((current (heap-remove open-states)))
                ;; check goal
                (if (equal (gs-robot-pos current) destination)
                    (return-from search-path-to current)
                    (generic-map-add closed-states current))

                ;; check continuations
                (dolist (move (possible-moves current))
                    (let ((new-state (do-move current move)))
                        (unless (generic-map-get closed-states new-state)
                            (heap-insert open-states new-state))))))))
