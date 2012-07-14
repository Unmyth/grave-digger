(defun game-state-hash (gs)
    (tree-map-hash (gs-field gs)))

(defun game-state-eq (a b)
    (tree-map-equals (gs-field a) (gs-field b)))

(defun do-search (state ;; game-state
                  &key termination-fn
                       estimation-fn
                       continuations-fn)
    (let ((closed-states     (make-generic-map #'game-state-hash #'game-state-eq))
          (open-states       (create-heap (lambda (a b) (< (gs-estimation a) (gs-estimation b))))))
        
        ;; initiate the loop 
        (setf (gs-estimation state) (funcall estimation-fn state))
        (heap-insert open-states state)

        ;; main loop
        (loop
            ;; failure
            (when (heap-empty-p open-states)
                (return-from do-search nil))

            (let ((current (heap-remove open-states)))
                ;; check goal
                (if (funcall termination-fn current)
                    (return-from do-search current)
                    (generic-map-add closed-states current))

                ;; check continuations
                (dolist (new-state (funcall continuations-fn current))
                    (unless (generic-map-get closed-states new-state)
                        (heap-insert open-states new-state)))))))
