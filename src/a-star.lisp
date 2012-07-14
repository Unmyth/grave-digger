(defun game-state-hash (gs)
    (tree-map-hash (gs-field gs)))

(defun game-state-eq (a b)
    (tree-map-equals (gs-field a) (gs-field b)))

(defvar *iters-count* 0)

(defun do-search (state ;; game-state
                  &key termination-fn
                       estimation-fn
                       continuations-fn)
    (let ((closed-states     (make-generic-map #'game-state-hash #'game-state-eq))
          (open-states       (create-heap (lambda (a b) (< (gs-estimation a) (gs-estimation b))))))
        
        ;; initiate the loop 
        (setf (gs-estimation state) (funcall estimation-fn state))
        (heap-insert open-states state)
        ;;(format t "added initial state: ~A~%" state)

        ;; main loop
        (loop
            (incf *iters-count*)

            ;; failure
            (when (heap-empty-p open-states)
                (return-from do-search nil))

            (let ((current (heap-remove open-states)))
                ;; check goal
                (if (funcall termination-fn current)
                    (return-from do-search current)
                    (generic-map-add closed-states current))

                (when (= (mod *iters-count* 50) 0)
                    (format t "On iteration ~A, state is ~A~%" *iters-count* current))  

                ;; check continuations
                (dolist (new-state (funcall continuations-fn current))
                    (unless (generic-map-get closed-states new-state)
                        (setf (gs-estimation new-state) (funcall estimation-fn new-state))
                        ;;(format t "closed state size: ~A adding new state: ~A~%"
                        ;;          (generic-map-size closed-states) new-state)
                        (heap-insert open-states new-state)))))))
