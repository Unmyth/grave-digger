;;(declaim (optimize (speed 3) (safety 0) (debug 0)))

(defun game-state-hash (gs)
    (mod 
     (+ (tree-map-hash (gs-field gs))
        (* (gs-water-level gs) 2191)
        (* (gs-flooding-counter gs) 123)
        (* (gs-cur-waterproof gs) 7))
     +big-prime+))

(defun game-state-eq (a b)
    (and (tree-map-equals (gs-field a) (gs-field b))
         (= (gs-water-level a) (gs-water-level b))
         (= (gs-flooding-counter a) (gs-flooding-counter b))
         (= (gs-cur-waterproof a) (gs-cur-waterproof b))))

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
