;;(declaim (optimize (speed 3) (safety 0) (debug 0)))

(defun game-state-hash (gs)
    (mod 
     (+ (tree-map-hash (gs-field gs))
        (* (gs-water-level gs) 2191)
        (* (gs-flooding-counter gs) 123)
        (* (gs-cur-waterproof gs) 7)
        (* (gs-cur-razors gs) 1231)
        (if (= 0 (gs-num-beards gs))
            0
            (* (gs-cur-growth gs)
               6567)))
     +big-prime+))

(defun game-state-eq (a b)
    (and (tree-map-equals (gs-field a) (gs-field b))
         (= (gs-water-level a) (gs-water-level b))
         (= (gs-flooding-counter a) (gs-flooding-counter b))
         (= (gs-cur-waterproof a) (gs-cur-waterproof b))
         (= (gs-cur-razors a) (gs-cur-razors b))
         (if (and (= 0 (gs-num-beards a))
                  (= 0 (gs-num-beards b)))
            t
            (= (gs-cur-growth a)
               (gs-cur-growth b)))))

(defvar *iters-count* 0)

(defun make-closed-state-table ()
  ;;(list 
   (make-generic-map #'game-state-hash #'game-state-eq)
  ;; (make-hash-table :test #'equalp))
  )

(defun add-closed-state (tab state)
  (generic-map-add tab state)
;;  (if (gethash (gs-robot-pos state) (second tab))
;;      (incf (gethash (gs-robot-pos state) (second tab)))
;;      (setf (gethash (gs-robot-pos state) (second tab)) 1))
  )

(defun is-closed-state (tab state)
  ;;(or 
   (generic-map-get tab state)
;;      (and (gethash (gs-robot-pos state) (second tab))
;;           (>= (gethash (gs-robot-pos state) (second tab)) ))
;;      )
)

(defun do-search (state ;; game-state
                  &key termination-fn
                       estimation-fn
                       continuations-fn)
    (let ((closed-states     (make-closed-state-table))
          (open-states       (create-heap (lambda (a b) (< (gs-estimation a) (gs-estimation b)))))
          (visited-positions (make-hash-table :test #'equalp)))
        
        ;; initiate the loop 
        (setf (gs-estimation state) (funcall estimation-fn state visited-positions))
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
                (if (or (funcall termination-fn current)
                        *got-timeout-signal*)
                    (return-from do-search current)
                    (progn 
                      (add-closed-state closed-states current)
                      (if (gethash (gs-robot-pos current) visited-positions)
                          (incf (gethash (gs-robot-pos current) visited-positions))
                          (setf (gethash (gs-robot-pos current) visited-positions) 1))))

                (when (= (mod *iters-count* 500) 0)
                     (format t "On iteration ~A, state is ~A~%" *iters-count* current))
                

                ;; check continuations
                (dolist (new-state (funcall continuations-fn current))
                    (unless (is-closed-state closed-states new-state)
                        (setf (gs-estimation new-state) (funcall estimation-fn new-state visited-positions))
                        ;;(format t "closed state size: ~A adding new state: ~A~%"
                        ;;          (generic-map-size closed-states) new-state)
                        (heap-insert open-states new-state)))))))
