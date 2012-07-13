(load "generic-map.lisp")
;;(defstruct open-state
;;    map
;;    path
;;    robot-x
;;    robot-y)
;;
;;(defun search-path-to (state x y)
;;    (let ((closed-states '())
;;          (open-states (list (make-open-state :state state :path '()))))
;;        ;;
;;        (when (not (heap-empty-p open-states))
;;            (let ((current (heap-remove open-states)))
;;                (if (and (eq (os-robot-x current) x)
;;                         (eq (os-robot-y current) y))
;;                    (values (os-path current) (os-map current))
;;                    (maps-hash-add closed-states (os-map current))
;;                    (dolist (move (possible-moves current))
;;                        (let ((new-state (make-open-state :state (do-move (os-map current) move)
;;                                                          :path (cons move (os-path current))
;;                                                          :robot-x (move-x move)
;;                                                          :robot-y (move-y move))))
;;                        (when (not (maps-set-contains (os-map new-state) closed-states))
;;                            (when (heap-find-idx 
;;                            (let ((estimated-score (+ (length (os-path new-state))
;;                                                      (distance move (make-move x y)))))
;;                                (
;;                        

;; open set: map + path + robot pos + heurisitc estim
;; closed set: set of maps

;; search process:
;;
;; while open-set:
;;    cur = top estim open-set
;;    
;;    when cur robot-pos == target robot-pos -> return current path
;;
;;    add cur to closed-set
;;    for each move in (possible-moves cur):
;;      new-state = (do-move cur move) <- updates map, path, robot pos and estim
;;      ;;
;;
