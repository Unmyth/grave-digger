
;; Symbols:
;; robot, wall, rock, lambda, closed-lift, open-lift, earth, space, falling-rock

(defun is-rock (sym)
  (or (eq sym 'rock)
      (eq sym 'falling-rock)))

(defclass map ()
  ((width :accessor map-width
          :initarg :width
          :type integer)
   (height :accessor map-height
           :initarg :height
           :type integer)))

(defclass simple-map (map)
  ((vector :accessor simple-map-vector
           :initarg :vector
           :type (vector t))))

(defgeneric at-pos (map x y))

(defgeneric update (map x y value))

(defgeneric update! (map x y value))

(defun create-simple-map (width height)
  (let ((vect (make-array (list width height))))
    (make-simple-map :width width :height height :vector vect)))

(defmethod at-pos ((map simple-map) x y)
  (aref (simple-map-vector map) x y))

(defmethod update! ((map simple-map) x y value)
  (setf (aref (simple-map-vector map) x y)
        value))

(defmethod update ((map simple-map) x y value)
  (let ((new-map (make-array (list (map-width map) (map-height map))
                             :initial-contents (simple-map-vector map))))
    (update! new-map x y value)))
