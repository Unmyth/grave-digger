
;; Symbols:
;; robot, wall, rock, lambda, closed-lift, open-lift, earth, space, falling-rock

(defun is-rock (sym)
  (or (eq sym 'rock)
      (eq sym 'falling-rock)))

(defclass a-map ()
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

(defun copy-array (arr)
  (let* ((linear-arr (make-array 
                      (* (1+ (row-major-aref arr 0))
                         (1+ (row-major-aref arr 1)))
                      :displaced-to arr))
         (linear-copy (copy-seq linear-arr)))
    (make-array (list (1+ (row-major-aref arr 0))
                      (1+ (row-major-aref arr 1)))
                :displaced-to linear-copy)))

(defgeneric at-pos (map x y))

(defgeneric update (map x y value))

(defgeneric update! (map x y value))

(defun create-simple-map (width height)
  (let ((vect (make-array (list width height))))
    (make-instance 'simple-map :width width :height height :vector vect)))

(defmethod at-pos ((map simple-map) x y)
  (aref (simple-map-vector map) x y))

(defmethod update! ((map simple-map) x y value)
  (setf (aref (simple-map-vector map) x y)
        value))

(defmethod update ((map simple-map) x y value)
  (let ((new-map (copy-array (simple-map-vector map))))
    (update! new-map x y value)))

;; Hierarchical map implementation

(defun tree-map-at (map cur-size x y)
  (if (= cur-size 2)
      (aref map x y)
      (let* ((middle (truncate cur-size 2))
             (local-x (truncate x middle))
             (local-y (truncate y middle))
             (x-offs (- x (* middle local-x)))
             (y-offs (- y (* middle local-y))))
        (tree-map-at (aref map local-x local-y) 
                     middle x-offs y-offs))))