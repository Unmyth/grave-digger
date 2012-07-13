
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

(defclass simple-map (a-map)
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

(defun copy-update-array (arr x y value)
  (let ((copy (copy-array arr)))
    (setf (aref arr x y) value)
    copy))

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

(defconstant +big-prime+ 15485543)

(defstruct tree-map-node
  subnodes hash-value)

(defun get-symbol-hash (sym)
  (case sym
    (robot 131511243)
    (wall 78631441)
    (rock 49869)
    (lambda 2938407)
    (closed-lift 24254753)
    (open-lift 12452351)
    (earth 2345211)
    (space 32426187)
    (falling-rock 235226509)))

(defun tree-map-node-hash (node)
  (if (symbolp node)
      (get-symbol-hash node)
      (if (tree-map-node-hash-value node)
          (tree-map-node-hash-value node)
          (let ((hash-val 0))
            (dolist (x '(0 1))
              (dolist (y '(0 1))
                (setf hash-val 
                      (+ hash-val 
                         (tree-map-node-hash (aref (tree-map-node-subnodes node) x y))))))
                (setf hash-val (mod hash-val +big-prime+))
                (setf (tree-map-node-hash-value node)
                      hash-val)
                hash-val))))

(defun update-node-hash (node)
  (tree-map-node-hash node)
  nil)

(defun create-node (subnodes)
  (let ((node (make-tree-map-node :subnodes subnodes)))
    (tree-map-node-hash node)
    node))

(defun tree-map-walk-update (node cur-size func terminal-func x y)
  "Call (func node subnode x y) for intermediate nodes and (terminal-func node x y) for leaf node along the path.
x and y are always local to the node."
  (if (= cur-size 2)
      (funcall terminal-func node x y)
      (let* ((middle (truncate cur-size 2))
             (local-x (truncate x middle))
             (local-y (truncate y middle))
             (x-offs (- x (* middle local-x)))
             (y-offs (- y (* middle local-y)))
             (subnode (aref (tree-map-node-subnodes node) local-x local-y)))
        (unless subnode
          (setf subnode (make-tree-map-node :subnodes (make-array '(2 2))))
          (setf (aref (tree-map-node-subnodes node) local-x local-y)
                subnode))
        (funcall func 
                 node
                 (tree-map-walk-update 
                  subnode middle
                  func terminal-func
                  x-offs y-offs)
                 x-offs
                 y-offs))))

(defun tree-map-at (node cur-size x y)
  (tree-map-walk-update node cur-size
                        (lambda (node subnode x y)
                          (declare (ignore node x y))
                          subnode)
                        (lambda (node x y)
                          (aref (tree-map-node-subnodes node) x y))
                        x y))

(defun tree-map-update! (node cur-size x y value)
  (tree-map-walk-update node cur-size
                        (lambda (node subnode x y)
                          (declare (ignore node subnode x y))
                          nil)
                        (lambda (node x y)
                          (setf (aref (tree-map-node-subnodes node) x y)
                                value)
                          nil)
                        x y))

(defun tree-map-update (node cur-size x y value)
  (tree-map-walk-update node cur-size
                        (lambda (node subnode x y)
                          (create-node
                           (copy-update-array
                            (tree-map-node-subnodes node)
                            x y
                            subnode)))
                        (lambda (node x y)
                          (create-node
                           (copy-update-array 
                            (tree-map-node-subnodes node) x y
                            value)))
                        x y))

(defun tree-map-node-equals (node1 node2)
  (if (eq node1 node2)
      t
      (if (and (typep node1 'tree-map-node)
               (typep node2 'tree-map-node))
          (when (= (tree-map-node-hash node1)
                   (tree-map-node-hash node2))
            (dolist (x '(0 1))
              (dolist (y '(0 1))
                (unless (tree-map-node-equals (aref (tree-map-node-subnodes node1) x y)
                                              (aref (tree-map-node-subnodes node2) x y))
                  (return-from tree-map-node-equals nil))))
            t)
          nil)))

(defclass tree-map (a-map)
  ((top-node :accessor tree-map-top
             :initarg top-node
             :type tree-map-node)
   (size :accessor tree-map-size
         :initarg :size
         :type integer)))

(defun create-tree-map (width height)
  (let ((subnodes (make-array '(2 2)))
        (size (ash 2 (integer-length (max width height)))))
    (make-instance 'tree-map
                   :top-node (make-tree-map-node :subnodes subnodes)
                   :width width
                   :height height
                   :size size)))

(defun tree-map-hash (tree-map)
  (tree-map-node-hash (tree-map-top tree-map)))

(defun tree-map-equals (tree-map1 tree-map2)
  (and
   (= (tree-map-size tree-map1)
      (tree-map-size tree-map2))
   (tree-map-node-equals (tree-map-top tree-map1)
                         (tree-map-top tree-map2))))

(defmethod at-ops ((map tree-map) x y)
  (tree-map-at (tree-map-top map) (tree-map-size map)
               x y))

(defmethod update! ((map tree-map) x y value)
  (tree-map-update! (tree-map-top map) (tree-map-size map)
                    x y value))

(defmethod update ((map tree-map) x y value)
  (make-instance 'tree-map
   :top-node (tree-map-update (tree-map-top map) (tree-map-size map)
                              x y value)
   :width (map-width map)
   :height (map-height map)
   :size (tree-map-size map)))
