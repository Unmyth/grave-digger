(defstruct (field-pos (:conc-name fp-))
    came-from
    cost)

(defun move-pos (pos dx dy)
    (make-pos :x (+ (pos-x pos) dx)
              :y (+ (pos-y pos) dy)))

(defstruct (routes-vector (:conc-name rv-))
    vec
    width
    height)

(defun at-routes-vector (rv x y)
    (aref (rv-vec rv) (+ (* y (rv-width rv)) x)))

(defun routes-vector-from-map (field start-pos)
 (let* ((w         (map-width field))
        (h         (map-height field))
        (mirror    (make-array (* w h) :initial-element nil))
        (old-front nil)
        (new-front nil))
     (macrolet ((@* (p) `(aref mirror (+ (* (pos-y ,p) w) (pos-x ,p)))))
         (push start-pos old-front)
         (setf (@* start-pos) (make-field-pos :came-from nil :cost 0))
         (loop while old-front do
            (dolist (point old-front)
             (dolist (delta '((1 . 0) (0 . 1) (-1 . 0) (0 . -1)))
              (let* ((dx (car delta))
                     (dy (cdr delta))
                     (new-pos (move-pos point dx dy))
                     (new-cost (1+ (fp-cost (@* point)))))
                (unless (or (>= (pos-x new-pos) w)
                            (< (pos-x new-pos) 0)
                            (>= (pos-y new-pos) h)
                            (< (pos-y new-pos) 0))
                  (let ((map-val (at-pos field (+ (pos-x point) dx) (+ (pos-y point) dy))))
                   (when (and (not (eq map-val 'wall))
                              (or (null (@* new-pos))
                                  (> (fp-cost (@* new-pos)) new-cost)))
                       (setf (@* new-pos) (make-field-pos :cost new-cost :came-from point))
                       (push new-pos new-front)))))))
             (setf old-front new-front
                   new-front nil)))
     (make-routes-vector :vec mirror :width w :height h)))

(defun show-costs (rv)
  (loop for y from (1- (rv-height rv)) downto 0 do
    (loop for x from 0 to (1- (rv-width rv)) do
      (format t "~2,'0D " (let ((val (at-routes-vector rv x y)))
                        (if val (fp-cost val)
                            ".."))))
    (format t "~%")))

(defun test-routes (state)
    (format t "Costs for map:~%~A~%" (map-to-string (gs-field state)))
    (show-costs (routes-vector-from-map (gs-field state) (gs-robot-pos state))))
 
