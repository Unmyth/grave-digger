(declaim (special *map-metadata*))

(defmacro read-meta-value (var sym)
  (let ((val (gensym)))
    `(let ((,val (assoc ',sym *map-metadata*)))
       (when ,val
         (setf ,var (second ,val))))))

(defun find-trampolines (map)
  (let ((h (map-height map))
        (w (map-width map)))
    (loop for y from 0 to (1- h) do
          (loop for x from 0 to (1- w) do
                (let ((val (at-pos map x y)))
                  (when (consp val)
                    (setf (gethash (cdr val) *map-trampoline-pos*)
                          (make-pos :x x :y y))))))))

(defun count-beards (map)
  (let ((h (map-height map))
        (w (map-width map))
        (num-beards 0)
        (lst nil))
    (loop for y from 0 to (1- h) do
          (loop for x from 0 to (1- w) do
                (when (eq (at-pos map x y)
                          'beard)
                  (incf num-beards)
                  (push (make-pos :x x :y y)
                        lst))))
    (values num-beards lst)))

(defun read-trampolines (data)
  (mapc
   (lambda (trampoline)
     (setf (gethash (second trampoline) *map-trampoline-target*)
           (intern (format nil "~A" (fourth trampoline))))
     (push (second trampoline)
           (gethash (intern (format nil "~A" (fourth trampoline))) *map-trampoline-target*)))
   data))

(defun interpret-metadata ()
  (read-meta-value *map-water-level* water)
  (read-meta-value *map-flooding* flooding)
  (read-meta-value *map-waterproof* waterproof)
  (read-meta-value *map-growth* growth)
  (read-meta-value *map-razors* razors)
  (read-trampolines (remove-if-not (lambda (val)
                                     (eq (car val) 'trampoline))
                                   *map-metadata*)))

(defun read-initial-state ()
    (multiple-value-bind (field rob-pos) (map-from-stdio)
      (find-trampolines field)
      (interpret-metadata)
      (multiple-value-bind (num-beards beard-list) (count-beards field)
        (make-game-state 
         :field field
         :need-to-be-updated (init-need-to-be-updated field)
         :robot-pos rob-pos

         :water-level (+ *map-water-level* 1)
         :flooding-counter *map-flooding*
         :cur-waterproof *map-waterproof*
         
         :num-beards num-beards
         :cur-growth *map-growth*
         :cur-razors *map-razors*
         :possible-beards beard-list))))


(defun main-manual ()
    (let ((gs (read-initial-state)))
      (format t "~A~%Score: ~A~%" (map-to-string (gs-field gs)) 0)
      (read-command gs)))

(defun main-game () 
  (install-handler)
  (play-a-game (read-initial-state)))

(defun install-handler ()
  (sb-sys:enable-interrupt
   sb-posix:sigint
   (lambda (signo context info)
     (declare (ignore signo context info))
     (setf *got-timeout-signal* t)
     ;;(format t "Got sigint~%")
     )))

(defun main-nearest ()
  (install-handler)
  (play-with-nearest-lambdas (read-initial-state)))
;;(defun main-nearest () (test-routes (read-initial-state)))
;;(defun main-nearest () ;;(play-with-nearest-lambdas (read-initial-state)))
;;    (let ((h (create-heap #'<)))
;;        (labels ((-> (el) (heap-insert h el)
;;                          (format t "Heap after ~A insert: ~A~%" el h)))
;;            (-> 10)
;;            (-> 20)
;;            (-> 30)
;;            (loop while (not (heap-empty-p h)) do
;;                (let ((top (heap-remove h)))
;;                    (format t "Heap after removal of ~A: ~A~%" top h))))))
