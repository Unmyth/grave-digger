(defun char-to-symbol (chr)
  (case chr
    (#\R 'robot)
    (#\# 'wall)
    (#\* 'rock)
    (#\\ 'lambda)
    (#\L 'closed-lift)
    (#\O 'open-lift)
    (#\. 'earth)
    (#\Space 'space)
    (#\@ 'falling-rock)
    (otherwise (error "wrong character"))))

(defun symbol-to-char (sym)
  (if sym
      (case sym
        (robot #\R)
        (wall #\#)
        (rock #\*)
        (lambda #\\)
        (closed-lift #\L)
        (open-lift #\O)
        (earth #\.)
        (space #\Space)
        (falling-rock #\@)
        (otherwise (error "wrong symbol")))
      #\Space))

(defun make-path-string (state)
    (apply #'concatenate 'string (mapcar #'symbol-name (reverse (gs-path state)))))

(defun read-command (g-s)
  (let* ((chr (read-char))
         (sym 
            (case chr
              (#\w 'u)
              (#\a 'l)
              (#\s 'd)
              (#\d 'r)
              (#\Space 'w)
              (#\q 'a)
              (otherwise 'error))))
    (if (eq sym 'error)
        (read-command g-s)
      (let ((new-state (update-game-state g-s sym)))
        (format t "~A~%Score: ~A~%Robot at ~A~%Path: ~A~%" 
                (map-to-string (gs-field new-state)) 
                (gs-cur-score new-state)
                (gs-robot-pos new-state)
                (make-path-string new-state))
        (if (eq (gs-state new-state) 'in-progress)
            (read-command new-state))))))

(defun map-from-stdio ()
  (let* ((file (second *posix-argv*))
         (str-lst
           (with-open-file (*standard-input* file)
             (loop for line = (read-line *standard-input* nil nil)
               while line
               collect line)))
         (h (length str-lst))
         (w (reduce (lambda (a x) (if (> (length x) a) (length x) a)) str-lst :initial-value 0))
         (smap (create-tree-map w h))
         (i 0)
         (j 0)
         (rob-pos nil))
    (loop for str in (reverse str-lst) do
          (setf j 0)
          (mapcar
            (lambda (chr)
              (let ((sym (char-to-symbol chr)))
                (if (eq sym 'robot) (setf rob-pos (make-pos :x j :y i)))
                (update! smap j i sym)
                (setf j (1+ j))))
            (coerce str 'list))
          (setf i (1+ i)))
    (count-lambdas smap)
    (values smap rob-pos)))

(defun map-to-string (mp)
  (let ((h (map-height mp))
        (w (map-width mp))
        (lst nil))
    (loop for i from 0 to (1- h) do
          (loop for j from (1- w) downto 0 do
               (push (symbol-to-char (at-pos mp j i)) lst))
          (push #\NewLine lst))
    (coerce lst 'string)))

(defvar *total-lambdas*)

(defun count-lambdas (old-map)
  (setf *total-lambdas* 0)
  (let ((h (map-height old-map))
        (w (map-width old-map)))
    (loop for i from 0 to (1- h) do
         (loop for j from 0 to (1- w) do
              (if (eq (at-pos old-map j i) 'lambda)
                  (incf *total-lambdas*))))))

;; Returned states : in-progress, win, lost, aborted

(defun update-robot (old-map robot-pos command cur-score cur-lamdas)
  (cond ((eq command 'w)
         (values old-map robot-pos (1- cur-score) cur-lamdas 'in-progress))
        ((eq command 'a)
         (values old-map robot-pos (+ cur-score (* 25 cur-lamdas)) cur-lamdas 'aborted))
        (t (let ((new-x (case command
                          (l (1- (pos-x robot-pos)))
                          (r (1+ (pos-x robot-pos)))
                          (otherwise (pos-x robot-pos))))
                 (x-offs-2 (case command
                             (l (- (pos-x robot-pos) 2))
                             (r (+ (pos-x robot-pos) 2))))
                 (new-y (case command
                          (d (1- (pos-y robot-pos)))
                          (u (1+ (pos-y robot-pos)))
                          (otherwise (pos-y robot-pos)))))
             (cond ((and
                     (or (eq command 'l)
                         (eq command 'r))
                     (is-rock (at-pos old-map new-x new-y))
                     (eq (at-pos old-map x-offs-2 new-y)
                         'space))
                    (values (multi-update old-map 
                                          (pos-x robot-pos) (pos-y robot-pos) 'space
                                          new-x new-y 'robot
                                          x-offs-2 new-y'rock)
                            (make-pos :x new-x :y new-y)
                            (1- cur-score)
                            cur-lamdas
                            'in-progress))
                   ((eq (at-pos old-map new-x new-y)
                        'open-lift)
                    (values (multi-update old-map 
                                          (pos-x robot-pos) (pos-y robot-pos) 'space
                                          new-x new-y 'robot)
                            (make-pos :x new-x :y new-y)
                            (+ cur-score (* 50 cur-lamdas) -1) 
                            cur-lamdas
                            'win))
                   ((case (at-pos old-map new-x new-y)
                      ((space earth lambda) t)
                      (otherwise nil))
                    (let ((lambda-coef (if (eq (at-pos old-map new-x new-y) 'lambda)
                                           1
                                           0)))
                      (values (multi-update old-map 
                                            (pos-x robot-pos) (pos-y robot-pos) 'space
                                            new-x new-y 'robot)
                              (make-pos :x new-x :y new-y)
                              (1- (+ cur-score (* 25 lambda-coef)))
                              (+ lambda-coef cur-lamdas)
                              'in-progress)))
                   (t (update-robot old-map robot-pos 'w cur-score cur-lamdas)))))))

(defun update-cell (old-map new-map i j no-more-lambdas need-to-be-updated)
  (if (is-rock (at-pos old-map j i))
    (cond
      ((eq (at-pos old-map j (1- i)) 'space)
            (let ((tmp-map (update new-map j i 'space)))
                (heap-insert need-to-be-updated (make-pos :x j :y (1- i)))
                (update tmp-map j (1- i) 'falling-rock)))
      ((and (eq (at-pos old-map j (1- i)) 'rock)
            (eq (at-pos old-map (1+ j) i) 'space)
            (eq (at-pos old-map (1+ j) (1- i)) 'space))
            (let ((tmp-map (update new-map j i 'space)))
                (heap-insert need-to-be-updated (make-pos :x (1+ j) :y (1- i)))
                (update tmp-map (1+ j) (1- i) 'falling-rock)))
      ((and (eq (at-pos old-map j (1- i)) 'rock)
            (eq (at-pos old-map (1- j) i) 'space)
            (eq (at-pos old-map (1- j) (1- i)) 'space))
            (let ((tmp-map (update new-map j i 'space)))
                (heap-insert need-to-be-updated (make-pos :x (1- j) :y (1- i)))
                (update tmp-map (1- j) (1- i) 'falling-rock)))
      ((and (eq (at-pos old-map j (1- i)) 'lambda)
            (eq (at-pos old-map (1+ j) i) 'space)
            (eq (at-pos old-map (1+ j) (1- i)) 'space))
            (let ((tmp-map (update new-map j i 'space)))
                (heap-insert need-to-be-updated (make-pos :x (1+ j) :y (1- i)))
                (update tmp-map (1+ j) (1- i) 'falling-rock)))
      (t (update new-map j i 'rock)))
    (if (eq (at-pos old-map j i) 'closed-lift)
      (progn
        (heap-insert need-to-be-updated (make-pos :x j :y i))
        (if no-more-lambdas
          (update new-map j i 'open-lift))
          new-map)
      new-map)))

(defun quick-update-map (map need-to-be-updated no-more-lambdas)
  (let ((new-need-to-be-updated (create-heap #'need-to-be-updated-heap-lambda))
        (new-map map))
     (loop while (not (heap-empty-p need-to-be-updated)) do
           (let ((cur-pos (heap-remove need-to-be-updated)))
             (setf new-map (update-cell map new-map (pos-y cur-pos) (pos-x cur-pos) no-more-lambdas new-need-to-be-updated))))
     (values new-map new-need-to-be-updated)))


(defun update-map (map no-more-lambdas)
  (let ((nmap map)
        (h (map-height map))
        (w (map-width map)))
    (loop for i from 0 to (1- h) do
          (loop for j from 0 to (1- w) do
                (setf nmap (update-cell map nmap i j no-more-lambdas (create-heap #'need-to-be-updated-heap-lambda)))))
    nmap))

(defun need-to-be-updated-heap-lambda (a b)
  (if (< (pos-y a) (pos-y b))
      t
      (if (> (pos-y a) (pos-y b))
          nil
          (if (< (pos-x a) (pos-x b))
            t
            nil))))

(defun init-need-to-be-updated (map)
  (let ((heap (create-heap #'need-to-be-updated-heap-lambda))
        (h (map-height map))
        (w (map-width map)))
    (loop for i from 0 to (1- h) do
          (loop for j from 0 to (1- w) do
                (heap-insert heap (make-pos :x j :y i))))
    heap))

(defun have-lost (the-map robot-pos)
  (eq (at-pos the-map 
              (pos-x robot-pos)
              (1+ (pos-y robot-pos)))
      'falling-rock))

(defun update-game-state (gs command)
  (let ((old-need-to-be-updated (gs-need-to-be-updated gs)))
    (multiple-value-bind (new-map new-robot-pos new-score new-lambdas after-move-state)
        (update-robot (gs-field gs) (gs-robot-pos gs) command
                      (gs-cur-score gs) (gs-cur-lambdas gs))
        (heap-insert old-need-to-be-updated (make-pos :x (1- (pos-x new-robot-pos)) :y (1+ (pos-y new-robot-pos))))
        (heap-insert old-need-to-be-updated (make-pos :x (pos-x new-robot-pos) :y (1+ (pos-y new-robot-pos))))
        (heap-insert old-need-to-be-updated (make-pos :x (1+ (pos-x new-robot-pos)) :y (1+ (pos-y new-robot-pos))))
        (heap-insert old-need-to-be-updated (make-pos :x (1- (pos-x new-robot-pos)) :y (pos-y new-robot-pos)))
        (heap-insert old-need-to-be-updated (make-pos :x (1+ (pos-x new-robot-pos)) :y (pos-y new-robot-pos)))
        (multiple-value-bind  (new-map-2 new-need-to-be-updated)
              (quick-update-map new-map old-need-to-be-updated (= new-lambdas *total-lambdas*))
              (let ((new-state (if (have-lost new-map-2 new-robot-pos)
                                    'lost
                                    after-move-state)))
                (make-game-state :field new-map-2
                                 :robot-pos new-robot-pos
                                 :cur-score new-score
                                 :cur-lambdas new-lambdas
                                 :state new-state
                                 :need-to-be-updated new-need-to-be-updated
                                 :path (cons command (gs-path gs))))))))


(defmethod print-object ((obj tree-map) stream)
    (format stream "hash is ~A, map is:~%~A~%" (tree-map-hash obj) (map-to-string obj)))
