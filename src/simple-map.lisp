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

(defun read-command (g-s)
  (let* ((chr (read-char))
         (sym 
            (case chr
              (#\w 'u)
              (#\a 'l)
              (#\s 'd)
              (#\d 'r)
              (#\Space 'w)
              (#\q 'a)))
         (new-state update-game-state g-s sym))
    (read-command new-state)))




(defun map-from-stdio ()
  (let* ((file (second *posix-argv*))
         (str-lst
           (with-open-file (*standard-input* file)
             (loop for line = (read-line *standard-input* nil nil)
               while line
               collect line)))
         (h (length str-lst))
         (w (reduce (lambda (a x) (if (> (length x) a) (length x) a)) str-lst :initial-value 0))
         (smap (create-simple-map w h))
         (i 0)
         (j 0))
    (loop for str in (reverse str-lst) do
          (setf j 0)
          (mapcar
            (lambda (chr)
              (update! smap j i (char-to-symbol chr))
              (setf j (1+ j)))
            (coerce str 'list))
          (setf i (1+ i)))
    (format t "~A~%" (map-to-string smap))))

(defun map-to-string (mp)
  (let ((h (map-height mp))
        (w (map-width mp))
        (lst nil))
    (loop for i from 0 to (1- h) do
          (loop for j from (1- w) downto 0 do
               (push (symbol-to-char (at-pos mp j i)) lst))
          (push #\NewLine lst))
    (coerce lst 'string)))

(defun no-more-lambdas (old-map)
  (let ((h (map-height old-map))
        (w (map-width old-map))
        (flag t))
    (loop for i from 0 to (1- h) do
      (loop for j from 0 to (1- w) do
        (if (eq (at-pos old-map j i) 'lambda)
          (setf flag nil))))
    flag))

;; Returned states : in-progress, win, lost, aborted

(defun update-robot (old-map robot-pos command cur-score cur-lamdas)
  (cond ((eq command 'w)
         (values old-map robot-pos (1- cur-score) cur-lamdas 'in-progress))
        ((eq command 'a)
         (values old-map robot-pos (+ cur-score (* 25 cur-lamdas)) cur-lamdas 'aborted))
        (t (let ((new-x (case command
                          (l (1- (pos-x robot-pos)))
                          (r (1+ (pos-x robot-pos)))))
                 (x-offs-2 (case command
                             (l (- (pos-x robot-pos) 2))
                             (r (+ (pos-x robot-pos) 2))))
                 (new-y (case command
                          (d (1- (pos-y robot-pos)))
                          (u (1+ (pos-y robot-pos))))))
             (cond ((and
                     (or (eq command 'l)
                         (eq command 'r))
                     (is-rock (at-pos old-map new-x new-y))
                     (eq (at-pos old-map x-offs-2 new-y)
                         'empty))
                    (values (multi-update old-map 
                                          (pos-x robot-pos) (pos-y robot-pos) 'empty
                                          new-x new-y 'robot
                                          x-offs-2 new-y'rock)
                            (make-pos :x new-x :y new-y)
                            (1- cur-score)
                            cur-lamdas
                            'in-progress))
                   ((eq (at-pos old-map new-x new-y)
                        'open-lift)
                    (values (multi-update old-map 
                                          (pos-x robot-pos) (pos-y robot-pos) 'empty
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
                                            (pos-x robot-pos) (pos-y robot-pos) 'empty
                                            new-x new-y 'robot)
                              (make-pos :x new-x :y new-y)
                              (1- (+ cur-score (* 25 lambda-coef)))
                              (+ lambda-coef cur-lamdas)
                              'in-progress)))
                   (t (update-robot old-map robot-pos 'w cur-score cur-lamdas)))))))

(defun update-cell (old-map i j)
  (if (is-rock (at-pos old-map j i))
    (cond
      ((eq (at-pos old-map j (1- i)) 'space)
            (let ((tmp-map (update old-map j i 'space)))
                (update tmp-map j (1- i) 'falling-rock)))
      ((and (eq (at-pos old-map j (1- i)) 'rock)
            (eq (at-pos old-map (1+ j) i) 'space)
            (eq (at-pos old-map (1+ j) (1- i)) 'space))
            (let ((tmp-map (update old-map j i 'space)))
                (update tmp-map (1+ j) (1- i) 'falling-rock)))
      ((and (eq (at-pos old-map j (1- i)) 'rock)
            (eq (at-pos old-map (1- j) i) 'space)
            (eq (at-pos old-map (1- j) (1- i)) 'space))
            (let ((tmp-map (update old-map j i 'space)))
                (update tmp-map (1- j) (1- i) 'falling-rock)))
      ((and (eq (at-pos old-map j (1- i)) 'lambda)
            (eq (at-pos old-map (1+ j) i) 'space)
            (eq (at-pos old-map (1+ j) (1- i)) 'space))
            (let ((tmp-map (update old-map j i 'space)))
                (update tmp-map (1+ j) (1- i) 'falling-rock)))
      (t (update old-map j i 'rock)))
      (if (and (eq (at-pos old-map j i) 'closed-lift) ;;No need to search all map for lambdas everyt update. Can be optimized
               (no-more-lambdas old-map))
        (update old-map j i 'open-lift)
        old-map)))

(defun update-map (map)
  (let ((nmap map)
        (h (map-height map))
        (w (map-width map)))
    (loop for i from 0 to (1- h) do
          (loop for j from 0 to (1- w) do
                (setf nmap (update-cell nmap i j))))
    nmap))

(defun have-lost (the-map robot-pos)
  (eq (at-pos the-map 
              (pos-x robot-pos)
              (1+ (pos-y robot-pos)))
      'falling-rock))

(defun update-game-state (gs command)
  (multiple-value-bind (new-map new-robot-pos new-score new-lambdas after-move-state)
      (update-robot (gs-field gs) (gs-robot-pos gs) command
                    (gs-cur-score gs) (gs-cur-lambdas gs))
    (let* ((new-map-2 (update-map new-map))
           (new-state (if (have-lost new-map-2 new-robot-pos)
                          'lost
                          after-move-state)))
      (make-game-state :field new-map-2
                       :robot-pos new-robot-pos
                       :cur-score new-score
                       :cur-lambdas new-lambdas
                       :state new-state
                       :path (cons command (gs-path gs))))))
