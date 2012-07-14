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

(defun map-from-stdio ()
  (let* ((str-lst
           (loop for line = (read-line *standard-input* nil nil)
             while line
             collect line))
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
        (update old-map j i 'open-lift))))

(defun update-map (map)
  (let ((nmap map)
        (h (map-height map))
        (w (map-width map)))
    (loop for i from 0 to (1- h) do
          (loop for j from 0 to (1- w) do
                (setf nmap (update-cell nmap i j))))
    nmap))
