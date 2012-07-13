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
    (otherwise (error "wrong symbol"))))

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
    (loop for str in str-lst do
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
    (loop for i from (1- h) downto 0 do
          (loop for j from (1- w) downto 0 do
               (push (symbol-to-char (at-pos mp j i)) lst))
          (if (> i 0) (push #\NewLine lst)))
    (coerce lst 'string)))
