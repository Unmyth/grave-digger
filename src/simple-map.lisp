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

(defun map-from-string (str)
  (let* ((lst (coerce str 'list))
         (h (1+ (count #\NewLine str)))
         (w (- (length lst) (length (member #\NewLine lst))))
         (smap (create-tree-map w h))
         (i 0)
         (j 0))
    (mapcar
      (lambda (chr)
        (if (eq chr #\NewLine)
          (progn (setf i (1+ i)) (setf j 0))
          (let ((sym (char-to-symbol chr)))
            (update! smap j i sym)
            (setf j (1+ j)))))
      lst)
    smap))

(defun map-to-string (mp)
  (let ((h (map-height mp))
        (w (map-width mp))
        (lst nil))
    (loop for i from (1- h) downto 0 do
          (loop for j from (1- w) downto 0 do
               (push (symbol-to-char (at-pos mp j i)) lst))
          (if (> i 0) (push #\NewLine lst)))
    (coerce lst 'string)))

(defun test ()
  (let ((a (map-from-string "######
#. *R#
#  \\.#
#\\ * #
L  .\\#
######")))
;     (print a)
     (format t "~A~%" (map-to-string a))
  ))

