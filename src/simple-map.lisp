;;(declaim (optimize (speed 3) (safety 0) (debug 0)))

(defvar *total-lambdas*)

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
    (#\^ 'falling-rock)
    (otherwise 
     (cond ((and (char>= chr #\A)
                 (char<= chr #\I))
            (cons 'trampoline (intern (string chr))))
           ((and (char>= chr #\1)
                 (char<= chr #\9))
            (cons 'target (intern (string chr))))
           (t (error "wrong character"))))))

(defun symbol-to-char (sym)
  (if (and sym
           (symbolp sym))
      (case sym
        (robot #\R)
        (wall #\#)
        (rock #\*)
        (lambda #\\)
        (closed-lift #\L)
        (open-lift #\O)
        (earth #\.)
        (space #\Space)
        (falling-rock #\^)
        (otherwise (error "wrong symbol")))
      (if (and sym
               (consp sym))
          (elt (symbol-name (cdr sym)) 0)
          #\Space)))

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
        (format t "~A~%Score: ~A~%Robot at ~A~%Path: ~A~% Lambdas ~A/~A~%Water level:~A~%"
                (map-to-string (gs-field new-state)) 
                (gs-cur-score new-state)
                (gs-robot-pos new-state)
                (make-path-string new-state)
                (gs-cur-lambdas new-state)
                *total-lambdas*
                (- (gs-water-level g-s) 2)
                )
        ;;(print new-state)
        (if (eq (gs-state new-state) 'in-progress)
            (read-command new-state)
            (progn 
              (format t "Result : ~A~%" (gs-state new-state))
              (format t "Final score : ~A~%" (gs-cur-score new-state))))))))

(defvar *map-metadata* nil)

(defun map-from-stdio ()
  (with-open-file (*standard-input* (second *posix-argv*))
    (let* ((str-lst
            (loop for line = (read-line *standard-input* nil nil)
               while (and line (not (string= line "")))
               collect line))
           (h (length str-lst))
           (w (reduce (lambda (a x) (if (> (length x) a) (length x) a)) str-lst :initial-value 0))
           (smap (create-tree-map (+ w 4) (+ h 4)))
           (i 2)
           (j 2)
           (rob-pos nil))
      ;;Read metadata
      (loop for line = (read-line *standard-input* nil nil)
               while line
               do (push (read-from-string (concatenate 'string "( " line " )"))
                        *map-metadata*))
      (loop for x from 0 to (1+ w) do
           (update! smap x 0 'wall)
           (update! smap x 1 'wall)
           (update! smap x (+ 2 h) 'wall)
           (update! smap x (1+ h) 'wall))
      (loop for y from 0 to (1+ h) do
           (update! smap 0 y 'wall)
           (update! smap 1 y 'wall)
           (update! smap (+ 2 w) y 'wall)
           (update! smap (1+ w) y 'wall))
      (loop for str in (reverse str-lst) do
           (setf j 2)
           (mapcar
            (lambda (chr)
              (let ((sym (char-to-symbol chr)))
                (if (eq sym 'robot) (setf rob-pos (make-pos :x j :y i)))
                (update! smap j i sym)
                (setf j (1+ j))))
            (coerce str 'list))
           (setf i (1+ i)))
      (count-lambdas smap)
      (values smap rob-pos))))

(defun map-to-string (mp)
  (let ((h (map-height mp))
        (w (map-width mp))
        (lst nil))
    (loop for i from 2 to (- h 3) do
          (loop for j from (- w 3) downto 2 do
               (push (symbol-to-char (at-pos mp j i)) lst))
          (push #\NewLine lst))
    (coerce lst 'string)))

(defun count-lambdas (old-map)
  (setf *total-lambdas* 0)
  (let ((h (map-height old-map))
        (w (map-width old-map)))
    (loop for i from 0 to (1- h) do
         (loop for j from 0 to (1- w) do
              (if (eq (at-pos old-map j i) 'lambda)
                  (incf *total-lambdas*))))))

;; Returned states : in-progress, win, lost, aborted

(defun update-robot (old-map robot-pos command cur-score cur-lamdas need-to-be-updated)
  (cond ((eq command 'w)
         (values old-map robot-pos (1- cur-score) cur-lamdas 'in-progress))
        ((eq command 'a)
         (values old-map robot-pos (+ cur-score (* 25 cur-lamdas))
                 cur-lamdas 'aborted))
        (t (let* ((new-x (case command
                           (l (1- (pos-x robot-pos)))
                           (r (1+ (pos-x robot-pos)))
                           (otherwise (pos-x robot-pos))))
                  (x-offs-2 (case command
                              (l (- (pos-x robot-pos) 2))
                              (r (+ (pos-x robot-pos) 2))))
                  (new-y (case command
                           (d (1- (pos-y robot-pos)))
                           (u (1+ (pos-y robot-pos)))
                           (otherwise (pos-y robot-pos))))
                  (map-to-cell (at-pos old-map new-x new-y)))
             (cond ((and
                     (or (eq command 'l)
                         (eq command 'r))
                     (is-rock map-to-cell)
                     (eq (at-pos old-map x-offs-2 new-y)
                         'space))
                    (add-to-heap-around-cell need-to-be-updated x-offs-2 new-y)
                    (add-to-heap-around-cell need-to-be-updated (pos-x robot-pos) (pos-y robot-pos))
                    (add-to-heap-around-cell need-to-be-updated new-x new-y)
                    (values (multi-update old-map 
                                          (pos-x robot-pos) (pos-y robot-pos) 'space
                                          new-x new-y 'robot
                                          x-offs-2 new-y'rock)
                            (make-pos :x new-x :y new-y)
                            (1- cur-score)
                            cur-lamdas
                            'in-progress))
                   ((eq map-to-cell
                        'open-lift)
                    (values (multi-update old-map 
                                          (pos-x robot-pos) (pos-y robot-pos) 'space
                                          new-x new-y 'robot)
                            (make-pos :x new-x :y new-y)
                            (+ cur-score (* 50 cur-lamdas) -1) 
                            cur-lamdas
                            'win))
                   ((case map-to-cell
                      ((space earth lambda) t)
                      (otherwise nil))
                    (let ((lambda-coef (if (eq (at-pos old-map new-x new-y) 'lambda)
                                           1
                                           0)))
                      (add-to-heap-around-cell need-to-be-updated (pos-x robot-pos) (pos-y robot-pos))
                      (add-to-heap-around-cell need-to-be-updated new-x new-y)
                      (values (multi-update old-map 
                                            (pos-x robot-pos) (pos-y robot-pos) 'space
                                            new-x new-y 'robot)
                              (make-pos :x new-x :y new-y)
                              (1- (+ cur-score (* 25 lambda-coef)))
                              (+ lambda-coef cur-lamdas)
                              'in-progress)))
                   ((and (consp map-to-cell)
                         (eq (car map-to-cell)
                             'trampoline))
                    (let* ((target-val (gethash (cdr map-to-cell) *map-trampoline-target*))
                           (target-pos (gethash target-val *map-trampoline-pos*))
                           (all-trampolines (gethash target-val *map-trampoline-target*))
                           (new-map (update old-map
                                            (pos-x robot-pos) (pos-y robot-pos) 'space)))
                      (format t "Teleporting from ~A to ~A at pos ~A, additional ~A~%" (cdr map-to-cell) target-val target-pos all-trampolines)
                      (add-to-heap-around-cell need-to-be-updated (pos-x target-pos) (pos-y target-pos))
                      (add-to-heap-around-cell need-to-be-updated (pos-x robot-pos) (pos-y robot-pos))
                      (loop for tr in all-trampolines
                           do (let ((tr-pos (gethash tr *map-trampoline-pos*)))
                                (add-to-heap-around-cell need-to-be-updated (pos-x tr-pos) (pos-y tr-pos))
                                (setf new-map (update new-map (pos-x tr-pos) (pos-y tr-pos) 'space))))
                      (values (multi-update new-map 
                                            (pos-x target-pos) (pos-y target-pos) 'robot)
                              target-pos
                              (1- cur-score)
                              cur-lamdas
                              'in-progress)))
                   (t (update-robot old-map robot-pos 'w cur-score cur-lamdas need-to-be-updated)))))))

(defun add-to-heap-around-cell (heap x y)
  (heap-insert heap (make-pos :x x :y y))
  (heap-insert heap (make-pos :x (1- x) :y y))
  (heap-insert heap (make-pos :x (1+ x) :y y))
  (heap-insert heap (make-pos :x x :y (1+ y)))
  (heap-insert heap (make-pos :x (1- x) :y (1+ y)))
  (heap-insert heap (make-pos :x (1+ x) :y (1+ y))))

(defun update-cell (old-map new-map i j no-more-lambdas need-to-be-updated)
  (if (is-rock (at-pos old-map j i))
    (cond
      ((eq (at-pos old-map j (1- i)) 'space)
            (let ((tmp-map (update new-map j i 'space)))
              (add-to-heap-around-cell need-to-be-updated j i)
              (heap-insert need-to-be-updated (make-pos :x j :y (1- i)))
              (update tmp-map j (1- i) 'falling-rock)))
      ((and (is-rock (at-pos old-map j (1- i)))
            (eq (at-pos old-map (1+ j) i) 'space)
            (eq (at-pos old-map (1+ j) (1- i)) 'space))
            (let ((tmp-map (update new-map j i 'space)))
              (add-to-heap-around-cell need-to-be-updated j i)
              (heap-insert need-to-be-updated (make-pos :x (1+ j) :y (1- i)))
              (update tmp-map (1+ j) (1- i) 'falling-rock)))
      ((and (is-rock (at-pos old-map j (1- i)))
            (eq (at-pos old-map (1- j) i) 'space)
            (eq (at-pos old-map (1- j) (1- i)) 'space))
            (let ((tmp-map (update new-map j i 'space)))
              (add-to-heap-around-cell need-to-be-updated j i)
              (heap-insert need-to-be-updated (make-pos :x (1- j) :y (1- i)))
              (update tmp-map (1- j) (1- i) 'falling-rock)))
      ((and (eq (at-pos old-map j (1- i)) 'lambda)
            (eq (at-pos old-map (1+ j) i) 'space)
            (eq (at-pos old-map (1+ j) (1- i)) 'space))
            (let ((tmp-map (update new-map j i 'space)))
              (add-to-heap-around-cell need-to-be-updated j i)
              (heap-insert need-to-be-updated (make-pos :x (1+ j) :y (1- i)))
              (update tmp-map (1+ j) (1- i) 'falling-rock)))
      (t (update new-map j i 'rock)))
    (if (eq (at-pos old-map j i) 'closed-lift)
      (progn
        (heap-insert need-to-be-updated (make-pos :x j :y i))
        (if no-more-lambdas
          (update new-map j i 'open-lift)
          new-map))
      new-map)))

(defun quick-update-map (map old-need-to-be-updated no-more-lambdas)
  (let ((new-need-to-be-updated (create-heap #'need-to-be-updated-heap-lambda))
        (new-map map)
        (last-pos nil))
    ;;(format t "HEAP-BEFORE==~A~%" old-need-to-be-updated)
    (loop while (not (heap-empty-p old-need-to-be-updated)) do
          (let ((cur-pos (heap-remove old-need-to-be-updated)))
            ;;(format t "~A:~A==~A~%" (pos-x cur-pos) (pos-y cur-pos) (at-pos map (pos-x cur-pos) (pos-y cur-pos)))
            ;;(format t "HEAP==~A~%" old-need-to-be-updated)
            (unless (and last-pos
                         (and (= (pos-x last-pos) (pos-x cur-pos))
                              (= (pos-y last-pos) (pos-y cur-pos)))) 
                (setf new-map (update-cell map new-map (pos-y cur-pos) (pos-x cur-pos) no-more-lambdas new-need-to-be-updated)))
            (setf last-pos cur-pos)))
    (values new-map new-need-to-be-updated)))

(defun update-map (map need-to-be-updated no-more-lambdas)
  (let ((nmap map)
        (h (map-height map))
        (w (map-width map)))
    (loop for i from 0 to (1- h) do
          (loop for j from 0 to (1- w) do
                (setf nmap (update-cell map nmap i j no-more-lambdas (create-heap #'need-to-be-updated-heap-lambda)))))
    (values nmap (create-heap #'need-to-be-updated-heap-lambda))))

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

(defun is-under-water (water-level robot-pos)
  (<= (pos-y robot-pos)
      water-level))

(defun update-game-state (gs command)
  (let ((old-need-to-be-updated (heap-copy (gs-need-to-be-updated gs))))
    (multiple-value-bind (new-map new-robot-pos new-score new-lambdas after-move-state)
        (update-robot (gs-field gs) (gs-robot-pos gs) command
                      (gs-cur-score gs) (gs-cur-lambdas gs)
                      old-need-to-be-updated)
      ;; (let ((x (pos-x new-robot-pos))
      ;;       (y (pos-y new-robot-pos))
      ;;       (old-x (pos-x (gs-robot-pos gs)))
      ;;       (old-y (pos-y (gs-robot-pos gs))))
      ;;   (add-to-heap-around-cell old-need-to-be-updated old-x old-y)
      ;;   (add-to-heap-around-cell old-need-to-be-updated x y)

      ;;   ;; (heap-insert old-need-to-be-updated (make-pos :x (+ x 2) :y (1+ y)))
      ;;   ;; (heap-insert old-need-to-be-updated (make-pos :x (- x 2) :y (1+ y)))
      ;;   ;; (heap-insert old-need-to-be-updated (make-pos :x (1- x) :y y))
      ;;   ;; (heap-insert old-need-to-be-updated (make-pos :x (1+ x) :y y))
      ;;   ;; (heap-insert old-need-to-be-updated (make-pos :x (+ x 2) :y y))
      ;;   ;; (heap-insert old-need-to-be-updated (make-pos :x (- x 2) :y y))
      ;;   )
      (multiple-value-bind  (new-map-2 new-need-to-be-updated)
          (quick-update-map new-map old-need-to-be-updated (= new-lambdas *total-lambdas*))
        (let* (;;(alternative-state (update-map new-map old-need-to-be-updated (= new-lambdas *total-lambdas*)))
               (new-water-level (if (= (gs-flooding-counter gs) 1)
                                    (1+ (gs-water-level gs))
                                    (gs-water-level gs)))
               (new-waterproof (if (is-under-water new-water-level new-robot-pos)
                                   (1- (gs-cur-waterproof gs))
                                   *map-waterproof*))
               (new-state (if (or (have-lost new-map-2 new-robot-pos)
                                  (< new-waterproof 0))
                              ;;(new-state (if (have-lost alternative-state new-robot-pos)
                              'lost
                              after-move-state))
               (new-flooding-counter (if (<= (gs-flooding-counter gs) 1)
                                         *map-flooding*
                                         (1- (gs-flooding-counter gs)))))
          (make-game-state :field new-map-2;;alternative-state
                           :robot-pos new-robot-pos
                           :cur-score new-score
                           :cur-lambdas new-lambdas
                           :state new-state
                           :need-to-be-updated new-need-to-be-updated
                           :path (cons command (gs-path gs))
                           
                           :water-level new-water-level
                           :cur-waterproof new-waterproof
                           :flooding-counter new-flooding-counter))))))


(defmethod print-object ((obj tree-map) stream)
    (format stream "hash is ~A, map is:~%~A~%" (tree-map-hash obj) (map-to-string obj)))
