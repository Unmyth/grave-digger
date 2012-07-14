(defstruct (generic-map (:conc-name gm-)
                        (:constructor mk-generic-map))
    hash-function
    compare-function
    table
	(size 0))

(defun make-generic-map (hash-function compare-function)
    (mk-generic-map :hash-function hash-function
                    :compare-function compare-function
                    :table (make-hash-table :test #'eq)))

(defun generic-map-add (gmap key &optional (value t))
    (push (cons key value)
          (gethash (funcall (gm-hash-function gmap) key)
                   (gm-table gmap)))
	(incf (gm-size gmap))
    gmap)

(defun generic-map-size (gmap) (gm-size gmap))

(defun generic-map-get (gmap key)
    (cdr
        (find-if (lambda (x) (funcall (gm-compare-function gmap) (car x) key))
                 (gethash (funcall (gm-hash-function gmap) key)
                          (gm-table gmap)))))

(defun test-generic-map ()
    (let ((m (make-generic-map (lambda (x) (mod x 17)) #'eq))
          (vals '((10 . "zulu")
                  (15 . "vaga")
                  (17 . "rubu")
                  (21 . "zifa")
                  (27 . "huji"))))
        (dolist (val vals)
            (generic-map-add m (car val) (cdr val)))
        (dolist (val vals)
            (format t "~A -> ~A~%" val (equalp (generic-map-get m (car val)) (cdr val)))))
    t)
