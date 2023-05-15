(ql:quickload :alexandria)
(use-package (ql:quickload :sketch))
(use-package (ql:quickload :sketch-fit))

(defun solve-triangles (n lines)
  (let ((paired (make-hash-table)))
    (macrolet ((pair (x y)
                 `(gethash (+ (min ,x ,y) (* (max ,x ,y) n)) paired)))
      (loop for line in lines
            do (loop for (x . rest) on line
                     do (loop for y in rest
                              do (setf (pair x y) t))))
      (- (loop for i from 1 to n
               sum (loop for j from (1+ i) to n
                         when (pair i j)
                         sum (loop for k from (1+ j) to n
                                   count (and (pair i k) (pair j k)))))
         (loop for line in lines
               when (> (length line) 2)
               sum (alexandria:binomial-coefficient (length line) 3))))))

(defparameter *20*
  '(7 ((1 2 5)
       (1 3 6)
       (1 4 7)
       (2 3 4)
       (2 6)
       (4 6)
       (5 6 7))))

(defparameter *30*
  '(12 ((1 2)
        (1 3 6 8)
        (1 4 7 11)
        (1 5)
        (2 3 4 5)
        (2 6 9 12)
        (2 8)
        (5 11)
        (5 7 10 12)
        (8 9 10 11)
        (8 12)
        (11 12))))

(defparameter *50*
  '(11 ((1 2 4 10)
        (1 3 6 11)
        (10 11)
        (3 5 7 10)
        (6 8 9 10)
        (2 5 8 11)
        (4 7 9 11))))

(defun make-grid (n m)
  (list (make-array (list n (1- m)) :initial-element nil :element-type 'boolean)
        (make-array (list (1- n) m) :initial-element nil :element-type 'boolean)))

(defun edge (grid x y direction)
  (case direction
    (:up    (aref (car  grid) x (1- y)))
    (:down  (aref (car  grid) x y))
    (:left  (aref (cadr grid) (1- x) y))
    (:right (aref (cadr grid) x y))))

(defun (setf edge) (value grid x y direction)
    (case direction
      (:up    (setf (aref (car  grid) x (1- y)) value))
      (:down  (setf (aref (car  grid) x y) value))
      (:left  (setf (aref (cadr grid) (1- x) y) value))
      (:right (setf (aref (cadr grid) x y) value))))

(defun dir (x y x* y*)
  (cond ((< x x*) :right)
        ((> x x*) :left)
        ((< y y*) :down)
        ((> y y*) :up)))

(defun look-dir (xy dir)
  (mapcar #'+ xy
          (case dir
            (:up '(0 -1)) (:down '(0 1))
            (:left '(-1 0)) (:right '(1 0)))))

(defun side? (xy dir count grid)
  (loop repeat count
        always (edge grid (car xy) (cadr xy) dir)
        do (setf xy (look-dir xy dir))))

(defun toggle-side (xy xy* grid &aux (dir (dir (car xy) (cadr xy) (car xy*) (cadr xy*))))
  (loop until (equal xy xy*)
        do (setf (edge grid (car xy) (cadr xy) dir)
                 (not (edge grid (car xy) (cadr xy) dir))
                 xy (look-dir xy dir))))

(defun (setf side?) (value xy dir count grid)
  (loop repeat count
        do (setf (edge grid (car xy) (cadr xy) dir) value
                 xy (look-dir xy dir))))

(defun square? (x y side grid)
  (and (side? (list x y) :down side grid)
       (side? (list (+ x side) y) :down side grid)
       (side? (list x y) :right side grid)
       (side? (list x (+ y side)) :right side grid)))

(defun squares (n m grid)
  (loop for x below n
        sum (loop for x* from (1+ x) below n
                  for dx = (- x* x)
                  sum (loop for y below (- m dx)
                            for y* = (+ y dx)
                            count (square? x y dx grid)))))

(defun color (color-name)
  (case color-name
    (:back +black+)
    (:text +white+)
    (:active-point (rgb 0.5 0.8 0))
    (:point (rgb 0.5 0.5 0))
    (:active-edge (rgb 0.5 1 0.7))
    (:edge (rgb 0 0 0.3))))

(defmacro with-color (where color-name &body body &aux (color (gensym)))
  `(let ((,color (color ,color-name)))
     (with-pen (make-pen ,@(let ((args (alexandria:ensure-list where)))
                             (loop while args
                                   collect (car args)
                                   if (member (car args) '(:fill :stroke))
                                     collect color
                                   else
                                     collect (cadr args) and
                                     do (setf args (cdr args))
                                   do (setf args (cdr args)))))
       ,@body)))

(defsketch square-solver ((n 6) (m 6)
                          (grid (make-grid n m))
                          (choice nil) (cell-size 40))
  (background (color :back))
  (with-font (make-font :size 30 :color (color :text) :align :center)
    (text (format nil "Squares: ~a" (squares n m grid)) (/ width 2) 40))
  (fit (* n cell-size) (* m cell-size) width height)
  (translate (/ cell-size 2) (/ cell-size 2))
  (dotimes (x n)
    (dotimes (y m)
      (when (> x 0)
        (with-color (:weight 2 :stroke) (if (edge grid x y :left) :active-edge :edge)
          (line (* x cell-size) (* y cell-size) (* (1- x) cell-size) (* y cell-size))))
      (when (> y 0)
        (with-color (:weight 2 :stroke) (if (edge grid x y :up) :active-edge :edge)
          (line (* x cell-size) (* y cell-size) (* x cell-size) (* (1- y) cell-size))))))
  (dotimes (x n)
    (dotimes (y m)
      (with-color :fill (if (member (list x y) choice :test 'equal) :active-point :point)
        (circle (* x cell-size) (* y cell-size) 2)))))

(defmethod kit.sdl2:mousebutton-event ((app square-solver) st ts button x y)
  (when (eq st :mousebuttondown)
    (with-slots (n m grid choice cell-size width height) app
      (destructuring-bind (x y) (fit-point x y (* n cell-size) (* m cell-size) width height)
        (let ((x (round (- x (/ cell-size 2)) cell-size))
              (y (round (- y (/ cell-size 2)) cell-size)))
          (when (and (<= 0 x (1- n))
                     (<= 0 y (1- m)))
            (if choice
                (when (zerop (apply #'* (mapcar #'- (car choice) (list x y))))
                  (toggle-side (car choice) (list x y) grid)
                  (setf choice nil))
                (setf choice (list (list x y))))))))))
