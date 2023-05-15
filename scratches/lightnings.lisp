(ql:quickload '(:alexandria :sketch))

(defpackage #:lightning
  (:use #:cl #:sketch)
  (:nicknames #:l)
  (:export #:make-maze
           #:strike))

(in-package #:lightning)

(defun odds? (p)
  (< (random 1000) (* 1000 p)))

(defun make-maze (width height &optional (p 0.5))
  ;; FAKE:
  ;; (aref maze i j) -- 0 to 15 number, each bit correspond to edge
  ;; 0 - closed, 1 - open
  ;; 1 0 1 1 = 11
  ;; ^ ^ ^ ^
  ;; | | | |
  ;; U R D L
  (let ((maze (make-array (list width height 4) :initial-element 0 :element-type 'bit)))
    ;; vertical edges
    (loop for x below width
          do (loop for y below (1- height)
                   when (odds? p)
                   do (incf (aref maze x y 0)) and
                   do (incf (aref maze x (1+ y) 1))))
    ;; horizontal edges
    (loop for x below (1- width)
          do (loop for y below height
                   when (odds? p)
                   do (incf (aref maze x y 2)) and
                   do (incf (aref maze (1+ x) y 3))))
    maze))

(defun maze-neighbours (maze coords)
  (loop with (x y) = coords
        for i below 4
        for (dx dy) in '((0 1) (0 -1) (1 0) (-1 0))
        if (and (= 1 (aref maze x y i))
                (<= 0 (+ x dx) (1- (array-dimension maze 0)))
                (<= 0 (+ y dy) (1- (array-dimension maze 1))))
        collect (cons i (list (+ x dx) (+ y dy)))))

(defun neighbours (arr coords)
  (loop with (x y) = coords
        for i in '(1 0 3 2)
        for (dx dy) in '((0 1) (0 -1) (1 0) (-1 0))
        if (and (<= 0 (+ x dx) (1- (array-dimension arr 0)))
                (<= 0 (+ y dy) (1- (array-dimension arr 1))))
        collect (cons i (list (+ x dx) (+ y dy)))))

(defun strike (maze sx sy ex ey)
  (let ((amazed (make-array (list (array-dimension maze 0)
                                  (array-dimension maze 1))
                            :initial-element nil)))
    (setf (aref amazed sx sy) :start)
    (let* ((q (list (list sx sy)))
           (qend q))
      (loop for head = (car q)
            while (and head (not (aref amazed ex ey)))
            do (loop for (i x y) in (maze-neighbours maze head)
                     unless (aref amazed x y)
                     do (setf (aref amazed x y) i
                              (cdr qend) (list (list x y))
                              qend (cdr qend)))
            do (setf q (cdr q))))
    (if (aref amazed ex ey)
        (let ((striked (make-array (list (array-dimension maze 0)
                                         (array-dimension maze 1))
                                   :initial-element 0 :element-type 'bit)))
          (loop for (x y) = (list ex ey) then (cdr (assoc (aref amazed x y)
                                                          (neighbours maze (list x y))))
                while x
                do (setf (aref striked x y) 1))
          striked))))

(defun make-lightning-canvas (w h &optional (p 0.5))
  (let ((lightning (strike (make-maze w h p)
                           (random w) (random h)
                           (random w) (random h)))
        (color (if (zerop (random 10)) +magenta+ +cyan+))
        (canvas (make-canvas w h)))
    (when lightning
      (loop for i below w
            do (loop for j below h
                     if (= 1 (aref lightning i j))
                     do (canvas-paint canvas color i j)))
      (canvas-image canvas))))

(defun time-from (x &optional (divisor 1))
  (/ (- (get-internal-real-time) x)
     internal-time-units-per-second
     divisor))

(defsketch lightnings ((cis nil) (i 0))
  (background +black+)
  (when (and (zerop (mod i 45))
             (< (length cis) 3))
    (let ((newl (make-lightning-canvas 400 400)))
      (when newl
        (push (list (get-internal-real-time) newl) cis))))
  (setf cis (remove-if (lambda (x) (> (time-from (car x)) 1)) cis))
  (let* ((d (min width height))
         (x (/ (- width d) 2))
         (y (/ (- height d) 2)))
    (loop for (_ ci) in cis
          do (with-pen (make-pen :fill ci)
               (with-current-matrix
                 (translate (/ width 2) (/ height 2))
                 (rotate (* 180 (mod _ 97)))
                 (rotate (time-from _ 10))
                 (scale (+ (expt 1.005 (mod _ 100)) (time-from _ 100)))
                 (translate (/ width -2) (/ height -2))
                 (rect x y d d))))))

(make-instance 'lightnings :resizable t)
