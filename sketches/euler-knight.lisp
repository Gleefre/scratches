(use-package (ql:quickload :sketch))
(use-package (ql:quickload :sketch-fit))

(defun color-filter-alpha (color alpha)
  (apply #'rgb (append (color-rgb color) (list alpha))))

(defun color (name)
  (hex-to-color
   (case name
     (:black "21de21")
     (:white "0cf0d9")
     (:black-used "5f8c5f")
     (:white-used "3c787d")
     (:line "0e1287")
     (:knight "210dd9")
     (:cursor "b02417"))))

(defun draw-knight ()
  (with-font (make-font :color (color :knight))
    (text "N" 0 0 20 20)))

(defsketch euler-knight ((board (make-array '(8 8) :element-type 'bit :initial-element 0))
                         (path ())
                         (cursor nil))
  (fit 160 160 width height)
  (dotimes (x 8)
    (dotimes (y 8)
      (with-pen (make-pen :fill (color (if (oddp (+ x y))
                                           (if (zerop (aref board x y))
                                               :black
                                               :black-used)
                                           (if (zerop (aref board x y))
                                               :white
                                               :white-used))))
        (rect (+ 2 (* x 20)) (+ 2 (* y 20)) 16 16))))
  (when cursor
    (destructuring-bind (x y) cursor
      (with-pen (make-pen :stroke (color :cursor) :weight 2)
        (rect (+ 2 (* x 20)) (+ 2 (* y 20)) 16 16))))
  (when path
    (loop for (px py) in path
          for (x y) in (cdr path)
          do (with-pen (make-pen :stroke (color-filter-alpha (color :line) 0.3))
               (line (+ 10 (* 20 px)) (+ 10 (* 20 py))
                     (+ 10 (* 20 x)) (+ 10 (* 20 y)))))
    (destructuring-bind (x y) (car path)
      (translate (* 20 x) (* 20 y))
      (draw-knight))))

(defun click (app x y)
  (with-slots (path board) app
    (if (null path)
        (progn (setf (aref board x y) 1)
               (push (list x y) path))
        (destructuring-bind (hx hy) (car path)
          (cond
            ((and (= hx x) (= hy y))
             (setf (aref board x y) 0)
             (pop path))
            ((and (= 3 (+ (abs (- x hx))
                          (abs (- y hy))))
                  (plusp (* (abs (- x hx))
                            (abs (- y hy))))
                  (zerop (aref board x y)))
             (setf (aref board x y) 1)
             (push (list x y) path)))))))

(defmethod kit.sdl2:mousebutton-event ((app euler-knight) st ts button x* y*)
  (when (eq st :mousebuttonup)
    (let* ((width (sketch-width app))
           (height (sketch-height app))
           (x* (if (< width height) x* (+ x* (/ (- height width) 2))))
           (y* (if (< height width) y* (+ y* (/ (- width height) 2))))
           (width (min width height))
           (height (min width height))
           (x (floor (/ x* width) 1/8))
           (y (floor (/ y* height) 1/8)))
      (when (and (<= 0 x 7) (<= 0 y 7)) (click app x y)))))

(defun move-cursor (app dx dy)
  (with-slots (cursor) app
    (if cursor
        (setf (car cursor) (mod (+ (car cursor) dx) 8)
              (cadr cursor) (mod (+ (cadr cursor) dy) 8))
        (setf cursor (list 0 0)))))

(defmethod kit.sdl2:keyboard-event ((app euler-knight) st ts rep-p keysym)
  (when (and (eq st :keydown) (not rep-p))
    (case (sdl2:scancode keysym)
      (:scancode-space (with-slots (cursor) app (when cursor (apply #'click app cursor))))
      ((:scancode-left :scancode-a) (move-cursor app -1 0))
      ((:scancode-right :scancode-d) (move-cursor app 1 0))
      ((:scancode-down :scancode-s) (move-cursor app 0 1))
      ((:scancode-up :scancode-w) (move-cursor app 0 -1)))))

(make-instance 'euler-knight)
