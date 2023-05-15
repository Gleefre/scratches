(ql:quickload :sketch)
(use-package :sketch)

(defsketch tutorial ()
  (background +blue+)
  (with-pen (make-pen :fill +green+)
    (dotimes (i 10)
      (rect 20 (+ 5 (* i 50)) ; position of top left corner
            (* (1+ i) 30) 40))) ; width & height
  (with-pen (make-pen :fill +yellow+)
    (circle 300 100 75)) ; (x, y) of center; radius
  (polygon 100 100
           200 150
           300 100
           200 200) ; (x, y) of corners
  (with-pen (make-pen :fill +red+)
    (dotimes (i 4)
      (ngon (+ i 3) ; number of vertexes
            (+ 225 (* i 30)) (+ 275 ( * i 50)) ; (x, y) of center
            20 20 ; radius x, radius y
            -90))) ; rotation
  (with-pen (make-pen :stroke +magenta+ :weight 5)
    (bezier 10 300 10 10 390 10 390 200)))

(make-instance 'tutorial :height 500)
