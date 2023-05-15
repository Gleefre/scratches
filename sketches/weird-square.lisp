(use-package (ql:quickload :sketch))

(defun rect-in-window (x y w h window-width window-height)
  (and (<= 0 x (+ x w) window-width)
       (<= 0 y (+ y h) window-height)))

(let ((x 1) (y 1)
      (cx 0) (cy 0)
      (dir 1/3)
      (moving-dir 0)
      (v 1/2))
  (defun draw-me (app)
    (with-slots (width height) app
      (rect (incf cx (* v (cos moving-dir))) (incf cy (* v (sin moving-dir)))
            (incf x dir) (incf y dir))
      (unless (and (> (/ width 2) x)
                   (> (/ height 2) y)
                   (rect-in-window cx cy x y width height))
        (setf moving-dir (random (* 2 pi)))
        (setf dir (- (abs dir))))
      (when (or (<= x 10) (<= y 10))
        (setf dir (abs dir))))))

(defsketch app ()
  (draw-me sketch::*sketch*))

(make-instance 'app :resizable t)
