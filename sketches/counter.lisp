(use-package (ql:quickload :sketch))
(use-package (ql:quickload :sketch-fit))

(defsketch counter ((n 0)
                    (pen (make-pen :fill +magenta+)))
  (background +blue+)
  (let ((a (max 1 (ceiling (sqrt n)))))
    (with-fit ((* 10 a) (* 10 a) width height)
      (dotimes (i n)
        (multiple-value-bind (k l)
            (floor i a)
          (with-pen pen
            (circle (+ 5 (* 10 l))
                    (+ 5 (* 10 k))
                    3)))))))

(make-instance 'counter :resizable t)

(defmethod kit.sdl2:keyboard-event ((app counter) (state (eql :keydown)) ts (rep (eql nil)) keysym)
  (incf (counter-n app))
  (when (>= (counter-n app) 10)
    (setf (counter-n app) 0)))
