(ql:quickload :sketch)
(ql:quickload :alexandria)
(use-package :sketch)

(defun random-between (a b)
  (+ a (random (- b a -1))))

(defun fit (w h app)
  (with-slots (width height) app
    (let* ((scale (min (/ width w)
                       (/ height h)))
           (x-shift (/ (- width (* w scale)) 2))
           (y-shift (/ (- height (* h scale)) 2)))
      (translate x-shift y-shift)
      (scale scale))))

(defsketch game ((entered nil)
                 (x 0) (y 0))
  (fit 100 150 sketch::*sketch*)
  (rect 0 0 100 150)
  (when entered
    (circle 20 20 40)))

(let ((o *standard-output*))
  (defmethod kit.sdl2:window-event ((app game) type ts data1 data2)
    (format o "~@{~a~%~}~%~%" type ts data1 data2)
    (with-slots (entered x y) app
      (case type
        (:enter (setf entered t))
        (:leave (setf entered nil))))))

(defparameter *a* (make-instance 'game :resizable t))

(defsketch okno ((x 0) (y 0) (data (loop repeat 100 collect (cons (random 1500) (random 1000)))))
  (translate (- x) (- y))
  (loop for (x . y) in data
        do (circle x y 5)))

(defmethod kit.sdl2:window-event :before ((app okno) type ts data1 data2)
  (with-slots (x y) app
    (case type
      (:moved (setf x data1 y data2) (kit.sdl2:render app)))))

(setf (kit.sdl2:idle-render (make-instance 'okno)) nil)
