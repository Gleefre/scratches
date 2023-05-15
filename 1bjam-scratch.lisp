(use-package (ql:quickload :sketch))

(defstruct mover
  (x 200) (y 200) (angle 0))

(defparameter *mover* (make-instance 'mover))
(defparameter *moving?* nil)
(defparameter *dir* 2.5)
(defparameter *last-press* (- (get-internal-real-time) internal-time-units-per-second))

(defun draw-mover (w h)
  (with-slots (x y angle) *mover*
    (setf x (mod x w) y (mod y h))
    (when *moving?*
      (incf x (* 3 (cos (radians angle))))
      (incf y (* 3 (sin (radians angle)))))
    (incf angle *dir*)
    (translate x y)
    (loop for dx in (list (- w) 0 w)
          do (loop for dy in (list (- h) 0 h)
                   do (with-current-matrix
                        (translate dx dy)
                        (rotate angle)
                        (with-pen (make-pen :fill (lerp-color +cyan+ +magenta+ 2/3))
                          (circle 0 0 30))
                        (with-pen (make-pen :stroke +cyan+ :weight (if *moving?* 20 5))
                          (line 0 0 50 0)))))))

(defsketch 1b ()
  (background +black+)
  (draw-mover width height))

(defun start ()
  (make-instance '1b :resizable t))

(defun time-from (x)
  (/ (- (get-internal-real-time) x) internal-time-units-per-second))

(defmethod kit.sdl2:keyboard-event ((app 1b) st ts rep? keysym)
  (when (and (not rep?)
             (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-space))
    (when (eq st :keydown)
      (when (< (time-from *last-press*) 0.3)
        (setf *dir* (- *dir*)))
      (setf *last-press* (get-internal-real-time)))
    (setf *moving?* (eq st :keydown))))

(start)
