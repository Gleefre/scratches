(use-package (ql:quickload :sketch))
(ql:quickload :easing)

(defparameter *state* 0)
(defparameter *last-press* 0)
(defparameter *animation-time* 1/2)

(defun time-from (x)
  (/ (- (get-internal-real-time) x) internal-time-units-per-second))

(defun fit (width height from-width from-height &optional (to-x 0) (to-y 0) (from-x 0) (from-y 0) max-scale)
  (translate from-x from-y)
  (let* ((scale (min (/ from-width width)
                     (/ from-height height)
                     (if max-scale max-scale 10000)))
         (x-shift (/ (- from-width (* width scale)) 2))
         (y-shift (/ (- from-height (* height scale)) 2)))
    (translate x-shift y-shift)
    (scale scale))
  (translate (- to-x) (- to-y)))

(defun draw-button ()
  (with-current-matrix
    (translate 150 150)
    (with-pen (make-pen :fill +blue+)
      (loop for i in '(-50 50)
            do (loop for j in '(-50 50)
                     do (circle i j 10))))
    (let ((progress (easing:in-bounce
                     (- 1 (/ (time-from *last-press*) *animation-time*)))))
      (rotate (* 90 (mod *state* 4)))
      (translate -50 -50)
      (rotate (* 90 progress))
      (with-pen (make-pen :stroke +red+ :weight 4)
        (line 5 0 95 0)))))

(defsketch button ((copy-pixels t))
  (with-pen (make-pen :fill (rgb 0 0 1/2 1/10))
    (rect 0 0 width height))
  (fit 300 300 width height)
  (draw-button))

(defmethod kit.sdl2:keyboard-event ((app button) (st (eql :keydown)) ts (rep? (eql t)) keysym)
  (when (and (eq (sdl2:scancode keysym) :scancode-space)
             (> (time-from *last-press*) *animation-time*))
    (setf *last-press* (get-internal-real-time))
    (incf *state*)))

(make-instance 'button :resizable t)
