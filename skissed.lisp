(use-package (ql:quickload :sketch))
(ql:quickload :easing)

(defstruct mover (x 0) (y 0))

(defstruct apple (x 200) (y 200))

(defun time-from (x &optional (divisor 1))
  (if x
      (/ (- (get-internal-real-time) x)
         internal-time-units-per-second
         divisor)
      1))

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

(defparameter *score* 0)
(defparameter *mover* (make-instance 'mover))
(defparameter *speed* 1)
(defparameter *moving* 1)
(defparameter *rotation-period* 10)

(defparameter *last-press* (- (get-internal-real-time) internal-time-units-per-second))
(defparameter *last-angle* 0)
(defparameter *dir* 1)

(defparameter *apple* (make-instance 'apple))

(defun eat-apple ()
  (with-slots (x y) *apple*
    (incf x (+ 150 (random 100)))
    (incf y (+ 150 (random 100)))
    (setf x (mod x 400) x (min x 380) x (max x 20)
          y (mod y 400) y (min y 380) y (max y 20))
    (setf *last-apple* (get-internal-real-time)))
  (incf *score*))

(defun collide? ()
  (< (sqrt (+ (expt (- (apple-x *apple*)
                       (mover-x *mover*))
                    2)
              (expt (- (apple-y *apple*)
                       (mover-y *mover*))
                    2)))
     20))

(defparameter *last-apple* nil)

(defun draw-apple ()
  (when (collide?) (eat-apple))
  (with-pen (make-pen)
    (image (load-resource "/home/grolter/mydata/arts/apple-240.png")
           (- (apple-x *apple*) 8) (- (apple-y *apple*) 8)
           16 16)))

(defun current-angle ()
  (+ *last-angle*
     (* *moving* *dir* 360 (time-from *last-press* *rotation-period*))))

(defun draw-mover (w h)
  (gl:enable :scissor-test)
  (sciss 0 0 w h)
  (with-pen (make-pen :fill (gray 0.5 0.3))
    (rect 0 0 w h))
  (with-slots (x y) *mover*
    (let ((angle (current-angle)))
      (let ((*speed* (* *speed* (- 2 (easing:in-circ (time-from *last-apple* 3))))))
        (incf x (* *speed* *moving* (cos (radians angle))))
        (incf y (* *speed* *moving* (sin (radians angle)))))
      (setf x (mod x w) y (mod y h))
      (translate x y)
      (loop for dx in (list (- w) 0 w)
            do (loop for dy in (list (- h) 0 h)
                     do (with-current-matrix
                          (translate dx dy)
                          (rotate angle)
                          (with-pen (make-pen)
                            (rotate 90)
                            (image (load-resource "/home/grolter/mydata/arts/turtlish-200.png")
                                   -20 -20 40 40)))))))
  (gl:disable :scissor-test))

(defun sciss (x y w h)
  (destructuring-bind ((x1 y1) (x2 y2))
      (list
       (sketch::transform-vertex (list x (+ y h)) (sketch::env-model-matrix sketch::*env*))
       (sketch::transform-vertex (list (+ x w) y) (sketch::env-model-matrix sketch::*env*)))
    (let* ((height (sketch::sketch-height sketch::*sketch*))
           (y1 (- height y1))
           (y2 (- height y2)))
      (gl:scissor x1 y1 (- x2 x1) (- y2 y1)))))

(defun draw-pause ()
  (when (zerop *moving*)
    (with-pen (make-pen :fill (gray 0.0 (min 0.5 (easing:in-exp (time-from *last-double-click* 1/2)))))
      (rect 0 0 440 480))
    (with-current-matrix
      (translate 220 260)
      (scale (easing:in-out-back (time-from *last-double-click* 1)))
      (rotate (* 360 (easing:in-out-back (time-from *last-double-click* 1))))
      (with-pen (make-pen :fill (gray 0.9 0.7))
        (rect 50 -100 50 200)
        (rect -100 -100 50 200)))))

(defsketch 1b ((title "WIP - no idea yet"))
  (background +black+)
  (fit 440 480 width height)
  (draw-stars)
  (with-font (make-font :color (gray 0.6) :align :center :size 40)
    (text (format nil "Score: ~a" *score*) 220 0))
  (with-current-matrix (translate 20 60)
    (with-pen (make-pen :stroke (gray 0.4) :weight 4)
      (rect 0 0 400 400))
    (draw-apple)
    (draw-mover 400 400))
  (draw-pause))

(defun start ()
  (make-instance '1b :resizable t))

(defun hold-down (app)
  (setf *last-angle* (current-angle))
  (setf *last-press* (get-internal-real-time))
  (setf *dir* -1))

(defun hold-up (app)
  (setf *last-angle* (current-angle))
  (setf *last-press* (get-internal-real-time))
  (setf *dir* 1))

(defun double-click (app)
  (setf *last-angle* (current-angle))
  (setf *last-double-click* (get-internal-real-time))
  (setf *last-press* (get-internal-real-time))
  (setf *moving* (- 1 *moving*)))

(defparameter *last-click* (- (get-internal-real-time) internal-time-units-per-second))
(defparameter *last-double-click* (- (get-internal-real-time) internal-time-units-per-second))

(defmethod kit.sdl2:keyboard-event ((app 1b) st ts rep? keysym)
  (when (and (not rep?)
             (eq (sdl2:scancode keysym) :scancode-space))
    (when (eq st :keydown)
      (when (< (time-from *last-click* 1/3) 1)
        (double-click app))
      (setf *last-click* (get-internal-real-time)))
    (case st
      (:keydown (hold-down app))
      (:keyup (hold-up app)))))

;; from sketch-stars

(defun make-stars ()
  (let ((canvas (make-canvas 100 100)))
    (dotimes (i 20)
      (let ((x (random 100))
            (y (random 100)))
        (unless (and (< 40 x 60)
                     (< 40 y 60)))
        (canvas-paint canvas (if (zerop (random 3)) +magenta+ +cyan+) x y)))
    canvas))

(defun lock-stars (stars)
  (loop for c in stars do (canvas-lock c)))

(defun rotate-list (list)
  (let ((el (pop list)))
    (reverse (cons el (reverse list)))))

(defun get-zoom (position)
  (exp (/ position 6)))

(defun get-rotation (position)
  (expt 1.25 position))

(defparameter *stars* (loop :for i :below 5 :collect (make-stars)))
(defparameter *positions* (loop :for i :from 16 :downto 0 :by 4 :collect i))
(defparameter *direction-shift* 0)

(defun draw-stars ()
  (lock-stars *stars*)
  (dotimes (i (length *stars*))
    (unless (zerop *moving*)
      (incf (elt *positions* i) 0.01))
    (let ((zoom (get-zoom (elt *positions* i)))
          (rotation (get-rotation (elt *positions* i)))
          (direction (expt -1 (+ *direction-shift* (mod i 2)))))
      (with-current-matrix
        (with-pen (make-pen :fill (canvas-image (elt *stars* i)))
          (translate 220 260)
          (scale zoom)
          (rotate (* direction rotation))
          (rect -50 -50 100 100)))))
  (when (>= (get-zoom (car *positions*)) 20)
    (setf (car *positions*) 0)
    (setf *positions* (rotate-list *positions*))
    (setf *stars* (rotate-list *stars*))
    (setf *direction-shift* (- 1 *direction-shift*))))

(start)
