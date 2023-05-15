(require '#:sketch)
(use-package '#:sketch)

(defparameter *width* 100)
(defparameter *height* 100)
(defparameter *max-lifetime* 20)
(defparameter *x-move-range* 3)
(defparameter *max-vy* 3)
(defparameter *max-hist* 3)

(defun random-between (x y)
     (+ x (random (1+ (- y x)))))

(defclass fire-particle ()
  ((x :initform 200 :accessor x :initarg :x)
   (y :initform 0 :accessor y :initarg :y)
   (history :initform nil :accessor hist)
   (vy :initform (random-between 1 *max-vy*) :initarg vy)
   (lifetime :initform 0 :accessor lifetime)
   (x-shift :initform 0 :initarg :x-shift)))

(defun fire-color (fire-particle)
  (let ((l (/ (lifetime fire-particle) *max-lifetime*)))
    #-t (if (< l 2/3)
            (lerp-color +red+ +yellow+ (* l 3/2))
            (lerp-color +yellow+ +black+ (- (* l 3) 2)))
    #+t (lerp-color +red+ +blue+ l)))

(defmethod animate ((fire fire-particle))
  (with-slots (x y lifetime vy x-shift history) fire
    (push (list x y) history)
    (when (> (length history) *max-hist*)
      (setf history (subseq history 0 *max-hist*)))
    (incf lifetime 1)
    (incf y (random-between 1 vy))
    (let* ((k (floor (* *x-move-range*
                        (/ (1+ lifetime) *max-lifetime*))))
           (x-step (+ x-shift (random-between (- k) k))))
      (incf x x-step)
      (setf x-shift (- (floor x-step 3))))))

(defun m-polyline (&rest coords)
  (loop for c = coords then (cddr c)
        while (>= (length c) 4)
        do (apply #'line (subseq c 0 4))))

(defmethod draw-particle ((p fire-particle))
  (with-pen (make-pen :fill (fire-color p) :weight 2/3)
    #+t (rect (x p) (y p) (/ (- (y p) 30) 2) 1)
    #+t (circle (x p) (y p) (if (< (/ (lifetime p) *max-lifetime*) 1/2) 2 1))
    #-t (ngon 6 (x p) (y p) (if (< (/ (lifetime p) *max-lifetime*) 1/2) 2 1)
              (if (< (/ (lifetime p) *max-lifetime*) 1/2) 2 1)))
  (with-pen (make-pen :stroke (fire-color p))
    #+t (apply #'m-polyline (x p) (y p) (loop for e in (hist p)
                                              append e)))
  (with-pen (make-pen :fill (rgb 1 1 1 1/100))
    #+t (circle (x p) (y p) 10 #+t (floor (* 20 (/ (lifetime p) *max-lifetime*))))
    #+t (rect (- (x p) 10) (- (y p) 10) 20 20)
    (ngon 8 (x p) (+ 5 (y p)) 15 10)))

(defun draw-scene (particles)
  (background +black+)
  (with-pen (make-pen :fill (rgb 0 1/2 0))
    (rect 0 0 100 20))
  (with-pen (make-pen :stroke +blue+ :weight 1)
    (rect 1 1 98 98))

  (dolist (p particles)
    (draw-particle p))

  (with-pen (make-pen :fill (lerp-color +magenta+ +white+ 4/5))
    (polygon 50 0 35 30 37 35 63 35 66 30)))

(defsketch fire ((particles ())
                 (width 800) (height 800)
                 (title "Torch")
                 (time 0)
                 (y-axis t)
                 (scale (min (/ (- width 5) *width*)
                             (/ (- height 5) *height*)))
                 (x-shift (/ (- width (* *width* scale)) 2))
                 (y-shift (/ (- height (* *height* scale)) 2)))
  (incf time)
  (when (= (mod time 3) 0)
    (dolist (p particles)
      (animate p))
    (dotimes (_ 20)
      (push (make-instance 'fire-particle :x (+ 50 (random-between -10 10))
                                          :y 30)
            particles)))
  (setf particles (remove-if (lambda (p) (> (lifetime p) *max-lifetime*))
                             particles))

  (translate x-shift y-shift)
  (scale scale)

  (draw-scene particles))

(make-instance 'fire)
