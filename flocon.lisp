(ql:quickload :sketch)
(use-package :sketch)

(defun lerp (a b k)
  (+ (* a (- 1 k)) (* b k)))

(defparameter +k+ (* (rationalize (/ (sqrt 3) 6))
                     #C(0 -1)))

(defun fit (w h app)
  (with-slots (width height) app
    (let* ((scale (min (/ width w)
                       (/ height h)))
           (x-shift (/ (- width (* w scale)) 2))
           (y-shift (/ (- height (* h scale)) 2)))
      (translate x-shift y-shift)
      (scale scale))))

(defun flocon-line (a b depth)
  (if (zerop depth)
      (list a)
      (let ((x (lerp a b 1/3))
            (y (+ (lerp a b 1/2) (* +k+ (- b a))))
            (z (lerp a b 2/3))
            (depth* (1- depth)))
        (append (flocon-line a x depth*)
                (flocon-line x y depth*)
                (flocon-line y z depth*)
                (flocon-line z b depth*)))))

(defun c-coords (coords-complex)
  (loop for c in coords-complex
        collect (floor (realpart c))
        collect (floor (imagpart c))))

(defparameter +c+ 200)

(defun points (d)
  (c-coords (append (flocon-line (complex +c+ +c+) (complex (* 3 +c+) +c+) d)
                    (flocon-line (complex (* 3 +c+) +c+) (complex (* 2 +c+) (* 3 +c+)) d)
                    (flocon-line (complex (* 2 +c+) (* 3 +c+)) (complex +c+ +c+) d))))

(let ((cache (make-hash-table)))
  (defun flocon (d)
    (if (gethash d cache)
        (gethash d cache)
        (setf (gethash d cache) (apply #'sketch::make-polygon (points d))))))

(defsketch flocon ((d 2)
                   (polygon (flocon d)))
  (fit (* 4 +c+) (* 4 +c+) sketch::instance)
  (apply #'sketch::draw-shape polygon)
  (text (format nil "~a" d) (* 2 +c+) (* 2 +c+)))

(defmethod kit.sdl2:mousebutton-event ((app flocon) st ts button x y)
  (when (eq st :mousebuttondown)
    (with-slots (d points polygon) app
      (if (= button 1)
          (incf d)
          (decf d))
      (setf d (max 0 d))
      (setf d (min d 4))
      (setf polygon (flocon d))
      (kit.sdl2:render app))))

(defparameter *app* (make-instance 'flocon :width 1000 :height 1000 :fullscreen nil))
(sleep 1)
(setf (kit.sdl2:idle-render *app*) nil)
