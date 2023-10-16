(defpackage #:%mb (:use #:cl))
(in-package #:%mb)

(use-package (ql:quickload :sketch))
(use-package (ql:quickload :sketch-fit))

(defun draw-pat-lines (c steps scale)
  (loop for z = c then (+ (* z z) c)
        and prev-z = 0 then z
        repeat steps
        while (> (* 2 scale) (max (abs (realpart z)) (abs (imagpart z))))
        do (line (* scale (realpart prev-z)) (* scale (imagpart prev-z))
                 (* scale (realpart z)) (* scale (imagpart z)))))

(defun draw-pat-points (c steps scale)
  (loop for z = 0 then (+ (* z z) c)
        repeat (1+ steps)
        while (> (* 2 scale) (max (abs (realpart z)) (abs (imagpart z))))
        do (circle (* scale (realpart z)) (* scale (imagpart z)) 4)))

(defun z-from-mouse (width height x y scale)
  (* 1.0
     (/ scale (min width height))
     (complex (- x (/ width 2))
              (- y (/ height 2)))))

(defun rate-of (x)
  (tanh (/ x 100)))

(declaim (inline rate-of-change))
(defun rate-of-change (c z* &optional (max-steps 64) &aux (max-abs (expt 2 32)))
  (loop for step from 1
        for z = z* then (+ (* z z) c)
        and prev-z = 0 then z
        when (> (abs z) max-abs) do (return (- 1 (/ step max-steps)))
        when (> step max-steps) do (return 0)))

(ql:quickload :lparallel)

(setf lparallel:*kernel* (lparallel:make-kernel 16))

(defun make-mad (c* min-x min-y max-x max-y width height wh-arr)
  (let ((c (make-canvas width height))
        (width (float width))
        (height (float height)))
    (flet ((draw (xy &aux (x (car xy)) (y (cdr xy)))
             (declare (optimize (speed 3)))
             (declare (type single-float width height min-x min-y max-x max-y)
                      (type fixnum x y))
             (let ((z* (complex (alexandria:lerp (/ (+ 0.5 x) width) min-x max-x)
                                (alexandria:lerp (/ (+ 0.5 y) height) min-y max-y))))
               (canvas-paint c (lerp-color +black+ +blue+ (rate-of-change c* z*))
                             x y))))
      #+(or)
      (dotimes (x (floor width))
        (dotimes (y (floor height))
          (draw (cons x y))))
      #-(or)
      (lparallel:pmap NIL #'draw wh-arr))
    c))

(progn
  (defparameter *w* 900)
  (defparameter *h* 1200)
  (defparameter *wh-arr* (make-array (* *w* *h*) :element-type 'list :initial-element NIL))
  (let ((i -1))
    (dotimes (x *w*)
      (dotimes (y *h*)
        (setf (aref *wh-arr* (incf i))
              (cons x y)))))

  (defsketch z^2+c ((mouse-x 200) (mouse-y 200) (steps 10) (scale-rate 5)
                    (canv nil) (gtime nil) (redraw? t))
    (let ((scale (/ (min width height) scale-rate))
          (c* (z-from-mouse width height mouse-x mouse-y scale-rate)))
      (unless (and canv (not redraw?))
        (let ((start (get-internal-real-time))
              (c (make-mad c*
                           (float (/ width -2 scale))
                           (float (/ height -2 scale))
                           (float (/ width 2 scale))
                           (float (/ height 2 scale))
                           *w* *h* *wh-arr*))
              (end (get-internal-real-time)))
          (canvas-lock c)
          (setf canv c                  ;redraw? nil
                gtime (/ (- end start) internal-time-units-per-second))))
      (with-pen (make-pen :fill (canvas-image canv))
        (rect 0 0 width height))
      (with-font (make-font :color +white+)
        (text (format nil "steps: ~a~%mouse: (~a, ~a)~%scale rate: ~a~%scale: ~a~%gtime: ~2,2f"
                      steps mouse-x mouse-y scale-rate scale gtime)
              0 0))
      (translate (/ width 2) (/ height 2))
      (with-pen (make-pen :fill +cyan+)
        (draw-pat-points c* steps scale))
      (with-pen (make-pen :stroke +magenta+)
        (circle 0 0 scale)
        (draw-pat-lines c* steps scale)))))

(defun run (&rest args)
  (apply #'make-instance 'z^2+c :resizable t args))

(defmethod kit.sdl2:mousemotion-event ((app z^2+c) ts buttons x y xrel yrel)
  (with-slots (mouse-x mouse-y) app
    (setf mouse-x x
          mouse-y y)))

(defmethod kit.sdl2:keyboard-event ((app z^2+c) st ts rep keysym)
  (when (and (eq st :keydown))
    (with-slots (scale-rate steps redraw?) app
      (case (sdl2:scancode keysym)
        (:scancode-z (incf scale-rate))
        (:scancode-x (decf scale-rate))
        (:scancode-q (incf steps))
        (:scancode-e (decf steps))
        (:scancode-r (setf redraw? t))))))

(run)
