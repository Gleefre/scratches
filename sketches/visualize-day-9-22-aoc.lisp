(use-package (ql:quickload '(:sketch :sketch-fit)))

(defun day-9+ ()
  (let ((visited? (make-hash-table :test 'equal))
        (rope (loop repeat 10 collect (list 0 0))))
    (setf (gethash (car (last rope)) visited?) t)
    (loop for (dir times) in (aoc/2022::read-day-9)
          append (loop repeat times
                       do (setf rope (aoc/2022::move-rope rope dir)
                                (gethash (car (last rope)) visited?) t)
                       collect rope))))

(aoc/interface:with-test-case (:test)
  (defparameter *a* (day-9+)))

(defparameter *dx* (- (apply #'min (mapcan (lambda (head) (mapcar #'first head)) *a*))))
(defparameter *dy* (- (apply #'min (mapcan (lambda (head) (mapcar #'second head)) *a*))))

(defparameter *sx* (1+ (+ *dx* (apply #'max (mapcan (lambda (head) (mapcar #'first head)) *a*)))))
(defparameter *sy* (1+ (+ *dy* (apply #'max (mapcan (lambda (head) (mapcar #'second head)) *a*)))))

(defsketch visualize ((ropes *a*)
                      (current-rope 0))
  (background +black+)
  (with-font (make-font :color +white+)
    (text (format nil "CR: ~a \ ~a" current-rope (length *a*))
          (/ width 2) 0))
  (fit (* *sx* 10) (* *sy* 10) width height)
  (setf current-rope (max current-rope 0)
        current-rope (min current-rope (1- (length *a*))))
  (dotimes (x *sx*)
    (dotimes (y *sy*)
      (with-pen (make-pen :stroke +cyan+ :weight 1/2)
        (rect (+ 1 (* 10 x)) (+ 1 (* 10 y)) 8 8))))
  (translate (* *dx* 10) (* *dy* 10))
  (loop for rp in ropes
        for (x y) = (car (last rp))
        do (with-pen (make-pen :fill (gray 0.5 0.5))
             (circle (+ 5 (* 10 x)) (+ 5 (* 10 y)) 3)))
  (loop for ((x y) (a b)) on (nth current-rope ropes)
        for i from 0
        while a
        do (with-pen (make-pen :stroke (lerp-color +magenta+ +cyan+ 1/3))
             (line (+ 5 (* x 10)) (+ 5 (* y 10)) (+ 5 (* a 10)) (+ 5 (* b 10))))
        do (with-pen (make-pen :fill (lerp-color +magenta+ +cyan+ (/ i 8)))
             (circle (+ 5 (* x 10)) (+ 5 (* y 10)) 2))
        do (with-pen (make-pen :fill (lerp-color +magenta+ +cyan+ (/ (1+ i) 8)))
             (circle (+ 5 (* a 10)) (+ 5 (* b 10)) 2))))

(defmethod kit.sdl2:keyboard-event ((app visualize) st ts rep keysym)
  (when (eq st :keydown)
    (with-slots (current-rope) app
      (case (sdl2:scancode keysym)
        (:scancode-1 (incf current-rope))
        (:scancode-2 (decf current-rope))
        (:scancode-q (incf current-rope 10))
        (:scancode-w (decf current-rope 10))
        (:scancode-a (incf current-rope 100))
        (:scancode-s (decf current-rope 100))
        (:scancode-z (incf current-rope 500))
        (:scancode-x (decf current-rope 500))))))

(make-instance 'visualize)
