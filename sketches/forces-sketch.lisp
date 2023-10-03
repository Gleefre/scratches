;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with C-x C-f and enter text in its buffer.

(ql:quickload :sketch)
(ql:quickload :3d-vectors)

(nick #:v #:3d-vectors)

(defparameter *coeff* 30000)

(defun force (a b &optional (coeff *coeff*))
  (let* ((ab (v:v- b a))
         (r (v:v2norm ab)))
    (if (plusp (- r 20))
        (v:v* (v:v/ ab (expt r 3))
              coeff)
        (v:vec 0 0))))

(defun draw-vec (x y vec)
  (sketch:line x y (+ x (v:vx vec)) (+ y (v:vy vec))))

(defun draw-forces (width height step x y &optional (coeff *coeff*))
  (let ((point (v:vec x y)))
    (loop for x from 0 to width by step
          do (loop for y from 0 to height by step
                   do (draw-vec x y (force point (v:vec x y) coeff))))))

(defparameter *step* 33)

(sketch:defsketch forces ((x 0)
                          (y 0))
  (sketch:background sketch:+black+)
  (sketch:with-pen (sketch:make-pen :stroke sketch:+blue+ :weight 1)
    (draw-forces sketch:width sketch:height *step* x y)))

(make-instance 'forces :resizable t)

(defmethod kit.sdl2:mousemotion-event ((app forces) ts but x y xrel yrel)
  (setf (forces-x app) x
        (forces-y app) y))
