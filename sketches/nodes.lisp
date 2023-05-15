#-quicklisp
(let ((quicklisp-init (merge-pathnames
                       "quicklisp/setup.lisp"
                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(ql:quickload :sketch)
(in-package :sketch)

(defun nd-n (node)
  (case node
    ((:tr-up :tr-down) 3)
    ((:sq :sq-l :sq-r) 4)
    (:hexagon          6)))

(defun nd-angle (node)
  (case node
    (:tr-up -90)
    (:tr-down 90)
    (:sq 45)
    (:sq-l 15)
    (:sq-r 75)
    (:hexagon 90)))

(defun nd-scale (node)
  (case node
    ((:sq :sq-l :sq-r) (sqrt 1/2))
    ((:tr-up :tr-down) (sqrt 1/3))
    (:hexagon          1)))

(defun draw-node (node x y side &aux (side (* side (nd-scale node))) (angle (nd-angle node)) (n (nd-n node)))
  (ngon n x y side side angle))

(defun xy-from-hex (hex-x hex-y side index &aux (angle (* (- index 3) (/ pi 6))) (shift-type (mod index 2)))
  (let ((shift (* (nth shift-type (list (+ 1 (sqrt 1/3))
                                        (+ 1/2 (sqrt 3/4))))
                  side)))
    (values (+ hex-x (* shift (cos angle)))
            (+ hex-y (* shift (sin angle))))))

(defparameter +forms+
  '(:tr-down :sq-r :tr-up :sq
    :tr-down :sq-l :tr-up :sq-r
    :tr-down :sq   :tr-up :sq-l))

(defmacro with-translate ((x y) &body body)
  `(progn (push-matrix)
          (translate ,x ,y)
          ,@body
          (pop-matrix)))

(defun draw-hexagon (x y side &optional (d 0))
  (with-translate (x y)
    (loop for i from 0
          for form in +forms+
          for (x y) = (multiple-value-list (xy-from-hex 0 0 side i))
          do (draw-node form x y (- side d)))
    (draw-node :hexagon 0 0 (- side d))))

(defparameter *colors* (list (gray 1/4) (gray 1/2) (gray 3/4)))

(defsketch nodes ((side 40))
  (loop for y below 20
        for hex-y = (* 1/2 y side 1 (+ 3 (sqrt 3)))
        for color-id = (* 2 (mod y 2))
        do (loop for x below 20
                 for hex-x = (* (+ x (if (= 1 (mod y 2)) 1/2 0)) side (+ 1 (sqrt 3)))
                 #+nil (with-pen (make-pen :fill (nth (mod (incf color-id) 3) *colors*)
                                           :stroke (nth (mod (- 2 color-id) 3) *colors*))
                         (draw-hexagon hex-x hex-y side 1))
                 do (draw-hexagon hex-x hex-y side 5))))

;(make-instance 'nodes :width 1920 :height 1080)

(defsketch rotate-node ((time -90)
                        (stop-counter 50)
                        (back-counter 20))
  (if (zerop stop-counter)
      (progn (incf time)
             (decf back-counter)
             (setf back-counter (max back-counter 0))
             (when (zerop (mod time 90))
               (setf stop-counter 25)
               (setf back-counter 20)))
      (decf stop-counter))
  (let ((angle time))
    (with-translate (200 200)
      (with-pen (make-pen :stroke +red+ :weight 5)
        (line -200 0 200 0)
        (line 0 -200 0 200))
      (when (zerop stop-counter)
        (translate 3 -3))
      (rotate angle)
      (with-pen (make-pen :fill (rgb 1 1 1 0.9) :stroke +black+)
        (draw-hexagon 0 0
                      (+ 50 (if (zerop stop-counter)
                                (floor (* (cos (* 1/20 20 pi 1/2)) 3) 0.01)
                                0))
                      (+ 5 (* 3 (sin (- (* time 4/180 pi)
                                        (* pi 1/2)))))))
      (with-pen (make-pen :stroke +blue+ :weight 3)
        (line 0 0 0 50)))))

#+nil
(defmethod kit.sdl2:keyboard-event ((app rotate-node) st ts rep? keysym)
  (incf (slot-value app 'time)))

(make-instance 'rotate-node)
