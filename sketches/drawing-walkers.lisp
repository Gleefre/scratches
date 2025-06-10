;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with C-x C-f and enter text in its buffer.

(:ql :sketch)

(defpackage #:sketch-user
  (:use #:cl)
  (:local-nicknames (#:s #:sketch)))

(in-package #:sketch-user)

(defun make-walker (w h)
  (list (sky-user::random-between 0d0 (float w 0d0))
        (sky-user::random-between 0d0 (float h 0d0))
        (sky-user::random-between 0 360)
        (sky-user::random-between 5d0 50d0)
        (sky-user::random-between 0d0 360d0)
        (sky-user::random-between 0.2d0 1d0)
        (sky-user::random-between 0.5d0 1d0)
        (sky-user::random-between 0.25d0 1d0)
        (sky-user::random-between 0.1d0 0.9d0)))

(defun move-walker (walker \w \h)
  (destructuring-bind (x y d r h s b a l) walker
    (list (sky-user::clamp (+ x (* r (cos (sky-user::radians d)))) 0d0 (float \w 0d0))
          (sky-user::clamp (+ y (* r (sin (sky-user::radians d)))) 0d0 (float \h 0d0))
          (mod (+ d (sky-user::random-between -150 150)) 360)
          (sky-user::clamp (* r (sky-user::random-between 0.75d0 1.25d0)) 5d0 50d0)
          (mod (+ h (sky-user::random-between -5d0 5d0)) 360d0)
          (sky-user::clamp (+ s (sky-user::random-between -0.02d0 0.02d0)) 0.2d0 1d0)
          (sky-user::clamp (+ b (sky-user::random-between -0.03d0 0.05d0)) 0.5d0 1d0)
          (sky-user::clamp (+ a (sky-user::random-between -0.025d0 0.02d0)) 0.25d0 1d0)
          (sky-user::clamp (+ l (sky-user::random-between -0.02d0 0.02d0)) 0.1d0 0.9d0))))

(defun draw-walker (walker)
  (destructuring-bind (x y d r h s b a l) walker
    (s:with-pen (:fill (s:hsb (/ h 360d0) s b a))
      (s:with-translate (x y)
        (s:with-rotate (d)
          (s:with-scale (l (1- l))
            (s:circle 0 0 r)))))))

(s:defsketch foo ((s:copy-pixels t)
                  (ws (loop repeat 3 collect (make-walker s:width s:height))))
  (setf ws (loop for w in ws
                 for w+ = (move-walker w s:width s:height)
                 do (draw-walker w+)
                 collect w+)))

(defmethod s:setup ((s foo) &key)
  (declare (ignore s))
  (s:background s:+black+))

(make-instance 'foo
               :width 1920
               :height 1200
               :x 0
               :y 0
               :flags '(:borderless))
