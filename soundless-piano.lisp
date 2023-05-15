(ql:quickload :sketch)
(use-package :sketch)

(defparameter *unit* 60)
(defparameter *keys* "q2w3er5t6y7ui9o0p[=]azsxcfvgbnjmk,l./'")
(defparameter *char-keycode*
  '((#\q . 20) (#\2 . 31) (#\w . 26) (#\3 . 32) (#\e . 8) (#\r . 21) (#\5 . 34)
    (#\t . 23) (#\6 . 35) (#\y . 28) (#\7 . 36) (#\u . 24) (#\i . 12) (#\9 . 38)
    (#\o . 18) (#\0 . 39) (#\p . 19) (#\[ . 47) (#\= . 46) (#\] . 48) (#\a . 4)
    (#\z . 29) (#\s . 22) (#\x . 27) (#\c . 6) (#\f . 9) (#\v . 25) (#\g . 10)
    (#\b . 5) (#\n . 17) (#\j . 13) (#\m . 16) (#\k . 14) (#\, . 54) (#\l . 15)
    (#\. . 55) (#\/ . 56) (#\' . 52)))

(defsketch key-piano ((width (* 12 *unit*))
                      (height (* 6 *unit*))
                      (pressed (make-hash-table :size 50)))
  (loop for char across *keys*
        for i from 1
        for black = (member (mod i 12)
                            '(2 4 7 9 11))
        for x = 0 then (if (equal char #\a) (- (/ *unit* 4))
                           (if black x
                               (+ *unit* x)))
        for y = 0 then (if (equal char #\a) (* 2 *unit*) y)
        do (let ((color (if black +black+ +white+))
                 (x (if black (+ x (/ *unit* 2)) x))
                 (y (if black y (+ y *unit*))))
             (when (gethash (cdr (assoc char *char-keycode*)) pressed)
               (setf color (gray (if black 0.2 0.8))))
             (with-pen (make-pen :fill color)
               (rect (+ 2 x) (+ 2 y) (- *unit* 4) (- *unit* 4)))
             (with-font (make-font :color (if (= i 13) +blue+ +red+) :size (/ *unit* 2) :align :left)
               (text (format nil "~a" char) (+ (/ *unit* 4) x) (+ (/ *unit* 4) y)))))
  (with-pen (make-pen :fill +yellow+)
    (rect (* *unit* 3) (+ 2 (* *unit* 4))
          (* *unit* 6) (- (* 2 *unit*) 4))))

(defmethod kit.sdl2:keyboard-event ((app key-piano) state ts repeat? keysym)
  (setf (gethash (sdl2:scancode-value keysym) (slot-value app 'pressed))
        (eq state :keydown)))

(make-instance 'key-piano)
