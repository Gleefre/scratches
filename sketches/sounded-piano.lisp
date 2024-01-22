(ql:quickload :sketch)
(ql:quickload '(harmony cl-mixed-mpg123
                #+linux cl-mixed-pulse
                #+darwin cl-mixed-coreaudio
                #+windows cl-mixed-wasapi
                #+bsd cl-mixed-oss))

(use-package :sketch)

(defparameter *unit* 100)
(defparameter *keys* "q2w3er5t6y7ui9o0p[=]azsxcfvgbnjmk,l./'")
(defparameter *char-keycode*
  '((#\q . 20) (#\2 . 31) (#\w . 26) (#\3 . 32) (#\e . 8) (#\r . 21) (#\5 . 34)
    (#\t . 23) (#\6 . 35) (#\y . 28) (#\7 . 36) (#\u . 24) (#\i . 12) (#\9 . 38)
    (#\o . 18) (#\0 . 39) (#\p . 19) (#\[ . 47) (#\= . 46) (#\] . 48) (#\a . 4)
    (#\z . 29) (#\s . 22) (#\x . 27) (#\c . 6) (#\f . 9) (#\v . 25) (#\g . 10)
    (#\b . 5) (#\n . 17) (#\j . 13) (#\m . 16) (#\k . 14) (#\, . 54) (#\l . 15)
    (#\. . 55) (#\/ . 56) (#\' . 52)))

(defparameter *note-shift* -20)
(defparameter *notes-folder* "/home/grolter/mydata/good-root/projects/lisp/lispiano/piano_c4/")

(defun note-filename (note-index)
  (format nil "~a~a.wav"
          *notes-folder*
          (+ *note-shift*
             note-index)))

(defun make-note (note-index)
  (let* ((filename (note-filename note-index))
         (path (pathname filename)))
    (org.shirakumo.fraf.harmony:create path)))

(defun play-note (note)
  (org.shirakumo.fraf.harmony:play note :reset t))

(defun stop-note (note)
  (org.shirakumo.fraf.harmony:stop note))

(defun make-notes ()
  (loop for (char . keycode) in *char-keycode*
        for i from 0
        collect (cons keycode (make-note i))))

;;; App

(defsketch key-piano ((width (* 12 *unit*))
                      (height (* 6 *unit*))
                      (pressed (make-hash-table :size 50))
                      (pressed-notes ())
                      (notes (make-notes)))
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
  (with-pen (make-pen :fill (gray 0.9))
    (rect (* *unit* 3) (+ 2 (* *unit* 4))
          (* *unit* 6) (- (* 2 *unit*) 4)))
  (with-font (make-font :color (gray 0.1) :size (/ *unit* 4) :align :left)
    (text (format nil "~a" pressed-notes)
          (+ 5 (* *unit* 3))
          (+ 5 (* *unit* 4)))))

(defmethod kit.sdl2:keyboard-event ((app key-piano) state ts repeat? keysym)
  (unless repeat?
    (let ((note (cdr (assoc (sdl2:scancode-value keysym) (slot-value app 'notes)))))
      (when note
        (if (eq state :keydown)
            (play-note note)
            (stop-note note)))))
  (setf (gethash (sdl2:scancode-value keysym) (slot-value app 'pressed))
        (eq state :keydown))
  (let ((note (sdl2:scancode keysym)))
    (if (eq state :keydown)
        (pushnew note (slot-value app 'pressed-notes))
        (setf (slot-value app 'pressed-notes)
              (remove note (slot-value app 'pressed-notes) :test #'equal))))
  (unless (eq (sdl2:scancode keysym) :scancode-escape)
    (kit.sdl2:render app)))

(defmethod kit.sdl2:close-window :before ((app key-piano))
  (loop for (_ . note) in (slot-value app 'notes)
        do (org.shirakumo.fraf.mixed:free note)))

(progn
  (org.shirakumo.fraf.harmony:maybe-start-simple-server)
  (let ((app (make-instance 'key-piano)))
    (setf (kit.sdl2:idle-render app) nil)
    (kit.sdl2:render app)))
