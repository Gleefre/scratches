(use-package (ql:quickload :sketch))

(defsketch bob-1 ((copy-pixels t))
  (point 0 0))

(defsketch bob-2 ((copy-pixels t)))

(defsketch bob-3 ((width 100)))

(make-instance 'bob-1 :width 200 :height 500)

(defmethod setup ((app bob-1) &key &allow-other-keys)
  (background +white+))

(defmethod setup ((app bob-2) &key &allow-other-keys)
  (background +white+))

(defsketch name (let-like-binding?
                 (like-this?)
                 (or-only-this? nil)))

(make-instance 'bob-1 :w 400 :h 400)
(make-instance 'bob-2 :w 400 :h 400)
(make-instance 'bob-3 :w 400 :h 400)

(let ((sketch::*default-height* 1000))
  (make-instance 'bob-3))

#+repl
(sdl2:in-main-thread ()
  (setf *trace-output* #.*trace-output*
        *standard-output* #.*standard-output*))


(defclass closable (kit.sdl2:gl-window) ())

(defmethod kit.sdl2:keyboard-event ((window closable) state ts rep? keysym)
  (print (list state ts rep? (sdl2:scancode keysym)))
  (when (and (eq state :keyup)
             (eq (sdl2:scancode keysym)
                 :scancode-escape))
    (kit.sdl2:close-window window)))

(make-instance 'closable)

