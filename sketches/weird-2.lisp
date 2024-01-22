;; hacking up a sketch with sketch

(ql:quickload :sketch)
(ql:quickload :stopclock) ;; my library for clocks

;; define working package
(defpackage #:weird-sketch
  (:use #:cl)
  (:local-nicknames (#:s #:sketch)
                    (#:sc #:stopclock)))

(in-package #:weird-sketch)

;; If you saw that - yes, x & y variables were reset.
;; Let's move all drawing into a funciton
;; so that variables of the sketch don't get reset.

(defun draw (x y w h clock)
  ;; width and height can be retrieved
  ;; from s:width and s:height variables

  ;; Let's change background
  (s:background (s:lerp-color s:+black+ s:+blue+ 0.8))
  ;; I like this color.

  ;; let's make font bigger
  (s:with-font (s:make-font :size 40)
    (s:text (format nil "~Ax~A" w h)
            ;; we need to pass them
            10 10))

  ;; Lets add a circle
  (s:with-pen (s:make-pen :stroke s:+magenta+
                          :weight 10)
    ;; You can see how sketch caught the error
    ;; You could click on it to debug,
    ;; but it breaks the game loop sometimes :(

    ;; To animate the circle let's add
    ;; a scale transform befor drawing it.
    (let ((dt (* 0.3
                 (abs (- (mod (sc:time clock)
                              2)
                         1)))
              ))
      (s:with-current-matrix
                                        ; so that it doesn't affect later code if any
        (s:scale (+ 1 dt) 1
                 ;; only scale by x
                 ;; center of the scaling

                 ;; Yay!
                 
                 x y)
        (s:circle x y 30))))

  ;; Lets add some movement.
  ;; We'll store x & y coordinates first.

  ;; Let's add an animation.
  ;; We'll need to introduce an internal clock
  )

(s:defsketch weird ((x 100)
                    (y 100)
                    (clock (sc:make-clock)))
  (draw x y s:width s:height clock))

;; Now lets handle input
(defmethod kit.sdl2:keyboard-event ((w weird)
                                    state
                                    ts
                                    rep?
                                    keysym)
  ;; Move on pressing a key
  (when (eq state :keydown)
    (case (sdl2:scancode keysym)
      ;; dispatch on the key we pressed
      ((:scancode-w :scancode-up)
       (decf (weird-y w) 10))
      ((:scancode-a :scancode-down)
       (incf (weird-y w) 10))
      ))

  ;; It works!
  )

(make-instance 'weird :resizable t) ; we want to resize it
