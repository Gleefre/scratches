;; hacking up a sketch with sketch

(ql:quickload :sketch)
(ql:quickload :stopclock)

;; defining working package
(defpackage #:ws
  (:use #:cl)
  (:local-nicknames (#:s #:sketch)
                    (#:sc #:stopclock)))

(in-package #:ws)

;; Adding a clock

;; move stuff to a separate function
;; so that internal clock is not reset on update
(Defun draw (clock w h)
  ;; Lets scale our sketch to fit into w x h
  ;; window
  ;; w -> 200
  ;; scale factor of Hm... 200/w
  ;; weird
  (s:scale (/ w 200))
  ;; I calculated something wrong initially.
  ;; Dunno what exactly, but it works!
  
  ;; colors
  (s:background s:+black+)
  ;; rectangle
  (s:with-pen (s:make-pen :fill s:+green+)
    (s:rect 100 100 200 10))
  ;; circle
  (let* ((dx* (mod (* 40 (sc:time clock)) 400))
         (dx (abs (- dx* 200))))
    (s:with-pen (s:make-pen :stroke s:+magenta+)
      (s:circle (+ dx 100) 150 20))))

(s:defsketch weird ((clock (sc:make-clock)))
  ;; s:width and s:height are current
  ;; width and height of the sketch
  (draw clock s:width s:height))

;; Adding key reactions

(defmethod kit.sdl2:keyboard-event
  ((app weird) state ts rep? keysym)
  (when (eq state :keydown); when key is pressed
    (when (not rep?) ; and it is first press
      (sc:accelerate (weird-clock app)
                     1.05)
      ;; Make it faster
      )))

;; Yay, works!

(make-instance 'weird :resizable t)
