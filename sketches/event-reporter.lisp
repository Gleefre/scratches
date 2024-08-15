(:ql :sketch)

(defpackage #:sketch-user
  (:use #:cl)
  (:local-nicknames (#:s #:sketch)))

(in-package #:sketch-user)

(s:defsketch event-reporter ((r 50))
  (s:with-pen (:fill s:+green+)
    (s:circle 200 200 r))
  (s:circle 200 200 60)
  (decf r 2)
  (when (<= r 50)
    (s:stop-loop)))

(defmethod s:setup ((instance event-reporter) &key &allow-other-keys)
  (defmethod kit.sdl2:other-event :before ((window (eql (slot-value instance 's::%window))) event)
    (print `(other-event ,event))))

(defmethod kit.sdl2:window-event :before ((window event-reporter) type timestamp data1 data2)
  (print `(window-event ,type ,data1 ,data2))
  (setf (event-reporter-r window) 100)
  (let ((s::*sketch* window))
    (s:start-loop)))

(defmethod kit.sdl2:controller-added-event :before ((window event-reporter) c)
  (print `(controller-event :added ,c))
  (setf (event-reporter-r window) 100)
  (let ((s::*sketch* window))
    (s:start-loop)))

(defmethod kit.sdl2:controller-removed-event :before ((window event-reporter) c)
  (print `(controller-event :removed ,c))
  (setf (event-reporter-r window) 100)
  (let ((s::*sketch* window))
    (s:start-loop)))

(defmethod kit.sdl2:controller-axis-motion-event :before ((window event-reporter) c ts axis value)
  (print `(controller-event :axis-motion ,c ,axis ,value))
  (setf (event-reporter-r window) 100)
  (let ((s::*sketch* window))
    (s:start-loop)))

(defmethod kit.sdl2:controller-button-event :before ((window event-reporter) c state ts button)
  (print `(controller-event :button ,c ,state ,button))
  (setf (event-reporter-r window) 100)
  (let ((s::*sketch* window))
    (s:start-loop)))

(defmethod kit.sdl2:mousebutton-event :before ((window event-reporter) state ts button x y)
  (print `(mouse :button ,state ,button ,x ,y))
  (setf (event-reporter-r window) 100)
  (let ((s::*sketch* window))
    (s:start-loop)))

(defmethod kit.sdl2:mousemotion-event :before ((window event-reporter) ts bmask x y xrel yrel)
  (print `(mouse :motion ,bmask ,x ,y that-is ,xrel ,yrel))
  (setf (event-reporter-r window) 100)
  (let ((s::*sketch* window))
    (s:start-loop)))

(defmethod kit.sdl2:mousewheel-event :before ((window event-reporter) ts x y)
  (print `(mouse :wheel ,x ,y))
  (setf (event-reporter-r window) 100)
  (let ((s::*sketch* window))
    (s:start-loop)))

(defmethod kit.sdl2:textinput-event :before ((window event-reporter) ts text)
  (print `(text ,text))
  (setf (event-reporter-r window) 100)
  (let ((s::*sketch* window))
    (s:start-loop)))

(defmethod kit.sdl2:keyboard-event :before ((window event-reporter) state ts rep keysym)
  (print `(keyboard ,state ,rep ,keysym))
  (setf (event-reporter-r window) 100)
  (let ((s::*sketch* window))
    (s:start-loop)))

(make-instance 'event-reporter)
