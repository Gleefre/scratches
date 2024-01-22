;;;; Welcome to Portacle, the Portable Common Lisp Environment.
;; For information on Portacle and how to use it, please read
;;   https://portacle.github.io or *portacle-help*
;; To report problems and to get help with issues,  please visit
;;   https://github.com/portacle/portacle/issues
;; Portacle is currently running SLIME , but you can
;;   Switch to SLY
;; You should also configure Portacle with the
;;   First-time setup
;; 
;; You can use this buffer for notes and tinkering with code.

(ql:quickload :sketch)
(use-package :sketch)

(defsketch images? ((angle 90)
                    (x 0)
                    (y 0))
  (rotate angle 64 64)
  (translate x y)
  (with-pen (make-pen)
    (image (load-resource "/home/grolter/good-root/arts/pics/drawings/little-thing-512.png") 0 0))
  (circle 64 64 4))

(defparameter *last-keysym* nil)

(defparameter *on-key* (make-hash-table))

(setf (gethash :scancode-a *on-key*)
      (lambda (app st ts rep keysym)
        (when (eq st :keydown)
          (with-slots (angle) app
            (incf angle 2)))))

(setf (gethash :scancode-d *on-key*)
      (lambda (app st ts rep keysym)
        (when (eq st :keydown)
          (with-slots (angle) app
            (decf angle 2)))))

(setf (gethash :scancode-w *on-key*)
      (lambda (app st ts rep keysym)
        (when (eq st :keydown)
          (with-slots (y) app
            (incf y)))))

(setf (gethash :scancode-s *on-key*)
      (lambda (app st ts rep keysym)
        (when (eq st :keydown)
          (with-slots (y) app
            (decf y)))))

(let ((output *standard-output*))
  (defmethod kit.sdl2:keyboard-event ((app images?) st ts rep keysym)
    (flet ((void (app st ts rep keysym)
             (print (sdl2:scancode keysym) output)
             (print st output)))
      (funcall (gethash (sdl2:scancode keysym) *on-key* #'void)
               app st ts rep keysym))))

(make-instance 'images?)
