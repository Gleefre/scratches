(in-package :cl-user)
(use-package (ql:quickload :sketch))
(ql:quickload :cl-raster)

(add-package-local-nickname '#:scene '#:cl-raster/scene)
(add-package-local-nickname '#:vec   '#:3d-vectors)

(defparameter *camera*
  (scene:make-camera :width 400
                     :height 400
                     :center (vec:vec 5 5 0)
                     :direction (vec:vec 0 0 1)
                     :x-vector (vec:vec 1 0 0)
                     :y-vector (vec:vec 0 1 0)))

(defparameter *scene*
  (scene:make-scene :triangles `(((,(vec:vec 0 0 0 )
                                   ,(vec:vec 1 1 -1)
                                   ,(vec:vec 0 -1 -1))
                                  ,+green+)
                                 ((,(vec:vec 0 0 0)
                                   ,(vec:vec 1 1 -1)
                                   ,(vec:vec -1 0 -1))
                                  ,+blue+)
                                 ((,(vec:vec 0 0 0)
                                   ,(vec:vec -1 0 -1)
                                   ,(vec:vec 0 -1 -1))
                                  ,+yellow+)
                                 ((,(vec:vec -1 0 -2/3)
                                   ,(vec:vec 0 -1 -2/3)
                                   ,(vec:vec 1 1 -2/3))
                                  ,+white+))))

(in-package :cl-raster/core)
(defun render/sk (scene camera)
  (let ((image (sketch:make-canvas (scene:camera-width camera) (scene:camera-height camera)))
        (depths (make-array (list (scene:camera-width camera) (scene:camera-height camera))
                            :initial-element :infinity)))
    (loop for (triangle color) in (scene:scene-triangles scene)
          for flat-triangle = (project-triangle-to-camera camera triangle)
          for minimal-x = (max 0 (floor (reduce #'min flat-triangle :key #'d-p-x)))
          for maximal-x = (min (1- (scene:camera-width camera))
                               (ceiling (reduce #'max flat-triangle :key #'d-p-x)))
          for minimal-y = (max 0 (floor (reduce #'min flat-triangle :key #'d-p-y)))
          for maximal-y = (min (1- (scene:camera-height camera))
                               (ceiling (reduce #'max flat-triangle :key #'d-p-y)))
          do (loop for pixel-x from minimal-x to maximal-x
                   do (loop for pixel-y from minimal-y to maximal-y
                            for point-depth = (triangle-contains flat-triangle (v:vec2 pixel-x pixel-y))
                            when point-depth
                            do (when (and (plusp point-depth)
                                          (or (eq (aref depths pixel-x pixel-y) :infinity)
                                              (< point-depth (aref depths pixel-x pixel-y))))
                                 (setf (aref depths pixel-x pixel-y) point-depth)
                                 (sketch:canvas-paint image color pixel-x pixel-y)))))
    (sketch:canvas-lock image)
    (sketch:canvas-image image)))

(in-package :cl-user)

(defsketch render ((scene *scene*)
                   (camera *camera*)
                   (i 0)
                   (x 1))
  ;; update width and height
  (setf (scene:camera-width camera) width
        (scene:camera-height camera) height)
  ;; position camera
  (setf (scene:camera-center camera) (vec:v+ (vec:vec 0 0 0)
                                             (vec:vec (* 3 (cos i))
                                                      (* 3 (sin i))
                                                      x))
        (scene:camera-direction camera) (vec:vunit (vec:v- (scene:camera-center camera)
                                                           (vec:vec 0 0 x)))
        (scene:camera-y-vector camera) (vec:vec 0 0 1)
        (scene:camera-x-vector camera) (vec:vrot (scene:camera-direction camera)
                                                 (scene:camera-y-vector camera)
                                                 (/ pi 2)))
  ;; update step
  (incf i (/ pi 180))
  ;; render to canvas and draw to screen
  (with-pen (make-pen :fill (cl-raster/core::render/sk scene camera))
    (rect 0 0 width height)))

(defmethod kit.sdl2:keyboard-event ((app render) st ts rep? keysym)
  (when (and (eq st :keydown)
             (not rep?))
    (case (sdl2:scancode keysym)
      ((:scancode-up :scancode-u) (incf (render-x app) 1/3))
      ((:scancode-down :scancode-d) (decf (render-x app) 1/3)))))

(when (find-symbol (string-upcase "define-start-function") :sketch)
  (push :sketch-start-function *features*))

#+:sketch-start-function
(progn
  (define-start-function (start) render (:width 700
                                         :height 700))
  (defun sketch::make-default-font ())
  (start-toplevel)
  (quit))

#-sketch-start-function
(progn
  (defparameter *close* nil)
  (defmethod kit.sdl2:close-window :after ((app render))
    (setf *close* t))
  (make-instance 'render :width 700 :height 700)
  (loop until *close*
        do (sleep 1)
        finally (quit)))
