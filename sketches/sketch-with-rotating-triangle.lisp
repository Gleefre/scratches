(use-package (ql:quickload :sketch))
(ql:quickload :cl-raster)

(add-package-local-nickname '#:scene '#:cl-raster/scene)
(add-package-local-nickname '#:vec   '#:3d-vectors)

(defparameter *camera*
  (scene:make-camera :width 400
                     :height 400
                     :center (vec:vec 0 0 0)
                     :direction (vec:vec 0 0 1)
                     :x-vector (vec:vec 1 0 0)
                     :y-vector (vec:vec 0 1 0)))

(defparameter *scene*
  (scene:make-scene :triangles `((,(vec:vec 0 2 5)
                                  ,(vec:vec 0 0 5)
                                  ,(vec:vec 1 0 5)))))

(defsketch render ((scene *scene*)
                   (camera *camera*)
                   (i 0))
  ;; update width and height
  (setf (scene:camera-width camera) width
        (scene:camera-height camera) height)
  ;; rotate camera
  (let ((rot-angle (/ pi 180)))
    (3d-vectors:nvrot (scene:camera-x-vector camera)
                      (3d-vectors:vec 0 0 1)
                      rot-angle)
    (3d-vectors:nvrot (scene:camera-y-vector camera)
                      (3d-vectors:vec 0 0 1)
                      rot-angle))
  ;; move camera a little bit
  (setf (vec:vz (scene:camera-center camera))
        (* 1 (sin (* (incf i)
                     (/ 180 1000 pi)))))
  ;; render and copy to canvas
  (let ((image (cl-raster/core:render scene camera))
        (canvas (make-canvas width height)))
    (dotimes (x width)
      (dotimes (y height)
        (canvas-paint canvas (apply #'rgb (mapcar (lambda (x)
                                                    (/ x 255))
                                                  (aref image x y)))
                      x y)))
    (canvas-lock canvas)
    (with-pen (make-pen :fill (canvas-image canvas))
      (rect 0 0 width height))))

(make-instance 'render :resizable T :width 200 :height 200)
