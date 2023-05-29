(ql:quickload :cl-raster)
(in-package :cl-raster)
(defun print-image (image)
  (format nil "~{~{~a~}~%~}"
          (loop for x below 100
                collect (loop for y below 100
                              collect (if (> (reduce #'+ (aref image x y)) (* 3 128))
                                          #\+
                                          #\space)))))
(defun princ-image (image)
  (format t (print-image image)))
(defparameter *camera*
  (scene:make-camera :width 100
                     :height 100
                     :center (3d-vectors:vec 0 0 0)
                     :direction (3d-vectors:vec 0 0 1)
                     :x-vector (3d-vectors:vec 1 0 0)
                     :y-vector (3d-vectors:vec 0 1 0)))
(defparameter *scene*
  (scene:make-scene :triangles `((,(3d-vectors:vec 0 2 5)
                                  ,(3d-vectors:vec 0 0 5)
                                  ,(3d-vectors:vec 1 0 5)))))
(loop repeat 360
      do (sleep 0.03)
         (setf (scene:camera-x-vector *camera*)
               (3d-vectors:vrot (scene:camera-x-vector *camera*)
                                (3d-vectors:vec 0 0 1)
                                (/ pi 90))
               (scene:camera-y-vector *camera*)
               (3d-vectors:vrot (scene:camera-y-vector *camera*)
                                (3d-vectors:vec 0 0 1)
                                (/ pi 90)))
         (princ-image (core:render *scene* *camera*)))
