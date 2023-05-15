(require '#:clog)
(require '#:alexandria)

(defpackage #:cloggy-walker
     (:use #:cl #:clog)
     (:import-from #:alexandria #:maxf #:minf #:switch)
     (:export #:start-walker))

(in-package :cloggy-walker)

(defconstant +walker-half-size+ 64)
(defparameter *fps* 60)
(defparameter *rotation-delta* (/ 120 *fps*))
(defparameter *movement-delta* (/ (* 3 +walker-half-size+) *fps*))
(defparameter *debug* nil)
(defparameter *r* 255)
(defparameter *g* 255)
(defparameter *b* 255)

(defclass walker ()
  ((source
    :accessor image-of
    :initform "mooge/walker128.png"
    :documentation "Stores background image of walker")
   (rotation
    :accessor rotation
    :initform 0
    :documentation "Stores current rotation. (deg)")
   (x
    :accessor pos-x
    :initform +walker-half-size+
    :documentation "X coordinate of walker's centre. (pix)")
   (y
    :accessor pos-y
    :initform +walker-half-size+
    :documentation "Y coordinate of walker's centre. (pix)")
   (window-x
    :accessor wind-x
    :initform 0
    :documentation "Window width (pix)")
   (window-y
    :accessor wind-y
    :initform 0
    :documentation "Window height (pix)")
   (controls
    :accessor controls
    :initform (make-hash-table :test 'equal)
    :documentation "Track whether keys WSAD are pressed.")))

(defun make-step (walker coef)
  (let* ((alpha (* (rotation walker) pi 1/180))
         (dx (* coef (cos alpha)))
         (dy (* coef (sin alpha))))
    (incf (pos-x walker) dx)    
    (minf (pos-x walker) (- (wind-x walker) +walker-half-size+))
    (maxf (pos-x walker) +walker-half-size+)

    (incf (pos-y walker) dy)
    (minf (pos-y walker) (- (wind-y walker) +walker-half-size+))
    (maxf (pos-y walker) +walker-half-size+)))

(defun move-walker (walker)
  (let ((controls (controls walker)))
    (incf (rotation walker) (* (- (gethash "d" controls 0)
                                  (gethash "a" controls 0))
                               *rotation-delta*))
    (make-step walker (* (- (gethash "w" controls 0)
                            (gethash "s" controls 0))
                         *movement-delta*))))

(defun on-key-down (obj event)
  (with-sync-event (obj)
    (let ((walker (connection-data-item obj "walker")))
      (setf (gethash (getf event :key)
                     (controls walker))
            1))))

(defun on-key-up (obj event)
  (with-sync-event (obj)
    (let ((walker (connection-data-item obj "walker")))
      (setf (gethash (getf event :key)
                     (controls walker))
            0))))

(defun on-resize (obj)
  (with-sync-event (obj)
    (let ((walker (connection-data-item obj "walker")))
      (setf (wind-x walker) (width obj))
      (setf (wind-y walker) (inner-height obj)))))

(defun on-new-window (body)
  (let ((walker-data (make-instance 'walker)))
    (setf (connection-data-item body "walker") walker-data)
    (on-resize (window body))

    (bordeaux-threads:make-thread
     (lambda ()
       (loop (unless (validp body) (return))
             (setf (image-of walker-data) "mooge/walker128.png")
             (sleep 1)
             (setf (image-of walker-data) "mooge/walker-anim128.png")
             (sleep 0.5)))
     :name "Walker background animation")

    (set-on-key-down body 'on-key-down)
    (set-on-key-up body 'on-key-up)
    (set-on-resize (window body) 'on-resize)

    (let ((walker (create-div body))
          (output (create-div body)))
      (setf (width walker) (unit :px 128))
      (setf (height walker) (unit :px 128))

      (setf (cursor walker) "url(mooge/walker32.png),auto")

      (setf (positioning walker) :fixed)

      (when *debug*
        (setf (style walker "border") "aqua")
        (setf (style walker "border-style") "double"))

      (loop (unless (validp body) (return))
            (move-walker walker-data)
            (setf (left walker) (unit :px (floor (- (pos-x walker-data) +walker-half-size+))))
            (setf (top walker) (unit :px (floor (- (pos-y walker-data) +walker-half-size+))))
            (setf (style walker "rotate") (unit :deg (- (rotation walker-data) 90)))
            (setf (background-image walker) (image-of walker-data))
            (setf (background-color body) (rgb *r* *g* *b*))
            (setf (text output) (rgb *r* *g* *b*))
            (sleep (/ *fps*))))))

(defun start-walker ()
  "Start walker."
  (initialize 'on-new-window)
  (open-browser))

(defun color-move ()
  (loop 
    (let ((r (random 256))
          (g (random 256))
          (b (random 256))
          (old-r *r*)
          (old-g *g*)
          (old-b *b*))
      (loop for i to 100
            do (setf *r* (floor (+ old-r (* i 1/100 (- r old-r))))
                     *g* (floor (+ old-g (* i 1/100 (- g old-g))))
                     *b* (floor (+ old-b (* i 1/100 (- b old-b)))))
            do (sleep 0.1)))))
