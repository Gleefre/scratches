;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with C-x C-f and enter text in its buffer.

(in-package #:cl-user)

(setf *random-state* (make-random-state T))

(ql:quickload '(:harmony
                :cl-mixed-pulse
                :cl-mixed-mpg123
                :bordeaux-threads
                :local-time))

(progn
  (add-package-local-nickname '#:h       '#:org.shirakumo.fraf.harmony)
  (add-package-local-nickname '#:harm    '#:org.shirakumo.fraf.harmony)
  (add-package-local-nickname '#:harmony '#:org.shirakumo.fraf.harmony)
  (add-package-local-nickname '#:m       '#:org.shirakumo.fraf.mixed)
  (add-package-local-nickname '#:mix     '#:org.shirakumo.fraf.mixed)
  (add-package-local-nickname '#:mixer   '#:org.shirakumo.fraf.mixed)
  (add-package-local-nickname '#:bt      '#:bordeaux-threads))

(h:maybe-start-simple-server)

(local-time:enable-read-macros)

(defun at (fun ts)
  (schedule-timer (make-timer fun)
                  (local-time:timestamp-difference ts (local-time:now))))

(defvar *lst* (remove-if-not (lambda (p) (string= "mp3" (pathname-type p)))
                             (uiop:directory-files #P"~/good-root/music/Угадайка/")))

(defparameter *stop* nil)

(defun play-shuffle ()
  (let* ((name (alexandria:random-elt *lst*))
         (voice (h:play name)))
    (loop :until (m:done-p voice)
          :if *stop* :do (return-from play-shuffle)
            :do (sleep 1))
    (play-shuffle)))

(defun curr-name ()
  (pathname-name
   (org.shirakumo.fraf.mpg123::path
    (org.shirakumo.fraf.mixed.mpg123::file
     (aref (m:segments (or (car (h:voices h:*server*))
                           (return-from curr-name "")))
           0)))))

(defun curr-progress ()
  (let ((source (or (car (h:voices h:*server*))
                    (return-from curr-progress -1))))
   (/ (m:frame-position source)
      (m:frame-count source))))

(defun next ()
  (let ((source (aref (m:segments
                       (or (car (h:voices h:*server*))
                           (return-from next nil)))
                      0)))
    (setf (m:done-p source) t)))

(use-package (ql:quickload :sketch))
(use-package (ql:quickload :sketch-fit))

(defsketch shuffle-name ((name))
  (text (format nil "~d%" (floor (curr-progress) 1/100)) 0 0)
  (with-font (make-font :align :center :size (if name 15 20))
    (text (or name
              "Click to show current name.")
          (/ width 2)
          (floor height 5))))

(defmethod kit.sdl2:mousebutton-event ((app shuffle-name) state timestamp button x y)
  (with-slots (name) app
    (setf name (and (not name)
                    (curr-name)))))

(defmethod kit.sdl2:keyboard-event ((app shuffle-name) state ts rep? keysym)
  (when (and (eq state :keydown)
             (not rep?)
             (eq (sdl2:scancode keysym) :scancode-n))
    (next)))

(defmethod kit.sdl2:close-window :after ((app shuffle-name))
  (setf *stop* T))

(make-instance 'shuffle-name :resizable t)
(play-shuffle)
