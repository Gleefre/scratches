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

(defsketch app ()
  (rect 140 140 120 120)
  (circle 200 200 50))

(setf (kit.sdl2:idle-render (kit.sdl2:last-window)) t)

(defun save-picture (sketch filename)
  (let* ((app (make-instance sketch))
         (window (kit.sdl2:sdl-window app))
         (surface (sdl2:get-window-surface window)))
    (setf (kit.sdl2:idle-render app) nil)
    (sdl2-image:save-png surface filename)
    (print (sdl2:))
    (setf (kit.sdl2:idle-render app) t)))

(defun save-picture (sketch filename)
  (let* ((app (make-instance sketch))
         (window (kit.sdl2:sdl-window app))
         (renderer (sdl2:get-renderer window))
         ;; (surface (sdl2:get-window-surface window))
         ;; (sshot (sdl2::sdl-create-rgb-surface 0
         ;;                                      (kit.sdl2:window-width window) (kit.sdl2:window-height window)
         ;;                                      #x00ff0000 #x0000ff00 #x000000ff #xff000000))
         (format (sdl2:+pixelformat-argb8888+))
         (sshot (sdl2::sdl-create-rgb-surface-with-format
                 0 (kit.sdl2:window-width window) (kit.sdl2:window-height window)
                 32 format))
         (sdl2::sdl-render-read-pixels renderer nil
                                       sdl2:+pixelformat-argb8888+
                                       (slot-value )
                                       sshot->pixels, sshot->pitch)
         (sdl2-image:save-png))))
