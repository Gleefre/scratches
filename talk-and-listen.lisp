;; Running:
;;   sbcl --load talk-and-listen.lisp --quit
;; Hold space to record.

(ql:quickload '(:sketch :cl-mixed :cl-mixed-pulse))

(add-package-local-nickname '#:mixed '#:org.shirakumo.fraf.mixed)

(defvar *mode* :play)
(defvar *run* T)

(sketch:defsketch talk-and-listen ((sketch:height 300)
                                   (sketch:width 400)
                                   (repeat-segment NIL)
                                   (output-pack NIL))
  (sketch:with-font (sketch:make-font :align :center)
    (sketch:text (format NIL "~a: ~a mode, ~$ sec"
                         repeat-segment
                         *mode*
                         (or (and repeat-segment (mixed:repeat-position repeat-segment)) -1))
                 (/ sketch:width 2)
                 (/ sketch:height 3))))

(defmethod kit.sdl2:keyboard-event ((app talk-and-listen) state ts rep? keysym)
  (when (and (not rep?) (eq :scancode-space (sdl2:scancode keysym)))
    (if (eq :keydown state)
        (setf (mixed:volume (talk-and-listen-output-pack app)) 0
              *mode* :record)
        (setf *mode* :play))))

(defmethod kit.sdl2:close-window :after ((app talk-and-listen))
  (setf *run* NIL))

(defun start (&optional (time 3))
  (mixed:with-objects ((input (mixed:make-unpacker))
                       (output (mixed:make-packer))
                       (rep-r (mixed:make-repeat :time time :mode *mode*))
                       (rep-l (mixed:make-repeat :time time :mode *mode*))
                       (drain (make-instance 'org.shirakumo.fraf.mixed.pulse:drain
                                             :pack output))
                       (mic (make-instance 'org.shirakumo.fraf.mixed.pulse:source
                                           :pack input)))
    (make-instance 'talk-and-listen
                   :resizable T
                   :repeat-segment rep-r
                   :output-pack output)
    (mixed:with-buffers 500 (l r ll rr)
      (mixed:connect input :left rep-l :mono l)
      (mixed:connect rep-l :mono output :left ll)
      (mixed:connect input :right rep-r :mono r)
      (mixed:connect rep-r :mono output :right rr)
      (mixed:with-chain chain (mic input rep-l rep-r output drain)
        (loop while *run*
              do (unless (eq *mode* (mixed:repeat-mode rep-r))
                   (setf (mixed:repeat-mode rep-r) *mode*
                         (mixed:repeat-mode rep-l) *mode*)
                   (when (eq *mode* :play)
                     (setf (mixed:volume output) 1)))
                 (mixed:mix chain))))))

(start)
