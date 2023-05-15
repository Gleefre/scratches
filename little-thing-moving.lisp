(ql:quickload :sketch)
(ql:quickload :alexandria)
(use-package :sketch)

(defun random-between (a b)
  (+ a (random (- b a -1))))

(defparameter *maps* (make-hash-table))

(defparameter *start-room* (make-hash-table :test 'equal))
(setf (gethash :start-room *maps*) *start-room*)

(defparameter +width+ 16)
(defparameter +height+ 16)
(defparameter +inner-width+ 1)
(defparameter +inner-height+ 1)
(defparameter +node-side+ 10)

(loop for x from -50 to 50
      do (loop for y from -50 to 50
               when (< (random 10) 7)
               do (push :platform (gethash (cons x y) *start-room*))))

(pushnew :platform (gethash (cons (floor +width+ 2)
                                  (floor +height+ 2))
                            *start-room*))

(defun fit (w h screen-w screen-h)
  (let* ((scale (min (/ screen-w w)
                     (/ screen-h h)))
         (x-shift (/ (- screen-w (* w scale)) 2))
         (y-shift (/ (- screen-h (* h scale)) 2)))
    (translate x-shift y-shift)
    (scale scale)))

(defun draw-background (stars)
  (background +black+)
  (loop for (x . y) in stars
        do #+nil (with-pen (make-pen :stroke +blue+)
                   (point x y))
           (with-pen (make-pen :fill (rgb 0 1 0 0.3))
             (circle x y 2)
             (circle x y 6))
           (with-pen (make-pen :fill (rgb 0 0 1 0.3))
             (circle x y 1)
             (circle x y 5))))

(defun draw-foreground ())

(defun draw-node (node)
  (when (member :platform node)
    (with-pen (make-pen :fill (rgb 1 1 1 0.8))
      (rect 2 2 6 6)))
  (when (member :player node)
    (let ((direction (second (member :player node))))
      (case direction
        (:n (circle 5 0 1))
        (:s (circle 5 10 1))
        (:w (circle 0 5 1))
        (:e (circle 10 5 1))))
    (scale 1/2)
    (with-pen (make-pen)
      (image (load-resource "player.png") 2 2))))

(defsketch little-mover ((scale 1)
                         (width (* scale +width+ +node-side+))
                         (height (* scale +height+ +node-side+))
                         (mp :start-room)
                         (view-x 0)
                         (view-y 0)
                         (player-x (floor +width+ 2))
                         (player-y (floor +height+ 2))
                         (player-dir :e)
                         (stars (loop repeat (* +width+ +height+)
                                      collect (cons (random-between (* -1 +width+ +node-side+)
                                                                    (* 2 +width+ +node-side+))
                                                    (random-between (* -1 +width+ +node-side+)
                                                                    (* 2 +width+ +node-side+))))))

  (fit (* +width+ +node-side+) (* +height+ +node-side+) width height)

  (translate (* -1/10 view-x +node-side+)
             (* -1/10 view-y +node-side+))
  
  (draw-background stars)

  (translate (* 1/10 view-x +node-side+)
             (* 1/10 view-y +node-side+))

  (loop for e in stars
        if (zerop (random 100))
        do (if (zerop (random 2))
               (incf (car e) (* 3 (random-between -1 1)))
               (incf (cdr e) (* 3 (random-between -1 1)))))

  (let ((map (gethash mp *maps*)))
    (when map
      (loop for x from view-x repeat +width+
            do (loop for y from view-y repeat +height+
                     for node = (gethash (cons x y) map)
                     if (and (= x player-x) (= y player-y))
                     do (setf node (append (list :player player-dir) node))
                     do (progn
                          (push-matrix)
                          (draw-node node)
                          (pop-matrix)
                          (translate 0 +node-side+)))
            do (translate +node-side+ (* -1 +width+ +node-side+)))))

  (translate 0 (* -1 +height+ +node-side+))

  (draw-foreground))

(defun move-view (app)
  (with-slots (view-x view-y player-x player-y) app
    (setf view-x (alexandria:clamp view-x
                                   (+ player-x (- (floor +width+ 2)) (- (floor +inner-width+ 2)))
                                   (+ player-x (- (floor +width+ 2)) (ceiling +inner-width+ 2)))
          view-y (alexandria:clamp view-y
                                   (+ player-y (- (floor +height+ 2)) (- (floor +inner-height+ 2)))
                                   (+ player-y (- (floor +height+ 2)) (ceiling +inner-height+ 2))))))

(defun cancel-move?! (app)
  (with-slots (player-x player-y player-dir mp) app
    (unless (member :platform (gethash (cons player-x player-y) (gethash mp *maps*)))
      (case player-dir
        (:n (incf player-y))
        (:s (decf player-y))
        (:w (incf player-x))
        (:e (decf player-x))))))

(defmethod kit.sdl2:keyboard-event ((app little-mover) st ts rep? keysym)
  (when (eql st :keydown)
    (with-slots (view-x view-y player-x player-y player-dir) app
      (case (sdl2:scancode keysym)
        (:scancode-r (setf view-x 0 view-y 0
                           player-x (floor +width+ 2) player-y (floor +height+ 2)))
        (:scancode-w (decf player-y) (setf player-dir :n))
        (:scancode-s (incf player-y) (setf player-dir :s))
        (:scancode-a (decf player-x) (setf player-dir :w))
        (:scancode-d (incf player-x) (setf player-dir :e))
        (t (if (zerop (random 2))
               (incf player-x (random-between -1 1))
               (incf player-y (random-between -1 1)))))
      (cancel-move?! app)
      (move-view app))))

(defun get-node-adress (app x y)
  (with-slots (view-x view-y width height) app
    (let* ((w (* +width+ +node-side+))
           (h (* +height+ +node-side+))
           (scale (min (/ width w)
                       (/ height h)))
           (x-shift (/ (- width (* w scale)) 2))
           (y-shift (/ (- height (* h scale)) 2)))
      (cons (+ view-x (floor (- x x-shift) (* scale +node-side+)))
            (+ view-y (floor (- y y-shift) (* scale +node-side+)))))))

(defmethod kit.sdl2:mousebutton-event ((app little-mover) st ts button x y)
  (when (eq st :mousebuttondown)
    (let ((node-id (get-node-adress app x y)))
      (with-slots (mp) app
        (if (= button 1)
            (pushnew :platform (gethash node-id (gethash mp *maps*)))
            (setf (gethash node-id (gethash mp *maps*))
                  (remove :platform (gethash node-id (gethash mp *maps*)))))))))

(make-instance 'little-mover :scale 5)
