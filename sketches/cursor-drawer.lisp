(ql:quickload (list :sketch :sketch-utils :sketch-buttons))
(nick #:s #:sketch #:s+ #:sketch-utils #:sb #:sketch-buttons)

(defun make-cursor (pixels &optional (hot-x 0) (hot-y 0)
                    &aux (height (array-dimension pixels 0))
                         (width (array-dimension pixels 1))
                         (size (/ (array-total-size pixels) 8))
                         (k 4))
  (assert (= 2 (length (array-dimensions pixels))))
  (assert (zerop (mod width 8)))
  (assert (plusp size))
  (cffi:with-foreign-object (data :uint8 (* k k size))
    (cffi:with-foreign-object (mask :uint8 (* k k size))
      (loop for i below size
            do (setf (cffi:mem-aref mask :uint8 i) 255))
      (loop for x below (* k width)
            do (loop for y below (* k height)
                     do (multiple-value-bind (index pos)
                            (floor (+ (* k height x) y) 8)
                          (setf (ldb (byte 1 (- 7 pos)) (cffi:mem-aref data :uint8 index))
                                0)
                          (setf (ldb (byte 1 (- 7 pos)) (cffi:mem-aref mask :uint8 index))
                                (- 1 (aref pixels (floor y k) (floor x k)))))))
      (prog1 (sdl2-ffi.functions:sdl-create-cursor data mask
                                                   (* k width) (* k height)
                                                   (* k hot-x) (* k hot-y))
        (multiple-value-bind (message error)
            (sdl2-ffi.functions:sdl-get-error)
          (unless (string= "" message)
            (sdl2-ffi.functions:sdl-clear-error)
            (error "SDL2 error: ~A ~S" message error)))))))

(defun free-cursor (cursor)
  (sdl2-ffi.functions:sdl-free-cursor cursor))

(defun set-cursor (cursor)
  (prog1 (sdl2-ffi.functions:sdl-set-cursor cursor)
    (multiple-value-bind (message error)
        (sdl2-ffi.functions:sdl-get-error)
      (unless (string= "" message)
        (sdl2-ffi.functions:sdl-clear-error)
        (error "SDL2 error: ~A ~S" message error)))))

(defun draw-cursor (pixels)
  (loop for x below 16
        do (loop for y below 16
                 do (s+:with-translate ((* x 4) (* y 4))
                      (s+:with-color ((if (zerop (aref pixels x y))
                                          s:+white+
                                          s:+black+))
                        (s:rect 2/3 2/3 8/3 8/3)
                        (sb:binds (sb:brect 2/3 2/3 8/3 8/3)
                          :hover (let ((x x) (y y))
                                   (λ (&rest _)
                                      (setf (aref pixels x y)
                                            (- 1 (aref pixels x y)))))))))))

(s:defsketch cursy ((pixels (make-array '(16 16) :initial-element 1 :element-type 'bit))
                    (reset t)
                    (cursor nil))
  (when reset
    (when cursor
      (free-cursor cursor))
    (setf cursor (make-cursor pixels 8 8))
    (set-cursor cursor))
  (s:background (s:gray 0.2))
  (s+:with-fit (64 64 s:width s:height)
    (draw-cursor pixels)))

(make-instance 'cursy :resizable t)