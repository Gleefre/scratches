(ql:quickload '(:sketch :sketch-fit :easing))
(use-package '(:sketch :sketch-fit))

(defparameter *funcs* (list 'easing:in-back 'easing:out-back 'easing:in-out-back
                            'easing:in-bounce 'easing:out-bounce 'easing:in-out-bounce
                            'easing:in-circ 'easing:out-circ 'easing:in-out-circ
                            'easing:in-cubic 'easing:out-cubic 'easing:in-out-cubic
                            'easing:in-elastic 'easing:out-elastic 'easing:in-out-elastic
                            'easing:in-exp 'easing:out-exp 'easing:in-out-exp
                            'easing:in-quad 'easing:out-quad 'easing:in-out-quad
                            'easing:in-quart 'easing:out-quart 'easing:in-out-quart
                            'easing:in-quint 'easing:out-quint 'easing:in-out-quint
                            'easing:in-sine 'easing:out-sine 'easing:in-out-sine
                            'easing:linear))

(defsketch he ((func-id 0) (i 0))
  (when (zerop (mod (incf i) 60))
    (incf func-id)
    (setf func-id (mod func-id (length *funcs*))))
  (let ((func (nth func-id *funcs*)))
    (background +white+)
    (with-font (make-font :color +black+ :align :center :size 50)
      (text (symbol-name func) (/ width 2) 50))
    (fit 200 200 width height)
    (translate 100 100)
    (with-pen (make-pen :stroke +red+ :weight 1/4)
      (line 50 100 50 -100)
      (line -50 100 -50 -100)
      (line 100 50 -100 50)
      (line 100 -50 -100 -50))
    (loop for x from -100 below 100
          do (point x (* 50 (funcall func (/ x 50)))))))

(make-instance 'he)
