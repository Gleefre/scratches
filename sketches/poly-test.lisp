(:sl :sketch)
(:sl :sketch-utils)

(defpackage #:sketch-user
  (:use #:cl)
  (:local-nicknames (#:s #:sketch)
                    (#:s+ #:sketch-utils)
                    (#:sc #:stopclock)))

(in-package #:sketch-user)

(defun make-fps-counter (&aux (n 10000))
  (let* ((count 1)
         (clock (sc:make-clock))
         (frame-queue (list :queue 0))
         (frame-queue-end (cdr frame-queue)))
    (flet ((qpush (val)
             (setf (cdr frame-queue-end) (list val)
                   frame-queue-end (cdr frame-queue-end)))
           (qpop ()
             (unless (eq frame-queue frame-queue-end)
               (pop frame-queue)))
           (qfirst ()
             (cadr frame-queue))
           (qlast ()
             (car frame-queue-end)))
      (lambda (action)
        (case action
          (:bump
           (incf count)
           (qpush (sc:time clock))
           (when (> count n) (qpop)))
          (:report
           (/ (min count n)
              (- (qlast) (qfirst)))))))))

(s:defsketch test-polygons ((s:width 300) (s:height s:width)
                            (fps-counter nil)
                            (errors 0)
                            (frames 0))
  (unless fps-counter
    (sdl2:gl-set-swap-interval 0)
    (setf fps-counter (make-fps-counter)))
  (funcall fps-counter :bump)
  (s:with-translate (100 100)
    (s+:with-scissor (0 0 100 100)
      (handler-case (apply #'s:polygon (loop repeat 20 collect (random 100)))
        (error ()
          (warn "POLYGON encountered an error!!!")
          (incf errors)))))
  (incf frames)
  (s:text (format nil "FPS ~$ ERRORS: ~A/~A" (funcall fps-counter :report) errors frames) 0 0))

(make-instance 'test-polygons)
