(defpackage #:sketch-bpm-counter
  (:use #:cl #:sketch)
  (:local-nicknames (#:sc #:stopclock)))

(in-package #:sketch-bpm-counter)

(defun make-bpm-counter (&optional (n 5))
  (let* ((count 0)
         (clock (sc:make-clock))
         (queue (list :queue))
         (queue-end queue))
    (flet ((qpush (val)
             (setf (cdr queue-end) (list val)
                   queue-end (cdr queue-end)))
           (qpop ()
             (unless (eq queue queue-end)
               (pop queue)))
           (qfirst ()
             (cadr queue))
           (qlast ()
             (car queue-end))
           (qemptyp ()
             (eq queue queue-end))
           (qreset ()
             (setf (cdr queue) nil
                   queue-end queue)))
      (lambda (action)
        (case action
          (:bump
           (incf count)
           (qpush (sc:time clock))
           (when (> count n) (qpop)))
          (:report
           (if (or (qemptyp) (eq (cdr queue) queue-end))
               0
               (/ (* 60 (min count n))
                  (- (qlast) (qfirst)))))
          (:reset
           (qreset)
           (setf count 0))
          (:idle-time
           (if (qemptyp)
               0
               (- (sc:time clock) (qlast)))))))))

(defsketch bpm-counter
    ((width 300) (height 300)
     (title "BPM counter")
     (bpm-counter (make-bpm-counter 30)))
  (with-fit (100 100 width height)
    (with-font (make-font :size 60 :align :center)
      (with-fit (600 600 100 100)
        (let ((bpm (funcall bpm-counter :report)))
          (text (format nil "BPM: ~,1F" bpm)
                300
                250))))))

(defmethod sketch:on-click ((bpm-counter bpm-counter) x y)
  (when (> (print (funcall (bpm-counter-bpm-counter bpm-counter) :idle-time)) 10)
    (print 'resetting)
    (funcall (bpm-counter-bpm-counter bpm-counter) :reset))
  (funcall (bpm-counter-bpm-counter bpm-counter) :bump))

(make-instance 'bpm-counter :resizable t)
