(ql:quickload '(:screamer :do :serapeum))

(in-package :screamer-user)

(defparameter *graph*
  (do/submit::with-assignment-folder :coloring
    (do/coloring::read-data "data/gc_100_1")))

(defun almost-solve (g colors)
  (declare (optimize (speed 3))
           (type (simple-array list) g)
           (type fixnum colors))
  (let ((c (make-array (length g)
                       :initial-contents
                       (loop for i below (length g)
                             collect (an-integer-betweenv 0 (1- colors) i)))))
    (loop for v from 0
          for nbs across g
          do (loop for w in nbs
                   do (assert! (/=v (aref c v) (aref c w)))))
    (assert! (=v (aref c 0) 0))
    (assert! (=v (aref c (car (aref g 0))) 1))
    (labels ((id (x) (screamer::variable-name x))
             (vars (x)
               (remove-if-not #'numberp
                              (mapcar #'value-of (mapcar (lambda (v) (aref c v))
                                                         (aref g (id x))))))
             (scope (x)
               (let ((vars (vars x)))
                 (set-difference (alexandria:iota colors)
                                 vars)))
             (free-degree (x)
               (- (length g)
                  (let ((used (make-array colors :initial-element t :element-type 'boolean)))
                    (loop for e in (mapcar #'value-of (mapcar (lambda (v) (aref c v))
                                                              (aref g (id x))))
                          if (numberp e)
                          if (aref used e)
                          count (not (setf (aref used e) nil)))))))
      (declare (ignorable #'scope #'vars))
      (one-value (solution (coerce c 'list)
                           (reorder #'free-degree
                                    (lambda (x) (declare (ignore x)) nil)
                                    #'<
                                    #'linear-force))))))

(defun search! (c)
  (let ((c (serapeum:sort-new c #'> :key #'domain-size)))
    (one-value)))
