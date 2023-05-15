(defmacro wrap-if (test wrap-form &body body)
  `(if ,test
       ,(append (alexandria:ensure-list wrap-form) body)
       (progn ,@body)))

(defmacro wrap*-if (test wrap-form vars &body body)
  `(flet ((body ()
            (locally (declare (special ,@vars))
              ,@body)))
     (declare (special ,@vars))
     (if ,test
         (,@(alexandria:ensure-list wrap-form)
          (declare (special ,@vars))
          (body))
         (body))))

(let ((x 10))
  (declare (special x))
  (wrap*-if (zerop (random 2))
      (let ((x 20)))
      (x)
    (print (incf x))
    (print (incf x)))
  x)

(let ((wrap? (zerop (random 2))))
  (wrap*-if wrap?
      (let ((x 20)))
      (x)
    (if wrap?
        (dotimes (_ 3)
          (print (incf x)))
        (print "No X available"))))
