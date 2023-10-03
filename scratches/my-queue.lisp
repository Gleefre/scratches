(defun make-queue (&rest elements)
  (let* ((queue (append elements (list (cons nil nil))))
         (tail (last queue)))
    (lambda (action)
      (case action
        (:pop (lambda ()
                (if (eq queue tail)
                    (error "Queue is empty!")
                    (pop queue))))
        (:add (lambda (value)
                (setf (car tail) value
                      (cdr tail) (cons nil nil)
                      tail (cdr tail))
                value))))))

(defun queue-pop (queue)
  (funcall (funcall queue :pop)))

(defun queue-add (queue value)
  (funcall (funcall queue :add) value))
