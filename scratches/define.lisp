(defmacro with-define-block (&body body &environment env)
  (let ((bindings))
    (macroexpand-all  ; for effect
     `(macrolet ((define (var form)
                   (funcall ,(lambda (var)
                               (pushnew var bindings))
                            var)
                   form))
        ,@body)
     env)
    `(let (,@bindings)
       (macrolet ((define (var form)
                    `(setf ,var ,form)))
         ,@body))))

(defmacro defun/s (name lambda-list &body body &environment env)
  `(defun ,name ,lambda-list
     (with-define-block
       ,@body)))
