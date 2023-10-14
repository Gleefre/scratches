;; One more utility

(defmacro with-lexenv ((var) &body body)
  (let ((g!body (gensym "call-with-lexenv")))
    `(macrolet ((,g!body (&environment ,var)
                  ,@body))
       (,g!body))))

;; reasoned-let*, compile time

(defun build-lets (bindings body-var &optional previous)
  (if bindings
      (destructuring-bind (name &optional value (reason :unknown))
          (alexandria:ensure-list (car bindings))
        `(let ((,name (with-reasoning (,name ,reason ,@previous)
                        ,value)))
           ,(build-lets (cdr bindings) body-var (list* name previous))))
      `(progn ,body-var)))

(defmacro reasoned-let* ((&rest bindings) &body body)
  (let ((g!rtable (gensym "rtable"))
        (g!body (gensym "body")))
    `(compile-time-let (,g!rtable)
       (macrolet ((with-reasoning ((name reason &rest vars) &body body)
                    (let ((g!reason (gensym "reason")))
                      (let ((vars (loop for var in vars collect `(,(gensym (symbol-name var)) ,var))))
                        `(let (,@vars)
                           (declare (ignorable ,@(mapcar #'first vars)))
                           (macrolet ((,g!reason (var g!var)
                                        (pushnew `(,',name <- ,var ,',reason) ,',g!rtable :test 'equal)
                                        g!var))
                             (symbol-macrolet (,@(loop for (g!var var) in vars
                                                       collect `(,var (,g!reason ,var ,g!var))))
                               ,@body))))))
                  (body-with-runtime-reasoning ()
                    `(let ((table ',,g!rtable))
                       ,@',body)))
         (with-lexenv (env)
           (let ((lets (tma:macroexpand-all
                        ',(build-lets bindings g!body)
                        env)))
             `(symbol-macrolet ((,',g!body (body-with-runtime-reasoning)))
                ,lets)))))))


