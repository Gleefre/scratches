(defun ensure-list (thing)
  (if (listp thing) thing (list thing)))

;; One more utility

(defmacro with-lexenv ((var) &body body)
  (let ((g!body (gensym "call-with-lexenv")))
    `(macrolet ((,g!body (&environment ,var)
                  ,@body))
       (,g!body))))

(ql:quickload :trivial-macroexpand-all)

(defun form-depends-on-p (vars form &optional env)
  (let ((bindings (loop for var in vars collect `(,(gensym) ,var)))
        (g!reason (gensym))
        (deps ()))
    (flet ((reason (var)
             (pushnew var deps)))
      (trivial-macroexpand-all:macroexpand-all
       `(let (,@bindings)
          (macrolet ((,g!reason (var g!var)
                       (funcall ,#'reason var)
                       g!var))
            (symbol-macrolet (,@(loop for (g!var var) in bindings
                                      collect `(,var (,g!reason ,var ,g!var))))
              ,form)))
       env))
    deps))

(form-depends-on-p '(x y z) '(+ a b x y))



(defmacro form-depends-on-p (vars form)
  (let ((bindings (loop for var in vars collect `(,(gensym) ,var)))
        (g!reason (gensym)))
    `(compile-time-let (deps)
       (at-compile-time (env)
         (let ((expanded (trivial-macroexpand-all:macroexpand-all
                          `(let (,@',bindings)
                             (macrolet ((,',g!reason (var g!var)
                                          (pushnew var deps)
                                          g!var))
                               (symbol-macrolet (,@',(loop for (g!var var) in bindings
                                                           collect `(,var (,g!reason ,var ,g!var))))
                                 ,',form)))
                          env)))
           `(values ',deps
                    ',expanded))))))

;; reasoned-let*, compile time

(defun build-lets (bindings body-var &optional previous)
  (if bindings
      (destructuring-bind (name &optional value (reason :unknown))
          (ensure-list (car bindings))
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
           (let ((lets (trivial-macroexpand-all:macroexpand-all
                        ',(build-lets bindings g!body)
                        env)))
             `(symbol-macrolet ((,',g!body (body-with-runtime-reasoning)))
                ,lets)))))))


