(defmacro lexical-boundp (var)
  `(handler-case (progn ,var t)
     (unbound-variable () nil)))

(defmacro locally-dynamic% ((var &optional default-value) &body body)
  "Declares `var` special, evaluates body in such context, updates latest binding of var"
  (let ((var% (gensym (symbol-name var)))
        (lex-bound (gensym "lex-bound")))
    `(let ((,lex-bound (handler-case (or ,var t)
                         (unbound-variable ()))))
       (let (,var%)
         (prog1
             (let ((,var (if ,lex-bound ,var ,default-value)))
               (declare (special ,var))
               (prog1 (progn ,@body)
                 (setf ,var% ,var)))
           (when ,lex-bound
             (setf ,var ,var%)))))))

(defmacro magic-declaration (var/s &body body)
  "Declare var/s special, executes body, propagates "
  (let* ((vars (ensure-list var/s))
         (gensym/vars (loop for var in vars
                            collect (gensym (symbol-name var)))))
    `(let (,@gensym/vars)
       (prog1
           (let (,@(loop for x in vars
                         collect `(,x (handler-case ,x (unbound-variable ())))))
             (declare (special ,@vars))
             (prog1 (progn ,@body)
               (setf ,@(loop for var in vars
                             for gensym/var in gensym/vars
                             collect gensym/var collect var))))
         (setf ,@(loop for var in vars
                       for gensym/var in gensym/vars
                       collect var collect gensym/var))))))

