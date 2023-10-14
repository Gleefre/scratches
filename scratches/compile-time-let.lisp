;; Lexical bindings only version of cltl-1's compiler-let

(defun ensure-list (thing)
  (if (listp thing) thing (list thing)))

(defmacro tmlet ((name lambda-list &body body) &body mlet-body
                 &environment env)
  (let ((transfer (gensym "MLET-TRANSFER")))
    `(macrolet ((,transfer (&rest args &environment env)
                  (funcall ,(macro-function name env)
                           `(,',name ,@args)
                           env))
                (,name ,lambda-list
                  (let ((,name ',transfer))
                    ,@body)))
       ,@mlet-body)))

(defun place (getter setter)
  (declare (ignore setter))
  (funcall getter))

(defun (setf place) (value getter setter)
  (declare (ignore getter))
  (funcall setter value))

(defmacro compile-time (name &optional (default nil default-p))
  (if default-p
      default
      `(error "~A doesn't have compile time value" ',name)))

(defmacro compile-time-let (bindings &body body)
  (let ((specs (loop for (name value) in (mapcar #'ensure-list bindings)
                     collect (list name (gensym "%PROXY") (gensym "%VAR") value))))
    `(macrolet (,@(loop for (name %proxy %var value) in specs
                        collect `(,%proxy ()
                                   (let ((,%var ,value))
                                     (values (lambda () ,%var)
                                             (lambda (v) (setf ,%var v)))))))
       (%compile-time-let (,@(loop for (name %proxy) in specs
                                   collect `(,name ,%proxy)))
         ,@body))))

(defmacro %compile-time-let (bindings &body body &environment env)
  `(tmlet (compile-time (name &rest args)
            (case name
              ,@(loop for (name %proxy) in bindings
                      collect `(,name '(place ,@(multiple-value-list
                                                 (funcall (macro-function %proxy env)
                                                         `(,%proxy)
                                                          env)))))
              (t `(,compile-time ,name ,@args))))
     (symbol-macrolet (,@(loop for (name) in bindings
                               collect `(,name (compile-time ,name))))
       ,@body)))

(defmacro at-compile-time ((&optional env) &body body)
  (let ((capture (gensym)))
    `(macrolet ((,capture (,@(when env `(&environment ,env)))
                  ,@body))
       (,capture))))

#|
;; WRONG!
(defun foo ()
  (compile-time-let ((x 10))
    (incf x)))

;; Good
(defun bar ()
  (compile-time-let ((x 0))
    (macrolet ((foo () (incf x)))
      (foo))))
|#
