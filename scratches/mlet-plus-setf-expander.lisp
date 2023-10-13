
(ql:quickload :trivial-macroexpand-all)

(defmacro %macrolet-place (bindings &body (form))
  `(macrolet ,bindings
     ,form))

(define-setf-expander %macrolet-place (&whole form &rest args
                                       &environment env)
  (declare (ignore args))
  (get-setf-expansion
   (car (last (print (trivial-macroexpand-all:macroexpand-all form env))))))

(defmacro mlet ((name lambda-list &body body) &body mlet-body
                &environment env)
  `(macrolet ((,name ,lambda-list
                `(%macrolet-place ((,',name (&whole form &rest args &environment env)
                                            (declare (ignore args))
                                            (funcall ,',(macro-function name env) form env)))
                   ,(progn ,@body))))
     ,@mlet-body))


(defparameter *foo* (list 1 2 3))
(defmacro foo () '*foo*)

(foo)  ; => (1 2 3)
(pop (foo))  ; => 1
(foo)  ; => (2 3)

(mlet (foo () '(car (foo)))
  (foo))  ; => 2

(trivial-macroexpand-all:macroexpand-all '(mlet (foo () '(car (foo)))
                                           (incf (foo))))  ; => 3

(foo)  ; => 3 3
