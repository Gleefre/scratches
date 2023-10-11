
(defmacro shallow-macrolet ((&whole defs (name-and-outer-name arglist &body macro-body) &rest definitions)
                            &body body
                            &environment env)
  (declare (ignore name-and-outer-name arglist macro-body definitions))
  (let* ((definitions (loop for (name-outer-name arglist . macro-body) in defs
                            unless (listp name-outer-name)
                              do (setf name-outer-name (list name-outer-name (gensym "unnamed-outer")))
                            collect `(,@name-outer-name ,(gensym "OUTER") ,arglist ,macro-body))))
    `(macrolet (,@(loop for (name outer g!outer arglist body) in definitions
                        collect `(,g!outer (&whole whole &rest args &environment env)
                                   (declare (ignore args))
                                   (funcall ,(macro-function name env) whole env))
                        collect `(,name ,arglist
                                   (let (,@(loop for (nil outer g!outer nil nil) in definitions
                                                 collect `(,outer ',g!outer)))
                                     (declare (ignorable ,@(loop for (nil outer nil nil nil) in definitions
                                                                 collect outer)))
                                     ,@body))))
       ,@body)))
