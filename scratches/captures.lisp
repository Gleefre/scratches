;;;; Usage examples
#+nil
(defun/captured foo (z)
  (declare (capture :value x)
           (capture :place y))
  (multiple-value-prog1 (values x y z)
    (incf y)))
#+nil
(let ((x 1)
      (y 2))
  (foo 3)  ; => 1, 2, 3
  (values x y))  ; => 1, 3
#+nil
(let ((x 1))
  (flet/captured ((foo (y)
                    (declare (capture :place x))
                    (prog1 x
                      (setf x y))))
    (let ((x x))
      (foo 10)  ; => 1
      x)  ; => 10
    x  ; => 1
    (foo 20)  ; => 1
    x))  ; => 20

;;;; Implementation

;;; Capture structure

(macrolet ((e (name) `(error "~A slot is not supplied." ',name)))
  (defstruct capture
    (lambda-list    ())
    (call-args      ())
    (body           (e body))
    (call           '%call)
    (args           (e args) :read-only t)
    (name           (e name) :read-only t)
    (function-name  (gensym) :read-only t)))

(defvar *capture*)

;;; Helpers

(defun pass-capture (var expression)
  (push var (capture-lambda-list *capture*))
  (push expression (capture-call-args *capture*)))

;;; Capture namespace
(s:eval-always
  (ql:quickload :in-nomine))

(in-nomine:define-namespace capture)

(defmacro defcapture (name lambda-list &body body
                      &aux (type (gensym)))
  `(setf (symbol-capture ',name)
         (lambda (,type ,@lambda-list)
           (declare (ignore ,type))
           ,@body)))

;;; Standard captures

(defcapture :variable (name)
  (pass-capture name name))

;; TODO:

;;; Expander

;; Alexandria's parse-body signals an error on duplicate docstrings;
;; I'd prefer to signal a warning as SBCL does.
(defun parse-body (body &key (parse-documentation t))
  (loop with doc of-type (or null string)
        for current = (car body)
        if (and parse-documentation (stringp current) (cdr body))
        if doc do (warn "Duplicate doc string ~S" (pop body))
        else do (setf doc (pop body))
        else if (and (listp current) (eq 'declare (car current)))
        collect (pop body) into decls
        else do (loop-finish)
        while (listp body)
        finally (return (values body decls doc))))

(defun get-vars (args)
  (multiple-value-bind (req optional rest key other-keys-p aux key-p)
      (alexandria:parse-ordinary-lambda-list args)
    (declare (ignore other-keys-p key-p))
    (remove nil
            (append req
                    (mapcar #'car optional)
                    (mapcar #'caddr optional)
                    (list rest)
                    (mapcar #'cadar key)
                    (mapcar #'caddr key)
                    (mapcar #'car aux)))))

(defun undefined-capture (type name &rest args)
  (warn "Undefined capture of type ~S for the name ~S."
        `(,type ,@args)
        name))

(defun parse-capture (name lambda-list body)
  (multiple-value-bind (body declarations doc)
      (parse-body body)
    (let (unused-decls captures)
      (loop for (nil . decls) in declarations
            do (loop for (decl-type . args) in decls
                     if (member decl-type '(capture spooky))
                     do (push args captures)
                     else
                     do (push (list* decl-type args) unused-decls)))
      (let ((*capture*
              (make-capture :args lambda-list
                            :body `(progn
                                     ,@body)
                            :name name
                            :function-name (gensym (concatenate 'string (symbol-name name) "-")))))
        (loop for (type . vars) in captures
              for (capture-name . args) = (alexandria:ensure-list type)
              do (loop for var in vars
                       do (apply (symbol-capture capture-name #'undefined-capture)
                                 capture-name
                                 var
                                 args)))
        (with-slots (lambda-list call-args body call args name function-name)
            *capture*
          (let ((form (gensym)))
            (values
             `(,name (&whole ,form ,@args)
                ,@(when doc `(,doc))
                (declare (ignorable ,@(get-vars args)))
                `(symbol-macrolet ((%call (,',function-name ,@',call-args ,@(cdr ,form))))
                   ,',call))
             `(,function-name (,@lambda-list ,@args)
                              (declare ,@unused-decls)
                              ,body))))))))

;;; Toplevel API

(defmacro defun/capture (name lambda-list &body body)
  (multiple-value-bind (macro function)
      (parse-capture name lambda-list body)
    `(progn
       (defun ,@function)
       (defmacro ,@macro))))

(defmacro flet/capture ((&rest definitions) &body body)
  (let (functions macros)
    (loop for (name lambda-list . body) in definitions
          do (multiple-value-bind (macro function)
                 (parse-capture name lambda-list body)
               (push macro macros)
               (push function functions)))
    `(macrolet (,@macros)
       (flet (,@functions)
         ,@body))))

(defmacro labels/capture ((&rest definitions) &body body)
  (let (functions macros)
    (loop for (name lambda-list . body) in definitions
          do (multiple-value-bind (macro function)
                 (parse-capture name lambda-list body)
               (push macro macros)
               (push function functions)))
    `(macrolet (,@macros)
       (labels (,@functions)
         ,@body))))
