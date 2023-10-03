(unless (boundp '+slot-name+)
  (defconstant +slot-name+ '#:slot))

(defun def-class-with-uninterned-slot-name ()
  (eval
   `(defclass my-class ()
      (,(list +slot-name+ :initform 0)))))

(def-class-with-uninterned-slot-name)

(print (slot-value (make-instance 'my-class) +slot-name+))  ; >> 0
