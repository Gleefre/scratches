(defun theatre-square (n m a)
  (* (1+ (floor (1- n) a))
     (1+ (floor (1- m) a))))

(format t "~a~%" (theatre-square (read) (read) (read)))
