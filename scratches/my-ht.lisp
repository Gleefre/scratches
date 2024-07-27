(defun make-my-ht (&optional (size 101))
  (make-array size :initial-element nil))

(defun my-gethash (key my-ht)
  (let ((i (mod (sxhash key) (length my-ht))))
    (let ((pair (assoc key (aref my-ht i))))
      (if pair
          (values (cdr pair) t)
          (values nil nil)))))

(defun (setf my-gethash) (value key my-ht)
  (let ((i (mod (sxhash key) (length my-ht))))
    (let ((pair (assoc key (aref my-ht i))))
      (if pair
          (setf (cdr pair) value)
          (prog1 value (push (cons key value) (aref my-ht i)))))))
