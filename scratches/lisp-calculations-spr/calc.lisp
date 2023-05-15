(defun expt-mod (n exponent modulus)
  "As (mod (expt n exponent) modulus), but more efficient."
  (declare (optimize (speed 3) (safety 0) (space 0) (debug 0)))  
  (loop with result = 1
        for i of-type fixnum from 0 below (integer-length exponent)
        for sqr = n then (mod (* sqr sqr) modulus)
        when (logbitp i exponent) do
        (setf result (mod (* result sqr) modulus))
        finally (return result)))

(defun random-between (a b)
  (+ a (random (- b a -1))))

(defun fis-prime (n)
  (if (< n 2)
      nil
      (progn (loop for i from 1 to 100
                   do (let ((a (random-between 1 (1- n))))
                        (if (= 1 (gcd a n))
                            (if (/= 1 (expt-mod a (1- n) n))
                                (return-from fis-prime nil))
                            (return-from fis-prime nil))))
                                        ;(save-it n)
             t)))

(defun from-list-to-int (base digits)
  (loop for ch in digits
        with power = 1
        sum (* power ch)
        do (setf power (* power base))))

(defun save-list (n)
  (with-open-file
      (out "~/spr/night2.txt"
           :direction :output
           :if-exists :append
           :if-does-not-exist :create)
    (print n out)))

(defun dfsl (base start-digit &optional (max-it 10))
  (do ((current-value (list 0 start-digit))
       (length 2)
       (result 1))
      ((> length max-it) nil)
    (if (fis-prime (from-list-to-int base current-value))
        (progn
          (setf result (max result length))
          (setf current-value (cons 1 current-value))
          (incf length))
        (progn
          (loop
            if (= 1 length) do (return-from dfsl result)
            while (= (first current-value) (1- base))
            do (decf length)
            do (setf current-value (rest current-value)))
          (incf (first current-value))))))

(defun ithrow-d (a &optional (max-it 10))
  (loop for i from 0 to (1- a) collect (dfsl a i max-it)))

(loop for i from 45 to 65
      do (save-list (list `(ithrow-d ,i 70) (ithrow-d i 70))))

