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

(defparameter *destfile* "~/primes/pseudoprimes2.txt")
(defun save-it (n)
  (with-open-file
      (out *destfile*
           :direction :output
           :if-exists :append
           :if-does-not-exist :create)
    (format out "~a~%" n)))

(defun is-prime (n)
  (if (<= n 1) nil t)
  (loop for d from 2 to (floor (sqrt n))
        if (= 0 (mod n d))
        do (return-from is-prime nil)))

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

(defun from-int-to-list (base n)
  (loop while (> n 0)
        collect (mod n base)
        do (setf n (floor (/ n base)))))

(defun next-prime-wave (base wave)
  (let ((result '()))
    (loop for el in wave
          do (loop for dg from 0 to (1- base)
                   if (fis-prime
                       (from-list-to-int
                        base
                        (cons dg el)))
                   do (push (cons dg el) result)))
    result))

(defun next-prime-wave-from-int (base wave)
  (let ((result '()))
    (loop for el in wave
          do (loop for dg from (* base el) to (+ (1- base) (* base el))
                   if (fis-prime dg)
                   do (push dg result)))
    result))

(defun one-digit-primes (base)
  (loop for i from 2 to (1- base)
        if (is-prime i)
        collect (list i)))

(defun bdn (base &optional (max-it 10))
  (let ((wave (one-digit-primes base)))
    (loop for i from 0 to max-it
          ;do (print wave)
          if (null wave)
          do (return-from bdn i)
          do (setf wave (next-prime-wave base wave)))
    nil))

(defun bdnf (base start-digit &optional (max-it 10))
  (let ((wave (list (list start-digit))))
    (loop for i from 0 to max-it
                                        ;do (print wave)
          if (null wave)
          do (return-from bdnf i)
          do (setf wave (next-prime-wave base wave)))
    nil))

(defun bdnfi (base start-digit &optional (max-it 10))
  (let ((wave (list start-digit)))
    (loop for i from 0 to max-it
                                        ;do (print wave)
          if (null wave)
          do (return-from bdnfi i)
          do (setf wave (next-prime-wave-from-int base wave)))
    nil))

(defun dfsl (base start-digit &optional (max-it 10))
  (do ((current-value (list 0 start-digit))
       (length 2)
       (result 1)
       (almost-tp-l (list start-digit))
       (almost-tp start-digit))
      ((> length max-it) (list nil almost-tp almost-tp-l))
                                        ;(print (list current-value (from-list-to-int base current-value) length result (fis-prime (from-list-to-int base current-value))))
    (let ((num (from-list-to-int base current-value)))
     (if (fis-prime num)
         (progn
           (if (>= length result)
               (progn
                 (setf result length)
                 (setf almost-tp-l (copy-list current-value))
                 (setf almost-tp num)))
           (setf current-value (cons 1 current-value))
           (incf length))
         (progn
           (loop
             if (= 1 length) do (return-from dfsl (list result almost-tp almost-tp-l))
             while (= (first current-value) (1- base))
             do (decf length)
             do (setf current-value (rest current-value)))
           (incf (first current-value)))))))

(defun ithrow-d (a &optional (max-it 10))
  (loop for i from 0 to (1- a) collect (dfsl a i max-it)))

(defun ithrow (a &optional (max-it 10))
  (loop for i from 0 to (1- a) collect (bdnf a i max-it)))

(defun ithrowi (a &optional (max-it 10))
  (loop for i from 0 to (1- a) collect (bdnfi a i max-it)))

(defun triangle (n &optional (max-it 30))
  (loop for i from 1 to n collect (ithrow i max-it)))

(defun trianglei (n &optional (max-it 30))
  (loop for i from 1 to n collect (ithrowi i max-it)))

(defun triangle-b (n &optional (max-it 30))
  (loop for i from 1 to n collect (ithrow-d i max-it)))

(defun print-line (ln max-size &optional (out t))
  (format out (make-array (- (+ 2 max-size) (list-length ln))
                          :element-type 'character
                          :initial-element #\,))
  (format out "~{~a~^,,~}" ln)
  (format out (make-array (- max-size (list-length ln))
                          :element-type 'character
                          :initial-element #\,))
  (format out "~%"))

(defun print-line-2 (ln max-size &optional (out t))
  (format out ",,~{~a~^,~}" ln)
  (format out (make-array (- (* 2 max-size) (* 2 (list-length ln)))
                          :element-type 'character
                          :initial-element #\,))
  (format out "~%"))

(defun print-triangle (tr &optional (out t))
  (let ((n (list-length tr)))
    (loop for el in tr
          do (print-line el n out))))

(defun print-triangle-2 (tr &optional (out t))
  (let ((n (list-length tr)))
    (loop for el in tr
          do (print-line-2 el n out))))

(defun print-triangle-3 (tr &optional (out t))
  (let ((n (list-length tr)))
    (loop for i from 0 to (1- n)
          for ln = (loop for j from i to (1- n) collect (nth i (nth j tr)))
          do (print-line-2 ln n out))))

(defun print-triangle-4 (tr &optional (out t))
  (let ((n (list-length tr)))
    (loop for i from 0 to (1- n)
          for ln = (loop for j from i to (1- n) collect (nth (- j i) (nth j tr)))
          do (print-line-2 ln n out))))

(defun save-triangle-as-csv (tr destfile)
  (with-open-file
      (out destfile
           :direction :output
           :if-exists :supersede
           :if-does-not-exist :create)
    (print-triangle tr out)))

(defun save-triangle-as-csv-2 (tr destfile)
  (with-open-file
      (out destfile
           :direction :output
           :if-exists :supersede
           :if-does-not-exist :create)
    (print-triangle-2 tr out)))

(defun save-triangle-as-csv-3 (tr destfile)
  (with-open-file
      (out destfile
           :direction :output
           :if-exists :supersede
           :if-does-not-exist :create)
    (print-triangle-3 tr out)))

(defun save-triangle-as-csv-4 (tr destfile)
  (with-open-file
      (out destfile
           :direction :output
           :if-exists :supersede
           :if-does-not-exist :create)
    (print-triangle-4 tr out)))

(defun save-triangle-all (tr name &optional (exp ".csv"))
  (save-triangle-as-csv tr (concatenate 'string
                                        name
                                        "-1"
                                        exp))
  (save-triangle-as-csv-2 tr (concatenate 'string
                                        name
                                        "-2"
                                        exp))
  (save-triangle-as-csv-3 tr (concatenate 'string
                                        name
                                        "-3"
                                        exp))
  (save-triangle-as-csv-4 tr (concatenate 'string
                                        name
                                        "-4"
                                        exp)))

(defvar *res*
  '((1 0 1)
    (2 0 3)
    (3 4 4)
    (4 4 6)
    (5 5 3)
    (6 7 7)
    (7 5 6)
    (8 8 7)
    (9 10 9)
    (10 8 10)
    (11 10 9)
    (12 10 15)
    (13 10 9)
    (14 17 12)
    (15 13 11)
    (16 14 16)
    (17 18 9)
    (18 15 14)
    (19 15 14)))

(defvar *st1*
  '(1 3 4 6
    3 7 6 7
    9 10 9 15
    9 12 11 16
    9 14 14 ))

(defvar *sq*
  '(1 9 10 16))


(defvar *triangle*
  '((1) 
    (1 6) 
    (5 4 4) 
    (5 6 4 3) 
    (6 4 4 5 2) 
    (8 7 7 5 3 6) 
    (6 6 5 4 5 4 7)
    (9 7 8 7 2 7 4 5) 
    (11 9 10 10 5 4 5 5 5) 
    (9 10 8 8 7 8 7 8 3 4)
    (11 9 10 6 9 8 8 8 3 7 3) 
    (11 15 9 10 7 8 14 7 12 8 6 6)
    (11 9 9 10 8 7 8 9 9 3 7 10 5)
    (18 12 17 9 11 13 10 9 10 10 7 12 9 12)
    (14 11 13 9 10 9 9 13 6 10 11 10 9 10 8)
    (15 16 11 14 11 9 12 6 13 14 14 9 13 11 11 10)))

(defvar *ptriangle*
  '((1)
    (1 6)
    (5 4 4)
    (5 6 4 3)
    (6 4 4 5 2)
    (8 7 7 5 3 6)
    (6 6 5 4 5 4 7)
    (9 7 8 7 2 7 4 5)
    (11 9 10 10 5 4 5 5 5)
    (9 10 8 8 7 8 7 8 3 4)
    (11 9 10 6 9 8 8 8 3 7 3)
    (11 15 9 10 7 8 14 7 12 8 6 6)
    (11 9 9 10 8 7 8 9 9 3 7 10 5)
    (18 12 17 9 11 13 10 9 10 10 7 12 9 12)
    (14 11 13 9 10 9 9 13 6 10 11 10 9 10 8)
    (15 16 11 14 11 9 12 6 13 14 14 9 13 11 11 10)
    (19 9 13 13 10 18 12 10 8 14 9 12 5 9 9 6 7)
    (16 14 15 13 14 11 11 12 15 16 12 6 13 15 13 13 9 11)
    (16 14 14 15 11 13 6 12 10 8 12 13 12 12 12 7 13 11 11)
    (18 16 17 14 15 12 14 14 13 13 9 13 12 14 14 14 13 15 11 10)
    (19 17 14 18 14 17 13 16 12 15 14 13 14 13 12 11 15 15 11 12 10)
    (21 21 15 16 15 20 15 13 17 14 18 13 13 11 16 13 17 14 17 16 15 12)
    (16 14 15 13 14 13 11 15 15 13 11 14 12 15 12 13 14 13 16 13 13 9 13)
    (25 17 16 24 17 15 14 16 18 17 15 15 15 16 17 16 16 15 14 17 16 11 16 12)
    (19 23 17 17 18 17 18 15 17 16 17 16 18 17 19 21 17 18 15 18 15 15 17 13 16)
    (20 18 17 17 16 19 18 16 18 18 18 18 17 15 15 16 17 19 17 15 18 18 15 16 18
     19)
    (22 21 21 19 19 19 17 19 16 17 18 18 16 17 17 15 18 16 17 15 16 17 17 17 14 15
     18)
    (22 20 20 19 18 21 17 20 22 18 18 17 19 17 18 17 17 15 16 17 19 20 16 17 16 19
     18 16)
    (23 19 19 22 19 14 18 17 15 16 18 18 20 15 20 17 14 17 16 13 17 17 16 18 15 17
     15 15 19)
    (23 22 19 19 18 21 19 19 20 22 21 20 19 19 20 19 20 17 19 22 19 20 19 19 17 18
     15 21 20 18)))

(defvar *triangle-probable-38*
  '((1) (1 3) (5 4 4) (5 6 4 3) (6 3 4 5 2) (8 7 7 5 3 6) (6 6 5 4 5 4 7)
    (9 7 8 7 2 7 4 5) (11 9 10 10 5 4 5 5 5) (9 10 8 8 7 8 7 8 3 4)
    (11 9 10 6 9 8 8 8 3 7 3) (11 15 9 10 7 8 14 7 12 8 6 6)
    (11 9 9 10 8 7 8 9 9 3 7 10 5) (18 12 17 9 11 13 10 9 10 10 7 12 9 12)
    (14 11 13 9 10 9 9 13 6 10 11 10 9 10 8)
    (15 16 11 14 11 9 12 6 13 14 14 9 13 11 11 10)
    (19 9 13 13 10 18 12 10 8 14 9 12 5 9 9 6 7)
    (16 14 15 13 14 11 11 12 15 16 12 6 13 15 13 13 9 11)
    (16 14 14 15 11 13 6 12 10 8 12 13 12 12 12 7 13 11 11)
    (18 16 17 14 15 12 14 14 13 13 9 13 12 14 14 14 13 15 11 10)
    (19 17 14 18 14 17 13 16 12 15 14 13 14 13 12 11 15 15 11 12 10)
    (21 21 15 16 15 20 15 13 17 14 18 13 13 11 16 13 17 14 17 16 15 12)
    (16 14 15 13 14 13 11 15 15 13 11 14 12 15 12 13 14 13 16 13 13 9 13)
    (25 17 16 24 17 15 14 16 18 17 15 15 15 16 17 16 16 15 14 17 16 11 16 12)
    (19 23 17 17 18 17 18 15 17 16 17 16 18 17 19 21 17 18 15 18 15 15 17 13 16)
    (20 18 17 17 16 19 18 16 18 18 18 18 17 15 15 16 17 19 17 15 18 18 15 16 18
     19)
    (22 21 21 19 19 19 17 19 16 17 18 18 16 17 17 15 18 16 17 15 16 17 17 17 14 15
     18)
    (22 20 20 19 18 21 17 20 22 18 18 17 19 17 18 17 17 15 16 17 19 20 16 17 16 19
     18 16)
    (23 19 19 22 19 14 18 17 15 16 18 18 20 15 20 17 14 17 16 13 17 17 16 18 15 17
     15 15 19)
    (23 22 19 19 18 21 19 19 20 22 21 20 19 19 20 19 20 17 19 22 19 20 19 19 17 18
     15 21 20 18)
    (23 20 19 19 21 22 21 18 20 18 21 19 18 18 20 19 18 15 19 22 20 17 18 19 19 18
     20 19 21 17 19)
    (24 24 22 20 25 20 20 23 20 20 19 19 17 21 20 17 19 17 19 18 21 18 23 19 17 18
     18 21 18 16 17 20)
    (24 22 23 23 22 21 18 22 24 20 23 20 18 20 23 19 20 18 19 22 21 16 22 18 21 22
     22 17 21 15 22 19 18)
    (26 23 22 25 21 20 21 21 23 23 21 22 20 20 20 21 22 20 18 21 19 20 21 22 23 22
     24 24 20 20 21 20 18 20)
    (25 25 20 22 23 23 23 20 22 20 21 21 20 21 20 22 21 22 20 21 20 21 20 23 20 20
     19 21 19 24 21 21 21 19 26)
    (25 23 24 23 23 24 28 23 22 23 24 23 26 24 23 23 22 22 26 22 21 25 26 22 21 24
     22 18 21 21 26 22 26 21 21 21)
    (27 24 26 22 21 22 22 22 22 20 22 21 24 20 21 25 22 21 21 22 23 19 20 23 18 21
     21 22 22 21 20 21 21 22 23 20 20)
    (28 25 24 27 23 25 24 23 27 23 24 23 23 24 24 25 23 25 22 25 24 21 25 24 22 23
     22 22 26 23 20 21 22 23 27 20 21 21)))

(defvar *row-39-47*
  '((28 24 26 27 25 24 26 23 23 23 23 23 24 24 23 23 25 21 22 23 22 26 23 24 25 25
     23 21 23 22 25 23 24 25 22 23 25 26 22)
    (30 27 28 25 27 29 27 25 23 25 28 24 24 25 26 24 25 29 27 23 25 25 26 24 25 23
     25 27 26 23 24 22 25 22 25 25 24 23 26 26)
    (27 27 26 25 24 24 23 24 25 22 22 25 22 22 23 23 21 23 22 21 22 23 24 23 23 24
     21 24 22 21 24 24 22 24 23 22 22 22 23 24 22)
    (30 28 29 28 28 26 26 27 26 26 24 24 23 25 25 25 26 26 26 25 28 24 25 25 25 25
     25 24 24 22 23 23 29 25 27 27 23 24 26 24 22 23)
    (31 28 30 27 26 26 28 27 28 26 26 27 26 28 25 24 24 24 28 24 26 27 27 27 26 23
     22 25 25 24 25 22 30 27 29 24 24 24 25 24 26 25 22)
    (28 28 27 27 27 26 27 25 27 25 26 26 28 25 24 24 29 26 25 27 24 24 26 27 25 28
     25 25 23 25 24 26 25 23 25 25 25 25 27 23 25 24 25 24)
    (32 30 28 29 28 31 28 28 30 30 26 26 27 29 27 31 32 29 29 27 27 29 27 24 27 29
     25 27 31 27 28 28 27 24 25 27 27 30 27 25 26 27 27 28 28)
    (32 30 30 31 31 28 28 29 29 29 28 29 27 29 27 27 32 27 27 29 28 32 28 31 28 28
     28 26 29 28 26 29 27 29 29 29 27 29 29 26 30 29 25 27 27 30)
    (29 29 27 27 27 27 25 26 25 25 28 25 25 26 27 25 26 28 25 26 24 28 28 24 25 32
     24 28 26 24 24 26 28 26 28 24 27 24 28 27 23 26 29 24 26 25 23)))

(defvar *row-48-57*
  '((33 32 32 29 31 31 28 28 30 29 27 29 27 31 31 26 32 28 28 27 27 28 29 27 29 28
     26 28 29 28 28 27 26 28 26 28 28 30 27 30 27 28 27 26 29 27 30 28)
    (33 32 30 29 30 30 28 32 27 27 30 29 27 32 29 28 30 27 27 29 28 28 28 30 32 27
     31 27 24 27 27 27 27 30 28 27 31 26 28 27 30 28 28 30 27 28 28 27 26)
    (34 31 31 31 30 32 30 33 36 31 32 29 29 27 28 29 31 29 29 30 30 31 28 30 27 28
     30 31 30 29 29 28 29 29 29 28 30 27 26 33 28 27 27 27 27 28 30 32 27 28)
    (36 35 32 32 31 32 30 35 35 33 29 31 31 30 33 29 30 28 30 31 30 30 29 28 28 30
     28 29 31 31 28 30 26 31 30 28 29 28 30 32 29 29 30 31 34 29 28 30 28 29 30)
    (37 33 36 32 31 34 32 32 33 31 31 30 32 30 31 31 30 29 30 32 31 30 30 31 33 31
     32 30 30 31 30 30 31 29 29 29 31 29 29 28 29 31 28 29 29 29 31 28 30 31 30
     29)
    (32 35 31 31 31 29 30 29 29 32 29 31 29 29 32 27 29 29 28 29 33 31 27 29 29 29
     27 29 28 29 29 26 29 30 31 29 26 28 28 30 32 28 29 26 30 27 30 27 28 31 31 30
     28)
    (37 33 32 33 34 35 31 36 33 31 31 33 33 32 30 32 29 31 32 33 36 32 32 29 30 30
     33 33 30 32 31 32 29 30 30 30 30 30 31 32 31 30 32 30 29 34 29 30 30 31 31 32
     32 31)
    (39 35 35 34 34 34 34 34 35 35 33 38 34 36 32 33 36 32 35 33 33 35 32 35 32 33
     34 34 34 34 32 33 34 32 33 34 38 31 34 33 34 33 32 32 32 30 35 32 33 32 31 31
     31 31 34)
    (38 35 34 33 33 34 33 32 34 35 32 33 32 35 33 33 33 31 33 32 34 32 32 37 34 34
     32 32 32 32 34 36 31 32 33 36 33 33 32 31 32 32 33 35 36 32 30 31 34 30 30 32
     32 33 31 33)
    (37 36 34 36 38 35 33 32 38 33 32 34 35 34 33 34 34 36 31 33 35 34 33 32 31 33
     31 33 34 31 32 33 35 31 31 31 34 35 32 31 34 33 32 33 33 32 34 33 33 33 31 32
     30 33 33 32 32)))

(defvar *ptr* (append *triangle-probable-38* *row-39-47* *row-48-56*))

(defvar *errors*
    '((:BASE '(2) :FIRST-DIGIT 1 :RESULT-F 6 :RESULT-I 6 :RESULT-B 6 :TR 3 :error t)
      (:BASE '(3) :FIRST-DIGIT 1 :RESULT-F 4 :RESULT-I 4 :RESULT-B 4 :TR 4)
      (:BASE '(5) :FIRST-DIGIT 1 :RESULT-F 4 :RESULT-I 4 :RESULT-B 4 :TR 3 :error t)
      (:BASE '(7) :FIRST-DIGIT 1 :RESULT-F 6 :RESULT-I 6 :RESULT-B 6 :TR 6)
      (:BASE '(11) :FIRST-DIGIT 1 :RESULT-F 9 :RESULT-I 9 :RESULT-B 9 :TR 9)
      (:BASE '(13) :FIRST-DIGIT 1 :RESULT-F 9 :RESULT-I 9 :RESULT-B 9 :TR 9)
      (:BASE '(17) :FIRST-DIGIT 1 :RESULT-F 9 :RESULT-I 9 :RESULT-B 9 :TR 9)
      (:BASE '(19) :FIRST-DIGIT 1 :RESULT-F 14 :RESULT-I 14 :RESULT-B 14 :TR 14)
      (:BASE '(23) :FIRST-DIGIT 1 :RESULT-F 14 :RESULT-I 14 :RESULT-B 14 :TR 14)
      (:BASE '(29) :FIRST-DIGIT 1 :RESULT-F 19 :RESULT-I 19 :RESULT-B 19 :TR 19)
      (:BASE '(31) :FIRST-DIGIT 1 :RESULT-F 20 :RESULT-I 20 :RESULT-B 20 :TR 20)
      (:BASE '(37) :FIRST-DIGIT 1 :RESULT-F 24 :RESULT-I 24 :RESULT-B 24 :TR 24)
      (:BASE '(41) :FIRST-DIGIT 1 :RESULT-F 27 :RESULT-I 27 :RESULT-B 27 :TR 27)
      (:BASE '(43) :FIRST-DIGIT 1 :RESULT-F 28 :RESULT-I 28 :RESULT-B 28 :TR 28)
      (:BASE '(47) :FIRST-DIGIT 1 :RESULT-F 29 :RESULT-I 29 :RESULT-B 29 :TR 29)))

