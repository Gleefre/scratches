(ql:quickload :alexandria)
(ql:quickload :simple-scanf)

(defun ts-ms (hour min sec ms)
  (+ ms (* 1000 (+ sec (* 60 (+ min (* 60 hour)))))))

(defun ms-ts (ms)
  (reverse
   (loop for dt in '(1000 60 60 24)
         collect (mod ms dt)
         do (setf ms (floor ms dt)))))

(defun procede (spec ms)
  (destructuring-bind (h1 m1 s1 ms1 h2 m2 s2 ms2)
      spec
    (append (ms-ts (+ ms (ts-ms h1 m1 s1 ms1)))
            (ms-ts (+ ms (ts-ms h2 m2 s2 ms2))))))

(defun adjust (line ms)
  "Adds `ms' milliseconds to each sub-track"
  (multiple-value-bind (spec spec-p)
      (snf:scanf "%d:%d:%d,%d --> %d:%d:%d,%d" line)
    (if (and spec spec-p)
        (format nil "~{~2,'0d:~2,'0d:~2,'0d,~3,'0d --> ~2,'0d:~2,'0d:~2,'0d,~3,'0d~}"
                (procede spec ms))
        (remove #\# line))))

(defun adjust-all (filename ms &rest open-args)
  (format t "~{~a~%~}"
          (mapcar (lambda (x) (adjust x ms))
                  (apply #'uiop:read-file-lines filename open-args))))

(alexandria:with-output-to-file (*standard-output* "~/Downloads/subs/big-bang.srt"
                                                   :if-exists :supersede)
  (adjust-all "~/Downloads/subs/big-bang-08.srt"
              4500 #+:external-format :iso-8859-1))
