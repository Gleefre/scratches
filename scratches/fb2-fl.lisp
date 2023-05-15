(ql:quickload '(:alexandria
                :plump
                :qbase64))

(defun plump-child (node child-index)
  (aref (plump:children node) child-index))

(defun get-image (binary)
  (qbase64:decode-string
   (with-output-to-string (image)
     (plump:serialize (plump-child binary 0) image))))

(defun set-image (binary file)
  (setf (plump:text (plump-child binary 0))
        (qbase64:encode-bytes (alexandria:read-file-into-byte-vector file))))

(defparameter *root* (plump:parse (uiop:read-file-string "../fl/fb2/large.fb2")))
(defparameter *binaries* (subseq (plump:children (plump-child *root* 2)) 2))

(loop for counter from 1
      for image-binary across *binaries*
      do (alexandria:with-output-to-file (out (format nil "../fl/fb2/images/input-~3,,,v@a.jpg" #\0 counter)
                                              :element-type 'unsigned-byte)
           (write-sequence
            (qbase64:decode-string
             (with-output-to-string (image)
               (plump:serialize (plump-child image-binary 0) image)))
            out)))

(defun kinda-compress (in out)
  (uiop:run-program (list "convert" "-monochrome" in out)))

(loop for counter from 1
      for image-binary across *binaries*
      for in-name = (format nil "../fl/fb2/images/input-~3,,,v@a.jpg" #\0 counter)
      for out-name = (format nil "../fl/fb2/images/result-~3,,,v@a.png" #\0 counter)
      do (kinda-compress in-name out-name)
      do (set-image image-binary out-name))

(alexandria:with-output-to-file (out "../fl/fb2/result.fb2")
  (plump:serialize *root* out))
