;; Struct, COPY-CONS/ and CONSP/
;; CONS/ and NIL/
(defstruct (cons/ (:constructor cons/ (car/ cdr/ &aux (%cons (cons car/ cdr/))))
                  (:constructor nil/ (&aux (%cons nil)))
                  (:predicate consp/)
                  (:conc-name nil))
  %cons)

;; CAR and CDR
(defun car/ (cons/)
  (car (%cons cons/)))

(defun (setf car/) (v cons/)
  (setf (car (%cons cons/)) v))

(defun cdr/ (cons/)
  (cdr (%cons cons/)))

(defun (setf cdr/) (v cons/)
  (setf (cdr (%cons cons/)) v))

;; NULL
(defun null/ (cons/)
  (null (%cons cons/)))

;; LIST and LIST*
(defun list/ (&rest args)
  (reduce #'cons/ args
          :from-end t
          :initial-value (nil/)))

(defun list*/ (&rest args)
  (reduce #'cons/ args
          :from-end t))

;; PUSH and POP
(defun push/ (val cons/)
  (setf (%cons cons/)
        (cons val (copy-cons/ cons/)))
  cons/)

(defun pop/ (cons/)
  (assert (not (null/ cons/)))
  (setf (%cons cons/)
        (%cons (cdr/ cons/)))
  cons/)

;; Printing and reading
(defmethod print-object ((cons/ cons/) out)
  (loop #:initially (format out "[")
        #:for obj #:= cons/ #:then (cdr/ obj)
        #:for sep #:= "" #:then " "
        #:do (cond ((not (consp/ obj))  ; end but not a list
                    (format out " . ~A]" obj)
                    (loop-finish))
                   ((null/ obj)         ; end of the list
                    (format out "]")
                    (loop-finish))
                   (t                   ; print car and continue
                    (format out "~A~A" sep (car/ obj))))))

(named-readtables:defreadtable :list/
  (:merge :standard)
  (:macro-char #\[
               (lambda (stream char)
                 (declare (ignore char))
                 (apply #'list/ (read-delimited-list #\] stream t))))
  (:macro-char #\]
               (lambda (stream char)
                 (declare (ignore char))
                 (error 'reader-error
                        :stream stream
                        :format-control "Unmatched close bracket ]."))))
