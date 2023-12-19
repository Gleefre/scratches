(declaim (optimize speed (safety 0)))
(require :uiop)

(defpackage #:bf
  (:use #:cl)
  (:export #:main))

(in-package #:bf)

;;; Utilities

(deftype array-index (&optional (length (1- array-dimension-limit)))
  `(integer 0 (,length)))

(deftype array-length (&optional (length (1- array-dimension-limit)))
  `(integer 0 ,length))

(defmacro e (slot-name) `(error ,(format nil "Missing initial value for slot ~A" slot-name)))

#+nil
(deftype go-tag ()
  '(or symbol integer))
#+nil
(defun go-tag-p (object)
  (typep object 'go-tag))
#+nil
(defmacro ctagbody (&rest statements)
  (let ((tags (remove-if-not #'go-tag-p statements)))
    `(macrolet ((cgo (clabel)
                  `(ecase ,clabel
                     ,@(loop for tag in ',tags
                             collect `((,tag) (go ,tag))))))
       (tagbody ,@statements))))

;;; Opcodes

(defconstant +op-inc+ 0)
(defconstant +op-move+ 1)
(defconstant +op-loop+ 2)
(defconstant +op-print+ 3)

#+nil (defconstant +op-exit-loop+ 3)
#+nil (defconstant +op-print+ 4)
#+nil (defconstant +op-input+ 5)
#+nil (defconstant +op-halt+ 6)

(deftype op-type () '(integer 0 3))
(deftype op-value () '(or null fixnum list))

(defstruct (op (:constructor make-op (type value)))
  (type  (e type) :type op-type)
  (value (e value) :type op-value))

;;; Printer

(defstruct printer
  (sum1 0 :type (unsigned-byte 8))
  (sum2 0 :type (unsigned-byte 8))
  (quiet (e quiet) :type boolean))

(declaim (ftype (function (printer (unsigned-byte 8)) null)))
(defun printer-print (printer n)
  (declare (type printer printer)
           (type (unsigned-byte 8) n))
  (with-slots (sum1 sum2 quiet) printer
    (if quiet
        (setf sum1 (mod (+ sum1 n) 255)
              sum2 (mod (+ sum1 sum2) 255))
        (progn
          (write-char (code-char n))
          (finish-output))))
  nil)

(declaim (ftype (function (printer) (unsigned-byte 16))))
(defun printer-checksum (printer)
  (declare (type printer printer))
  (with-slots (sum1 sum2) printer
    (+ sum1 (ash sum2 8))))

;;; Parser

(declaim (ftype (function (stream) list) parse))
(defun parse (program-stream)
  (loop for byte = (read-char program-stream nil nil)
        while byte until (char= byte #\])
        for op = (case byte
                   (#\+ (make-op +op-inc+ 1))
                   (#\- (make-op +op-inc+ -1))
                   (#\> (make-op +op-move+ 1))
                   (#\< (make-op +op-move+ -1))
                   (#\. (make-op +op-print+ nil))
                   (#\[ (make-op +op-loop+ (parse program-stream))))
        when op collect op))

;;; Tape

(defstruct tape
  (cap 1 :type array-length)
  (pos 0 :type array-index)
  (tape (make-array 1 :element-type 'fixnum :initial-element 0)
   :type (simple-array fixnum)))

;;; Tape utilities

(declaim (inline tape-get tape-grow tape-move tape-inc))

(declaim (ftype (function (tape) fixnum) tape-get))
(defun tape-get (tape)
  (declare (optimize speed (safety 0))
           (type tape tape))
  (aref (tape-tape tape) (tape-pos tape)))

(declaim (ftype (function (tape) null) tape-grow))
(defun tape-grow (tape)
  (declare (optimize speed (safety 0))
           (type tape tape))
  (with-slots (cap tape) tape
    (setf cap (ash cap 1)
          tape (adjust-array tape cap :initial-element 0)))
  nil)

(declaim (ftype (function (tape fixnum) null) tape-move))
(defun tape-move (tape amount)
  (declare (optimize speed (safety 0))
           (type tape tape)
           (type fixnum amount))
  (incf (tape-pos tape) amount)
  (when (>= (tape-pos tape) (tape-cap tape))
    (tape-grow tape))
  nil)

(declaim (ftype (function (tape fixnum) null) tape-inc))
(defun tape-inc (tape amount)
  (declare (optimize speed (safety 0))
           (type tape tape)
           (type fixnum amount))
  (incf (aref (tape-tape tape)
              (tape-pos tape))
        amount)
  nil)

(declaim (ftype (function (list tape printer) null) eval-bf))
(defun eval-bf (ops tape printer)
  (declare (type list ops)
           (type tape tape)
           (type printer printer))
  (loop for op = (pop ops) while op
        do (case (op-type op)
             (#.+op-inc+ (tape-inc tape (the fixnum (op-value op))))
             (#.+op-move+ (tape-move tape (the fixnum (op-value op))))
             (#.+op-loop+ (loop until (zerop (tape-get tape))
                                with inner-op-list = (the list (op-value op))
                                do (eval-bf inner-op-list tape printer)))
             (#.+op-print+ (printer-print printer (the (unsigned-byte 8) (tape-get tape)))))))

(declaim (ftype (function (string printer) null) run))
(defun run (code printer)
  (declare (type string code)
           (type printer printer))
  (eval-bf (parse (make-string-input-stream code)) (make-tape) printer))

(defun verify ()
  (declare (optimize (speed 1) safety debug))
  (assert
   (string= (format nil "Hello World!~%")
            (with-output-to-string (*standard-output*)
              (run "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>
                  ---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++."
                   (make-printer :quiet nil))))))

;;; Benchmark utilities

#+nil (require :sb-bsd-sockets)
#+nil
(defun notify (msg)
  (let (socket)
    (unwind-protect
         (progn
           (setf socket (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp))
           (handler-case
               (progn
                 (sb-bsd-sockets:socket-connect socket #(127 0 0 1) 9001)
                 (sb-bsd-sockets:socket-send socket msg nil))
             (sb-bsd-sockets:socket-error ())))
      (when socket
        (sb-bsd-sockets:socket-close socket)))))

;;; Entry point

#+nil (require :sb-posix)

(require :sb-sprof)

(defun main ()
  (verify)
  (let* ((program (destructuring-bind (program-file) (uiop:command-line-arguments)
                    (uiop:read-file-string program-file)))
         (env-quiet (uiop:getenv "QUIET"))
         (quietp (and (stringp env-quiet) (not (string= "" env-quiet))))
         (printer (make-printer :quiet quietp)))
    #+nil (notify (format nil "~A~C~D" (lisp-implementation-type) #\tab (sb-posix:getpid)))
    (sb-sprof:with-profiling (:report :flat)
     (run program printer))
    #+nil (notify "stop")
    (when quietp
      (format t "Output checksum: ~A~%" (printer-checksum printer))
      (finish-output))))

#+build
(sb-ext:save-lisp-and-die "l.out" :toplevel #'main :executable t)
#-build (main)
