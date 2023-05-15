;; quicklisp initialization
(let ((quicklisp-init (merge-pathnames "portacle/all/quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(pushnew #P"~/portacle/projects/" ql:*local-project-directories*)

(defpackage #:grolter-core
  (:use #:cl #:sb-ext))
(in-package #:grolter-core)

(defparameter *systems*
  '((#:al #:alexandria)
    (#:sp #:serapeum)
    #:bordeaux-threads
    (#:re #:cl-ppcre)
    #:cffi
    #:babel
    #:flexi-streams
    #:named-readtables
    (#:ss #:split-sequence)
    #:closer-mop
    (#:we #:wasm-encoder)
    (#:ps #:parenscript)))

(defparameter *systems-without-nicknames* ())
(defparameter *systems-with-nicknames* ())

(defun load-and-nickname (system &optional nickname)
  (ql:quickload system :silent t)
  (if nickname
      (progn
        (add-package-local-nickname nickname system :cl-user)
        (push nickname *systems-with-nicknames*)
        (push system *systems-with-nicknames*))
      (push system *systems-without-nicknames*)))

(dolist (args *systems*)
  (apply #'load-and-nickname
         (if (listp args)
             (reverse args)
             (list args))))

(defparameter *hello-string*
  (format nil "~
Welcome to ~:@(grolter~) core!
Nicknames:
~(~{  :~2a -> :~a~^;~%~}~).
 loaded (~(~{:~a~^ ~}~)).
Happy hacking!~2%"
          (reverse *systems-with-nicknames*)
          (reverse *systems-without-nicknames*)))

(defun shortest-name (package)
  (car (sort (list* (package-name package)
                    (package-nicknames package))
             #'<
             :key #'length)))

(setf sb-int:*repl-prompt-fun*
      (lambda (s) (format s "~&~a> " (shortest-name *package*))))

(push :grolter-core *features*)

(sb-ext:save-lisp-and-die ".sbclrc.core"
  :toplevel (lambda ()
              (format t "~&~a" *hello-string*)
              (unwind-protect (sb-impl::toplevel-init)
                (format t "~2&Bye :-] !~%"))))
