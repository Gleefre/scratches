#| Results:

clasp is not tested because it doesn't provide an implementation of macroexpand-all

Allegro, LW, CormanCL and MKCL support passing lexical environment,
but TRIVIAL-MACROEXPAND-ALL doesn't support it.

declarations bug -- DECLARE is not processed correctly, for example placed in a PROGN.
lambda bug -- doesn't expand LAMBDA, even though it is a macro (common behavior).
tag bug -- expands symbol macros in the tag positions in TAGBODY.
not tag bug -- after expanding macros in the tagbody single symbols expansions can become tags.

no-env -- implementation doesn't support passing lexical environment

| impl    | version     | all macro types | declarations bug | expands LAMBDA | tag bug | not tag bug |
|---------+-------------+-----------------+------------------+----------------+---------+-------------|
| sbcl    | 2.4.10      | +               | -                | -              | -       | YES         |
| cmucl   | 21E Unicode | NO              | -                | -              | YES     | YES         |
| ccl     | 1.12        | +               | -                | -              | YES     | YES         |
| allegro | 11.0        | +               | -                | -              | -       | YES         |
| ecl     | 23.9.9      | NO              | YES (runtime)    | -              | YES     | YES         |
| abcl    | 1.9.2       | +               | -                | -              | -       | YES (ints)  |
| clisp   | 2.49.93+    | ±, no-env       | YES (logic)      | YES            | -       | YES         |
| LW      | 8.0.1       | +               | -                | YES            | YES     | YES         |
| corman  | 3.1 (wine)  | +               | YES (logic)      | YES            | -       | YES         |
| mkcl    | 1.1.11.188  | NO              | YES (warning)    | -              | YES     | YES         |


M  -- macro
SM -- symbol macro

global     -- macro defined in the global environment
local-form -- macro defined in the form being macroexpanded
local-env  -- macro defined in the lexical environment being passed

| impl    | global M | local-form M | local-env M | global SM | local-form SM | local-env SM |
|---------+----------+--------------+-------------+-----------+---------------+--------------|
| sbcl    | +        | +            | +           | +         | +             | +            |
| cmucl   | +        | +            | +           | NO        | +             | NO           |
| ccl     | +        | +            | +           | +         | +             | +            |
| allegro | +        | +            | +           | +         | +             | +            |
| ecl     | +        | NO           | +           | NO        | +             | NO           |
| abcl    | +        | +            | +           | +         | +             | +            |
| clisp   | +        | +            | no-env      | +         | +             | no-env       |
| LW      | +        | +            | +           | +         | +             | +            |
| corman  | +        | +            | +           | +         | +             | +            |
| mkcl    | +        | NO           | +           | NO        | +             | NO           |


M -- MACROLET
SM -- SYMBOL-MACROLET
| impl    | behaviour                                                         |
|---------+-------------------------------------------------------------------|
| sbcl    | Keeps M / SM                                                      |
| cmucl   | Keeps M / SM                                                      |
| ccl     | Converts M / SM to PROGN; or to LOCALLY if there are declarations |
| allegro | Keeps M / SM; if EXCL::*KEEP-MACROLET* is NIL converts to LOCALLY |
| ecl     | Keeps M / SM; errors on declarations in SM                        |
| abcl    | Converts M / SM to LOCALLY                                        |
| clisp   | Converts M to PROGN, unless body is 0--1 form; Keeps SM           |
| LW      | Keeps M / SM; merges declarations, removes them if empty          |
| corman  | Converts M to LET (); converts SM to PROGN ()                     |
| mkcl    | Keeps M / SM                                                      |

|#

#-(or sbcl cmucl ccl allegro ecl abcl clisp lispworks cormanlisp mkcl)
(warn "MACROEXPAND-ALL is not provided by this implementation.")

(defpackage #:test-macroexpand-all
  (:use #:cl)
  (:export #:macroexpand-all))

(in-package #:test-macroexpand-all)

;; NIH: trivial-macroexpand-all
#+mkcl (require :walker)
(defun macroexpand-all (form &optional env)
  (declare (ignorable env))
  (values (#+sbcl sb-walker:macroexpand-all
           #+cmucl walker:macroexpand-all
           #+ccl ccl:macroexpand-all
           #+allegro excl::walk-form
           #+ecl walker:macroexpand-all
           #+abcl ext:macroexpand-all
           #+clisp ext:expand-form
           #+lispworks walker:walk-form
           #+cormanlisp ccl:macroexpand-all
           #+mkcl walker:macroexpand-all
           #-(or sbcl cmucl ccl allegro ecl abcl clisp lispworks cormanlisp mkcl) macroexpand
           form
           #-clisp env)
          t
          (or #-clisp t)))

;; utility macro
(defmacro at-compile-time ((&optional env) &body body)
  (let ((capture (gensym)))
    `(macrolet ((,capture (,@(when env `(&environment ,env)))
                  ,@body))
       (,capture))))

;; Test 0
;; Identify lisp implementation
(format t "~&Implementation:~%  ~A version ~A~%" (lisp-implementation-type) (lisp-implementation-version))

;; Test 1
;; Here we test if MACROEXPAND-ALL expands all macro types.

(defmacro NO-1 () ''YES-1)
(define-symbol-macro NO-4 'YES-4)

(format t "~&Test 1~%  ~S~%"
 (macrolet ((NO-3 () ''YES-3))
   (symbol-macrolet ((NO-6 'YES-6))
     (at-compile-time (env)
       `',(macroexpand-all
           '(macrolet ((NO-2 () ''YES-2))
             (symbol-macrolet ((NO-5 'YES-5))
               (values (NO-1) (NO-2) (NO-3) NO-4 NO-5 NO-6)))
           env)))))

;; Expected result (with or without MACROLET and SYMBOL-MACROLET forms)
#+nil
(MACROLET ((NO-2 ()
             ''YES-2))
  (SYMBOL-MACROLET ((NO-5 'YES-5))
    (VALUES 'YES-1 'YES-2 'YES-3 'YES-4 'YES-5 'YES-6)))

;; Test 2
;; Here we throw a bunch of configurations of [symbol-]macrolet,
;; bodies of different lengths and declarations.

(let ((*print-pretty* nil))
  (format t "~&Test 2~%~{  ~S~%~}"
          (mapcan (lambda (header)
                    (mapcar (lambda (form)
                              (handler-case (macroexpand-all form)
                                (error () 'ERROR)))
                            `((,@header )
                              (,@header atom)
                              (,@header (form))
                              (,@header two forms)
                              (,@header (declare))
                              (,@header (declare) and-a-form)
                              (,@header (declare) and two-forms)
                              (,@header (declare (optimize)) and-a-form)
                              (,@header (declare (optimize)) (declare (type t)) and-a-form))))
                  `((macrolet ())
                    (macrolet ((def ())))
                    (symbol-macrolet ())
                    (symbol-macrolet ((def nil)))))))

;; Test 3
;; Here we test if the LAMBDA macro is expanded.

(format t "~&Test 3~%  Expands LAMBDA: ~A~%"
        (not (equal (macroexpand-all '(lambda ())) '(lambda ()))))

;; Test 4
;; Here we test whether symbols with a symbol-macro definition are
;; expanded in the TAGBODY when in the tag position.

(defun tree-find (item tree &key (key #'identity) (test #'eql))
  (labels ((test (x y) (funcall test x y))
           (key (x) (funcall key x))
           (rec (tree)
             (cond ((test item (key tree))
                    tree)
                   ((consp tree)
                    (or (rec (car tree))
                        (rec (cdr tree)))))))
    (rec tree)))

(format t "~&Test 4~%  Wrongly expands tags in TAGBODY: ~A~%"
        (not
         (null
          (tree-find '(tagbody (tag-expanded))
                     (macroexpand-all
                      '(symbol-macrolet ((tag (tag-expanded)))
                        (tagbody tag)))
                     :test 'equal))))

;; Test 5
;; Here we test whether single symbol expansions of macros are
;; protected against becoming tags in TAGBODY

(defmacro not-tag () 'tag-expanded)

(format t "~&Test 5~%  Wrong macro expansion in TAGBODY: ~A~%    expansion: ~S~%"
        (not
         (null
          (or (tree-find '(tagbody tag-expanded)
                         (macroexpand-all '(tagbody (not-tag)))
                         :test 'equal)
              (tree-find '(tagbody)
                         (macroexpand-all '(tagbody (not-tag)))
                         :test 'equal))))
        (macroexpand-all '(tagbody (not-tag))))

(defmacro not-tag-int () 1514)

(format t "~&Test 5' (integer tag)~%  Wrong macro expansion in TAGBODY: ~A~%    expansion: ~S~%"
        (not
         (null
          (or (tree-find '(tagbody 1514)
                         (macroexpand-all '(tagbody (not-tag-int)))
                         :test 'equal)
              (tree-find '(tagbody)
                         (macroexpand-all '(tagbody (not-tag-int)))
                         :test 'equal))))
        (macroexpand-all '(tagbody (not-tag-int))))
