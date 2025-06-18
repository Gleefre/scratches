;; unicode?.. https://www.unicode.org/versions/Unicode16.0.0/UnicodeStandard-16.0.pdf

(deftype ascii   () '(mod #x80))
(deftype latin1  () '(mod #x100))
(deftype bmp     () '(mod #x10000))
(deftype unicode () '(mod #x110000))

(deftype surrogate () '(integer #xD800 (#xE000)))

(deftype code-point () 'unicode)
(deftype unit (n) `(unsigned-byte ,(* n 8)))

;; literal (done-prelim)
(define-encoding :ascii)
(define-encoding :iso-8859-1)
(define-encoding :ucs-2) ; ?
(define-encoding :utf-32)

;; code pages
(define-encoding :code-page-?) ; ...
(define-encoding :iso-8859-X) ; ...
(define-encoding :windows-X) ; ...
(define-encoding :ibm-X) ; ...

;; unicde (done-prelim)
(define-encoding :utf-8)  ; :wtf-8, :utf-8b, :cesu-8, :mutf-8
(define-encoding :utf-16) ; :wtf-16

;; jap
(define-encoding :jis)
(define-encoding :shift-jis)
(define-encoding :euc)

;; chi
(define-encoding :gb-X)
(define-encoding :gbk)
(define-encoding :big5)

;; more
(define-encoding :koi-X)

;; MORE!!!


;; definitions?

;; a code-point -- character code (unicode code-point)
;; a code-unit  -- several (1, 2 or 4) octets

;; (done) -- early return

;; with-next-point -- either writes or reads a code-point
;; with-next-unit -- either writes or reads a code-unit
;; -- must accepts :return-eof T to inhibit automatic EOF handling, this is used by utf-8b
;; with-next-points, with-next-units -- multiple points/units version of the above
;; last-unit / last-units / last-point / last-points
;; -- same as (with-next-X (...) (done))

;; The following "features" of the external formats might apply:
;;   ascii -- T if max code-point is ascii, else NIL
;;   latin1 -- T if max code-point is latin1, else NIL
;;   bmp -- T if max code-point is bmp, else NIL
;; In the decoder these might inhibit sanity checks, so use them only after checking the validity of the statement.
;; These are treated as variables, but should preferrably be defined as symbol-macro's.
;;
;; In addition:
;;   unchecked -- T means don't do extra validity checks. Use at your own risk!
;;   overlong  -- T means accept overlong sequences (used by utf-8)
;;   surrogate -- T means accept surrogate code-points

;; (coder-error datum &rest args) -- macro/local function to raise an encoding or decoding error.
;;   This (probably?) should be handling the substitution/replacement stuff.
;;
;; coder-cerror -- like above, but signals a continuable error.
;;   Used for checking for overlong utf-8 sequences, for surrogates, ...?

;; examples (can't run this yet)


;; literal (identity)

(define-encoder :ascii ()
  (with-next-point (code)
    (if (or ascii (< code #x80))
        (last-unit code)
        (coder-error "Character out of range: #x~2,'0X" code))))

(define-decoder :ascii ()
  (with-next-unit (code)
    (if (or ascii (< code #x80))
        (last-point code)
        (coder-error "Unit out of range: #x~2,'0X" code))))

(define-encoder :latin1 ()
  (with-next-point (code)
    (if (or latin1 (< code #x100))
        (last-unit code)
        (coder-error "Character out of range: #x~2,'0X" code))))

(define-decoder :latin1 ()
  (with-next-unit (code)
    (if (or latin1 (< code #x100))
        (last-point code)
        (coder-error "Unit out of range: #x~2,'0X" code))))

(define-encoder :ucs-2 ()
  (with-next-point (code)
    (unless (or latin1 surrogate unchecked (< code #xD800) (< #xDFFF code))
      (coder-cerror "Surrogate character: #x~4,'0X"))
    (if (or bmp (< code #x10000))
        (last-unit code)
        (coder-error "Character out of range: #x~4,'0X" code))))

(define-decoder :ucs-2 ()
  (with-next-unit (code)
    (unless (or latin1 surrogate unchecked (< code #xD800) (< #xDFFF code))
      (coder-cerror "Surrogate character: #x~4,'0X"))
    (if (or bmp (< code #x10000))
        (last-point code)
        (coder-error "Unit out of range: #x~4,'0X" code))))

(define-encoder :utf-32 ()
  (with-next-point (code)
    (unless (or latin1 surrogate unchecked (< code #xD800) (< #xDFFF code))
      (coder-cerror "Surrogate character: #x~4,'0X"))
    (if (or unicode unchecked (< code #x110000))
        (last-unit code)
        (coder-error "Code point out of range: #x~8,'0X" code))))

(define-decoder :utf-32 ()
  (with-next-unit (code)
    (if (or unicode unchecked (< code #x110000))
        (last-point code)
        (coder-error "Code point out of range: #x~8,'0X" code))))

;; utf-16 & variations

(declaim (inline surrogate-tag))
(defun surrogate-tag (code)
  (mask-field (byte 6 10) code))

(define-encoder :utf-16 ()
  (with-next-point (code)
    (cond ((not (or latin1 surrogate unchecked (< code #xD800) (< #xDFFF code)))
           (coder-error "Surrogate character: #x~4,'0X"))
          ((or bmp (< code #x10000))
           (last-unit code))
          (t (let ((code (the (unsigned-byte 20) (- code #x10000))))
               (last-units (logior #xD800 (ldb (byte 10 10) code))
                           (logior #xDC00 (ldb (byte 10 0) code))))))))

(define-decoder :utf-16 ()
  (with-next-unit (code)
    (cond (bmp (unless (or latin1 surrogate unchecked (< code #xD800) (< #xDFFF code))
                 (coder-cerror "Surrogate character: #x~4,'0X"))
               (last-point code))
          (t (let ((tag (surrogate-tag code)))
               (cond ((= #xD800 tag)
                      (let ((high (ash (- code #xD7F7) 10)))
                        (with-next-unit (code)
                          (cond ((or unchecked (= #xDC00 (surrogate-tag-code)))
                                 (last-point (+ high code)))
                                ((= #xD800 (surrogate-tag-code))
                                 (coder-error "Unexpected high surrogate unit: #x~4,'0X" code))
                                (t (coder-error "Expected a low surrogate unit, found: #x~4,'0X" code))))))
                     ((and (not unchecked) (= #xDC00 tag))
                      (coder-error "Unexpected low surrogate unit: #x~4,'0X" code))
                     (t (last-point code))))))))


(define-encoder :wtf-16 ()  ; like unchecked UTF-16
  (with-next-point (code)
    (if (or bmp (< code #x10000))
        (last-unit code)
        (let ((code (the (unsigned-byte 20) (- code #x10000))))
          (last-units (logior #xD800 (ldb (byte 10 10) code))
                      (logior #xDC00 (ldb (byte 10 0) code)))))))

(define-decoder :wtf-16 ()
  (with-next-unit (code)
    (if (and (not bmp) (= #xD800 (surrogate-tag code)))
        (let ((high (ash (- code #xD7F7) 10)))
          (with-next-unit (code)
            (last-point (+ high code))))
        (last-point code))))


;; utf-8 & variations

(defmacro with-next-utf-8-continuation-unit ((name) &body body)
  `(with-next-unit (,name)
     (if (or unchecked (= #x80 (mask-field (byte 2 6) ,name)))
         (progn ,@body)
         (coder-error "Expected a UTF-8 continuation byte, found: #x~2,'0X" ,name))))

(defmacro with-next-utf-8-continuation-units ((&rest names) &body body)
  (if names
      `(with-next-utf-8-continuation-unit (,(car names))
         (with-next-utf-8-continuation-units (,@(cdr names))
           ,@body))
      `(progn ,@body)))

(define-encoder :utf-8 ()  ; normal utf-8
  (with-next-point (code)
    (cond ((or ascii (< code #x80))
           (last-unit code))
          ((or latin1 (< code #x800))
           (last-units (logior #xC0 (ldb (byte 5 6) code))
                       (logior #x80 (ldb (byte 6 0) code))))
          ((or bmp (< code #x10000))
           (unless (or surrogate unchecked (< code #xD800) (< #xDFFF code))
             (coder-cerror "Surrogate character: #x~4,'0X"))
           (last-units (logior #xE0 (ldb (byte 4 12) code))
                       (logior #x80 (ldb (byte 6 6) code))
                       (logior #x80 (ldb (byte 6 0) code))))
          (t (last-units (logior #xF0 (ldb (byte 3 18) code))
                         (logior #x80 (ldb (byte 6 12) code))
                         (logior #x80 (ldb (byte 6 6) code))
                         (logior #x80 (ldb (byte 6 0) code)))))))

(define-decoder :utf-8 ()
  (with-next-unit (c1)
    (cond ((or ascii (< c1 #x80))
           (last-point c1))
          ((< c1 #xC0)
           (coder-error "Unexpected UTF-8 continuation byte: #x~2,'0X" c1))
          ((or latin1 (< c1 #xE0))
           (let ((high (ash (ldb (byte 5 0) c1) 6)))
             (with-next-utf-8-continuation-unit (c2)
               (when (and (not (or overlong unchecked)) (< c1 #xC2))
                 (coder-cerror "Overlong UTF-8 sequence:~@{ #x~2,'0X~}" c1 c2))
               (last-point (logior (ldb (byte 6 0) c2) high)))))
          ((or bmp (< c1 #xF0))
           (let ((high (ash (ldb (byte 4 0) c1) 12)))
             (with-next-utf-8-continuation-units (c2 c3)
               (when (and (not (or overlong unchecked)) (= c1 #xE0) (< c2 #xA0))
                 (coder-cerror "Overlong UTF-8 sequence:~@{ #x~2,'0X~}" c1 c2 c3))
               (if (or surrogate unchecked (/= c1 #xED) (< c2 #xA0))
                   (last-point (logior (ldb (byte 6 0) c3)
                                       (ash (ldb (byte 6 0) c2) 6)
                                       high))
                   (coder-error "Surrogate UTF-8 sequence:~@{ #x~2,'0X~}" c1 c2 c3)))))
          ((or unchecked (< c1 #xF8))
           (let ((high (ash (ldb (byte 3 0) c1) 18)))
             (with-next-utf-8-continuation-units (c2 c3 c4)
               (when (and (not (or overlong unchecked)) (= c1 #xF0) (< c2 #x90))
                 (coder-cerror "Overlong UTF-8 sequence:~@{ #x~2,'0X~}" c1 c2 c3 c4))
               (let ((code (logior (ldb (byte 6 0) c4)
                                   (ash (ldb (byte 6 0) c3) 6)
                                   (ash (ldb (byte 6 0) c2) 12)
                                   high)))
                 (if (or unchecked (< code #x110000))
                     (last-point code)
                     (coder-error "Code point out of range: #x~6,'0X" code))))))
          (t (coder-error "Invalid UTF-8 starting byte: #x~2,'0X" c1)))))


(defmacro last-utf-8b-invalid-points (names)
  `(last-units ,@(loop for name in names collect `(logior #xDC00 ,name))))

(defmacro with-next-utf-8b-continuation-unit ((&rest previous-names) (name) &body body)
  `(with-next-unit (,name :return-eof T)
     (cond
       ((null name) (last-utf-8b-invalid-points ,@previous-names))
       ((= #x80 (mask-field (byte 2 6) ,name)) ,@body)
       (t (with-unread-unit (,name) (last-utf-8b-invalid-points ,@previous-names))))))

(defmacro with-next-utf-8b-continuation-units ((&rest previous-names) (&rest names) &body body)
  (if names
      `(with-next-utf-8b-continuation-unit (,@previous-names) (,(car names))
         (with-next-utf-8b-continuation-units (,@previous-names ,(car names)) (,@(cdr names))
           ,@body))
      `(progn ,@body)))

(define-encoder :utf-8b ()  ; encodes malformed utf-8 sequences as malformed utf-16 sequences
  (with-next-point (code)
    (cond ((or ascii (< code #x80))
           (last-unit code))
          ((or latin1 (< code #x800))
           (last-units (logior #xC0 (ldb (byte 5 6) code))
                       (logior #x80 (ldb (byte 6 0) code))))
          ((= #xDC00 (mask-field (byte 8 8) code))
           (last-unit (ldb (byte 8 0) code)))
          ((or bmp (< code #x10000))
           (unless (or surrogate unchecked (< code #xD800) (< #xDFFF code))
             (coder-cerror "Surrogate character: #x~4,'0X"))
           (last-units (logior #xE0 (ldb (byte 4 12) code))
                       (logior #x80 (ldb (byte 6 6) code))
                       (logior #x80 (ldb (byte 6 0) code))))
          (t (last-units (logior #xF0 (ldb (byte 3 18) code))
                         (logior #x80 (ldb (byte 6 12) code))
                         (logior #x80 (ldb (byte 6 6) code))
                         (logior #x80 (ldb (byte 6 0) code)))))))

(define-decoder :utf-8b ()
  (with-next-unit (c1)
    (cond ((or ascii (< c1 #x80))
           (last-point c1))
          ((< c1 #xC2)
           (last-utf-8b-invalid-points c1))
          ((or latin1 (< c1 #xE0))
           (let ((high (ash (ldb (byte 5 0) c1) 6)))
             (with-next-utf-8b-continuation-unit (c1) (c2)
               (when (< c1 #xC2)
                 (last-utf-8b-invalid-points c1 c2))
               (last-point (logior (ldb (byte 6 0) c2) high)))))
          ((or bmp (< c1 #xF0))
           (let ((high (ash (ldb (byte 4 0) c1) 12)))
             (with-next-utf-8b-continuation-units (c1) (c2 c3)
               (if (or (and (= c1 #xE0) (< c2 #xA0))
                       (and (= c1 #xED) (< #x9F c2)))
                   (last-utf-8b-invalid-points c1 c2 c3)
                   (last-point (logior (ldb (byte 6 0) c3)
                                       (ash (ldb (byte 6 0) c2) 6)
                                       high))))))
          ((< c1 #xF5)
           (let ((high (ash (ldb (byte 3 0) c1) 18)))
             (with-next-utf-8b-continuation-units (c1) (c2 c3 c4)
               (if (or (and (= c1 #xF0) (< c2 #x90))
                       (and (= c1 #xF4) (< #x8F c2)))
                   (last-utf-8b-invalid-points c1 c2 c3 c4)
                   (logior (ldb (byte 6 0) c4)
                           (ash (ldb (byte 6 0) c3) 6)
                           (ash (ldb (byte 6 0) c2) 12)
                           high)))))
          (t (last-utf-8b-invalid-points c1)))))


(define-encoder :wtf-8 ()  ; like WTF-16
  (with-next-point (code)
    (cond ((or ascii (< code #x80))
           (last-unit code))
          ((or latin1 (< code #x800))
           (last-units (logior #xC0 (ldb (byte 5 6) code))
                       (logior #x80 (ldb (byte 6 0) code))))
          ((and (not surrogate) (= #xD800 (surrogate-tag code)))
           (let ((high (ash (- code #xD7F7) 10)))
             (with-next-point (low)
               (let ((code (+ low high)))
                 (last-units (logior #xF0 (ldb (byte 3 18) code))
                             (logior #x80 (ldb (byte 6 12) code))
                             (logior #x80 (ldb (byte 6 6) code))
                             (logior #x80 (ldb (byte 6 0) code)))))))
          ((or bmp (< code #x10000))
           (last-units (logior #xE0 (ldb (byte 4 12) code))
                       (logior #x80 (ldb (byte 6 6) code))
                       (logior #x80 (ldb (byte 6 0) code))))
          (t (last-units (logior #xF0 (ldb (byte 3 18) code))
                         (logior #x80 (ldb (byte 6 12) code))
                         (logior #x80 (ldb (byte 6 6) code))
                         (logior #x80 (ldb (byte 6 0) code)))))))

(define-decoder :wtf-8 ()
  (with-next-unit (c1)
    (cond ((or ascii (< c1 #x80))
           (last-point c1))
          ((< c1 #xC0)
           (coder-error "Unexpected UTF-8 continuation byte: #x~2,'0X" c1))
          ((or latin1 (< c1 #xE0))
           (let ((high (ash (ldb (byte 5 0) c1) 6)))
             (with-next-utf-8-continuation-unit (c2)
               (when (and (not (or overlong unchecked)) (< c1 #xC2))
                 (coder-cerror "Overlong UTF-8 sequence:~@{ #x~2,'0X~}" c1 c2))
               (last-point (logior (ldb (byte 6 0) c2) high)))))
          ((or bmp (< c1 #xF0))
           (let ((high (ash (ldb (byte 4 0) c1) 12)))
             (with-next-utf-8-continuation-units (c2 c3)
               (when (and (not (or overlong unchecked)) (= c1 #xE0) (< c2 #xA0))
                 (coder-cerror "Overlong UTF-8 sequence:~@{ #x~2,'0X~}" c1 c2 c3))
               (last-point (logior (ldb (byte 6 0) c3)
                                   (ash (ldb (byte 6 0) c2) 6)
                                   high)))))
          ((or unchecked (< c1 #xF8))
           (let ((high (ash (ldb (byte 3 0) c1) 18)))
             (with-next-utf-8-continuation-units (c2 c3 c4)
               (when (and (not (or overlong unchecked)) (= c1 #xF0) (< c2 #x90))
                 (coder-cerror "Overlong UTF-8 sequence:~@{ #x~2,'0X~}" c1 c2 c3 c4))
               (let ((code (logior (ldb (byte 6 0) c4)
                                   (ash (ldb (byte 6 0) c3) 6)
                                   (ash (ldb (byte 6 0) c2) 12)
                                   high)))
                 (if (or unchecked (< code #x110000))
                     (last-point code)
                     (coder-error "Code point out of range: #x~6,'0X" code))))))
          (t (coder-error "Invalid UTF-8 starting byte: #x~2,'0X" c1)))))


(define-encoder :cesu-8 ()  ; like UTF-8, but use two-by-three sequence for points above bmp
  (with-next-point (code)
    (cond ((or ascii (< code #x80))
           (last-unit code))
          ((or latin1 (< code #x800))
           (last-units (logior #xC0 (ldb (byte 5 6) code))
                       (logior #x80 (ldb (byte 6 0) code))))
          ((or bmp (< code #x10000))
           (unless (or surrogate unchecked (< code #xD800) (< #xDFFF code))
             (coder-cerror "Surrogate character: #x~4,'0X"))
           (last-units (logior #xE0 (ldb (byte 4 12) code))
                       (logior #x80 (ldb (byte 6 6) code))
                       (logior #x80 (ldb (byte 6 0) code))))
          (t
           (let ((code (the (unsigned-byte 20) (- code #x10000))))
             (last-units #xED
                         (logior #xA0 (ldb (byte 4 16) code))
                         (logior #x80 (ldb (byte 6 10) code))
                         #xED
                         (logior #xB0 (ldb (byte 4 6) code))
                         (logior #x80 (ldb (byte 6 0) code))))))))

(define-decoder :cesu-8 ()
  (with-next-unit (c1)
    (cond ((or ascii (< c1 #x80))
           (when (and (not unchecked) (= c1 #x0))
             (coder-cerror "Unexpected embedded NULL in modified UTF-8: #x~2',0X"))
           (last-point c1))
          ((< c1 #xC0)
           (coder-error "Unexpected UTF-8 continuation byte: #x~2,'0X" c1))
          ((or latin1 (< c1 #xE0))
           (let ((high (ash (ldb (byte 5 0) c1) 6)))
             (with-next-utf-8-continuation-unit (c2)
               (when (and (not (or overlong unchecked)) (< c1 #xC2))
                 (coder-cerror "Overlong UTF-8 sequence:~@{ #x~2,'0X~}" c1 c2))
               (last-point (logior (ldb (byte 6 0) c2) high)))))
          ((or bmp (< c1 #xF0))
           (let ((high (ash (ldb (byte 4 0) c1) 12)))
             (with-next-utf-8-continuation-units (c2 c3)
               (when (and (not (or overlong unchecked)) (= c1 #xE0) (< c2 #xA0))
                 (coder-cerror "Overlong UTF-8 sequence:~@{ #x~2,'0X~}" c1 c2 c3))
               (let* ((code (logior (ldb (byte 6 0) c3)
                                    (ash (ldb (byte 6 0) c2) 6)
                                    high))
                      (tag (surrogate-tag code)))
                 (cond
                   ((= #xD800 tag)
                    (let ((high (ash (- code #xD7C0) 10)))
                      (with-next-unit (c4)
                        (if (or unchecked (= c4 #xED))
                            (with-next-utf-8-continuation-units (c5 c6)
                              (cond ((or unchecked (< #xAF c5))
                                     (last-point (logior
                                                  (ldb (byte 6 0) c6)
                                                  (ash (ldb (byte 4 0) c4) 6)
                                                  high)))
                                    ((< c5 #xA0) (coder-error "Unexpected UTF-8 sequence:~@{ #x~2,'0X~}" c4 c5 c6))
                                    (t (coder-error "Unexpected high surrogate sequence:~@{ #x~2,'0X~}" c4 c5 c6))))
                            (coder-error "Expected #xED, found: #x~2,'0X")))))
                   ((and (not unchecked) (= #xDC00 tag))
                    (coder-error "Unexpected low surrogate sequence:~@{ #x~2,'0X~}" c1 c2 c3))
                   (t (last-point code)))))))
          (t (coder-error "Invalid UTF-8 starting byte: #x~2,'0X" c1)))))


(define-encoder :modified-utf-8 ()  ; like CESU-8, but with overlong encoding of NULL
  (with-next-point (code)
    (cond ((zerop code)
           (last-units #xC0 #x80))
          ((or ascii (< code #x80))
           (last-unit code))
          ((or latin1 (< code #x800))
           (last-units (logior #xC0 (ldb (byte 5 6) code))
                       (logior #x80 (ldb (byte 6 0) code))))
          ((or bmp (< code #x10000))
           (unless (or surrogate unchecked (< code #xD800) (< #xDFFF code))
             (coder-cerror "Surrogate character: #x~4,'0X"))
           (last-units (logior #xE0 (ldb (byte 4 12) code))
                       (logior #x80 (ldb (byte 6 6) code))
                       (logior #x80 (ldb (byte 6 0) code))))
          (t
           (let ((code (the (unsigned-byte 20) (- code #x10000))))
             (last-units #xED
                         (logior #xA0 (ldb (byte 4 16) code))
                         (logior #x80 (ldb (byte 6 10) code))
                         #xED
                         (logior #xB0 (ldb (byte 4 6) code))
                         (logior #x80 (ldb (byte 6 0) code))))))))

(define-decoder :modified-utf-8 ()
  (with-next-unit (c1)
    (cond ((or ascii (< c1 #x80))
           (when (and (not unchecked) (= c1 #x0))
             (coder-cerror "Unexpected embedded NULL in modified UTF-8: #x~2',0X"))
           (last-point c1))
          ((< c1 #xC0)
           (coder-error "Unexpected UTF-8 continuation byte: #x~2,'0X" c1))
          ((= c1 #xC0)
           (with-next-utf-8-continuation-unit (c2)
             (unless (or unchecked (= c2 #x80))
               (coder-cerror "Expected #x80, found an overlong UTF-8 sequence:~@{ #x~2,'0X~}" c1 c2)
               (last-point (logior (ldb (byte 6 0) c2)
                                   (ash (ldb (byte 5 0) c1) 6))))
             (last-point 0)))
          ((or latin1 (< c1 #xE0))
           (let ((high (ash (ldb (byte 5 0) c1) 6)))
             (with-next-utf-8-continuation-unit (c2)
               (when (and (not (or overlong unchecked)) (= c1 #xC1))
                 (coder-cerror "Overlong UTF-8 sequence:~@{ #x~2,'0X~}" c1 c2))
               (last-point (logior (ldb (byte 6 0) c2) high)))))
          ((or bmp (< c1 #xF0))
           (let ((high (ash (ldb (byte 4 0) c1) 12)))
             (with-next-utf-8-continuation-units (c2 c3)
               (when (and (not (or overlong unchecked)) (= c1 #xE0) (< c2 #xA0))
                 (coder-cerror "Overlong UTF-8 sequence:~@{ #x~2,'0X~}" c1 c2 c3))
               (let* ((code (logior (ldb (byte 6 0) c3)
                                    (ash (ldb (byte 6 0) c2) 6)
                                    high))
                      (tag (surrogate-tag code)))
                 (cond
                   ((= #xD800 tag)
                    (let ((high (ash (- code #xD7C0) 10)))
                      (with-next-unit (c4)
                        (if (or unchecked (= c4 #xED))
                            (with-next-utf-8-continuation-units (c5 c6)
                              (cond ((or unchecked (< #xAF c5))
                                     (last-point (logior
                                                  (ldb (byte 6 0) c6)
                                                  (ash (ldb (byte 4 0) c4) 6)
                                                  high)))
                                    ((< c5 #xA0) (coder-error "Unexpected UTF-8 sequence:~@{ #x~2,'0X~}" c4 c5 c6))
                                    (t (coder-error "Unexpected high surrogate sequence:~@{ #x~2,'0X~}" c4 c5 c6))))
                            (coder-error "Expected #xED, found: #x~2,'0X")))))
                   ((and (not unchecked) (= #xDC00 tag))
                    (coder-error "Unexpected low surrogate sequence:~@{ #x~2,'0X~}" c1 c2 c3))
                   (t (last-point code)))))))
          (t (coder-error "Invalid UTF-8 starting byte: #x~2,'0X" c1)))))
