(defpackage #:cloggy-key
  (:use #:cl #:clog)
  (:export :start-recording))

(in-package :cloggy-key)

(defun on-new-window (body)
  (create-div body :content "Just <b>type</b>!")
  (set-on-key-press body (let ((mood (create-audio body :source "/demo/mood.mp3" :controls nil :autoloop t :autoplay t)))
                           (play-media mood)
                           (lambda (obj params)
                             (let* ((txt (format nil "<tt>Char: ~a KChar: ~a Code: ~a KCode: ~a</tt>"
                                                 (code-char (getf params :char-code))
                                                 (getf params :key)
                                                 (getf params :char-code)
                                                 (getf params :key-code)))
                                    (div (create-div obj :content txt)))
                               (play-media (create-audio body :source "/demo/eat.wav" :controls nil))
                               (sleep 1)
                               (destroy div))))))

(defun start-recording ()
  "Start recording."
  (initialize 'on-new-window)
  (open-browser))
