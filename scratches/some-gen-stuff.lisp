(defpackage #:gr.synthy
  (:use #:cl #:sb-ext))
(in-package #:gr.synthy)

(ql:quickload '(:harmony :cl-mixed :sketch :stopclock :sketch-fit))

(progn
  (add-package-local-nickname '#:mixed   '#:org.shirakumo.fraf.mixed)
  (add-package-local-nickname '#:harmony '#:org.shirakumo.fraf.harmony)
  (add-package-local-nickname '#:s       '#:sketch)
  (add-package-local-nickname '#:sf      '#:sketch-fit)
  (add-package-local-nickname '#:sc      '#:stopclock))

(defun h-restart (&optional (drain :pulse))
  (when harmony:*server*
    (org.shirakumo.fraf.harmony.user:stop harmony:*server*)
    (setf harmony:*server* nil))
  (harmony:maybe-start-simple-server :drain drain :mixers `(:effect)))

(defun fg-sin (phase)
  (sin (* 2 pi phase)))

(defun fg-cos (phase)
  (cos (* 2 pi phase)))

(defun fg-square (phase)
  (if (< phase 0.5) 1 -1))

(defun fg-triangle (phase)
  (cond ((< phase 0.25) (* 4 phase))
        ((< phase 0.75) (* 4 (- 1/2 phase)))
        (T              (* 4 (1- phase)))))

(defun fg-sawtooth (phase)
  (1- (* phase 2)))

(defun fg-noise (x)
  (declare (ignore x))
  (- (random 2.0) 1))

(defmacro mix ((phase) &body clauses &aux (!x (gensym "x"))
                                          (!ci (loop repeat (length clauses)
                                                     collect (gensym "coef")))
                                          (!cs (gensym "coef-sum")))
  `(let* ((,!x ,phase)
          ,@(loop for !c in !ci
                  for (coef) in clauses
                  collect `(,!c ,coef))
          (,!cs (+ ,@(loop for !c in !ci
                           collect `(abs ,!c)))))
     (if (zerop ,!cs)
         0
         (/ (+ ,@(loop for (coef fun) in clauses
                       for !c in !ci
                       collect `(* ,!c (,fun ,!x))))
            ,!cs))))

(progn
  (defparameter *sin*      1)
  (defparameter *sawtooth* 0)
  (defparameter *square*   -1/2)
  (defparameter *triangle* 1)
  (defparameter *noise*    0))

(defun fg-try (x)
  (mix (x)
    (*sin*      fg-sin)
    (*sawtooth* fg-sawtooth)
    (*square*   fg-square)
    (*triangle* fg-triangle)
    (*noise*    fg-noise)))

(defun fg-synth (time &optional release-p)
  (if release-p
      0
      (min 1 (max 0 (/ (- 1 time) 1/2)))))

(defun fd-adsr (&optional (attack 0.3)
                          (decay 0.5)
                          (sustain 0.3)
                          (release 1))
  (lambda (time &optional release-p)
    (cond ((< time attack)
           (if (zerop attack)
               1
               (/ time attack)))
          ((< (- time attack) decay)
           (- 1 (* (- 1 sustain) (if (zerop decay)
                                     1
                                     (/ (- time attack) decay)))))
          ((and release-p (<= release-p time))
           (let ((result (max 0 (* sustain (- 1 (if (zerop release)
                                                    1
                                                    (/ (- time release-p)
                                                       release)))))))
             (values result (zerop result))))
          (T (values sustain (zerop sustain))))))

(progn
  (defparameter *attack*  0)
  (defparameter *decay*   1/4)
  (defparameter *sustain* 0)
  (defparameter *release* 0))

(defun fg-global-adsr (time &optional release-p)
  (funcall (fd-adsr *attack* *decay* *sustain* *release*) time release-p))

(defparameter *release-point* 1)
(defparameter *fg-synth* 'fg-global-adsr)

(defparameter *volume* 1)

(defun fg-volume ()
  *volume*)

(defclass fun-generator (mixed:virtual)
  ((func :initarg :function :initform #'fg-sin :accessor func)
   (offset :initform 0 :accessor offset)
   (samplerate :initarg :samplerate :initform 44100 :accessor samplerate)
   (note :initarg :note :initform 0 :accessor note)
   (frequency :initarg :frequency :initform 440 :accessor frequency)
   (synth :initarg :synth :initform #'fg-synth :accessor synth)
   (volume :initarg :volume :initform #'fg-volume :accessor volume)
   (release-p :initarg :release-p :initform NIL :accessor release-p)
   (fg-endp :initform NIL :accessor fg-endp)))

(defparameter *fg-to-close* ())

(defun close-silents ()
  (loop while *fg-to-close*
        do (harmony:stop (pop *fg-to-close*))
        count 1))

(defmethod fg-reset ((fg fun-generator))
  (mixed:seek fg 0)
  (setf (fg-endp fg) NIL)
  (setf (release-p fg) NIL))

#+debug
(defparameter *clocks* (make-hash-table))

(defmethod mixed:mix ((fg fun-generator))
  (with-slots (func offset samplerate frequency synth volume release-p fg-endp) fg
    #+debug
    (when (zerop offset)
      (print (list :start-at (float (stopclock:time (gethash (note fg)
                                                             *clocks*
                                                             (sc:make-clock))))
                   :note (note fg))))
    (when (and (not fg-endp)
               release-p
               (nth-value 1 (funcall synth
                                     (/ offset samplerate)
                                     (/ release-p samplerate))))
      (setf fg-endp T)
      (push fg *fg-to-close*))
    (mixed:with-buffer-tx (data start size (aref (mixed:outputs fg) 0) :direction :output)
      (mixed:with-buffer-tx (data-2 start-2 size-2 (aref (mixed:outputs fg) 1) :direction :output)
        (setf size (min size size-2))
        (setf size-2 (min size size-2))
        (loop repeat size
              for index from start
              for index-2 from start-2
              for offset from offset
              for res = (coerce (* (funcall func (/ (mod (* offset frequency) samplerate)
                                                    samplerate))
                                   (funcall synth
                                            (/ offset samplerate)
                                            (and release-p (/ release-p samplerate)))
                                   (funcall volume))
                                'single-float)
              do (assert (<= -1.0 res 1.0))
                 (setf (aref data index) res
                       (aref data-2 index-2) res))
        (incf offset (min size size-2))
        (mixed:finish size-2))
      (mixeD:finish size))))

(defmethod mixed:seek ((segment fun-generator) position &key)
  (setf (offset segment) position)
  segment)

(defun release (fg)
  (setf (release-p fg) (offset fg)))

(defmethod mixed:info ((segment fun-generator))
  (list :name "fun-generator"
        :description "Simple two channel function generator"
        :flags 0
        :min-inputs 0
        :max-inputs 0
        :outputs 2
        :fields ()))

(defparameter *colors*
  `(:pressed  ,(s:gray 0.2)
    :open     ,(s:gray 0.7)
    :back     ,s:+black+
    :back-2   ,(s:gray 0.6)
    :font     ,s:+blue+
    :cback    ,s:+black+
    :fun      ,s:+red+
    :border   ,s:+white+
    :statefl  ,s:+magenta+
    :-statefl ,s:+green+
    :funkey   ,s:+yellow+))

(defun c (name)
  (getf *colors* name))

(defun draw-fun (fun x y w h &optional (dx 1/100) (maxx 1) args)
  (s:with-current-matrix
    (s:translate x y)
    (s:with-pen (s:make-pen :stroke (c :fun))
      (apply #'s:polyline
             (loop for x from 0 to maxx by dx
                   collect (/ (* x w) maxx)
                   collect (* h (/ (1+ (- (apply fun x args)))
                                   2)))))))

(defmacro with-margin ((var-x var-w c &optional (c2 c)) &body body
                       &aux (!c (gensym)) (!c2 (gensym)))
  `(let* ((,!c ,c)
          (,!c2 ,c2))
     (let ((,var-x (+ ,var-x (* ,!c ,var-w)))
           (,var-w (- ,var-w (* (+ ,!c2 ,!c) ,var-w))))
       ,@body)))

(defun draw-chooser (state fun x y w h &optional (dx 1/100) (maxx 1) &rest args)
  (with-margin (x w 1/20)
    (with-margin (y h 1/20)
      (s:with-pen (s:make-pen :fill (c :cback))
        (s:rect x y w h))
      (with-margin (x w 1/20)
        (with-margin (y h 1/20 (+ 1/4 1/40))
          (s:with-pen (s:make-pen :stroke (c :border))
            (s:rect x y w h))
          (with-margin (x w 1/20)
            (with-margin (y h 1/20)
              (draw-fun fun x y w h dx maxx args)))))
      (with-margin (x w 1/20)
        (with-margin (y h (+ 3/4 1/40) 1/20)
          (s:with-pen (s:make-pen :stroke (c :border))
            (s:rect x y w h))
          (when (plusp state)
            (s:with-pen (s:make-pen :fill (c :statefl))
              (s:rect x y (* w state) h)))
          (when (minusp state)
            (s:with-pen (s:make-pen :fill (c :-statefl))
              (s:rect x y (* w (- state)) h))))))))

(defun new-state (mx my x y w h &optional (coef 1) (dd 8))
  (with-margin (x w 1/20)
    (with-margin (y h 1/20)
      (when (and (<= x mx (+ x w))
                 (<= y my (+ y h)))
        (* coef
           (/ (min dd (max 0 (round (* dd (/ (- mx x) w)))))
              dd))))))

(defparameter *rows*
  `(((:scancode-1 :scancode-2 :scancode-3 :scancode-4 :scancode-5 :scancode-6
      :scancode-7 :scancode-8 :scancode-9 :scancode-0 :scancode-minus :scancode-equals)
     "1234567890-="
     0)
    ((:scancode-q :scancode-w :scancode-e :scancode-r :scancode-t :scancode-y
      :scancode-u :scancode-i :scancode-o :scancode-p :scancode-leftbracket :scancode-rightbracket)
     "qwertyuiop[]"
     1/2)
    ((:scancode-a :scancode-s :scancode-d :scancode-f :scancode-g :scancode-h
      :scancode-j :scancode-k :scancode-l :scancode-semicolon :scancode-apostrophe :scancode-return)
     "asdfghjkl;'⏎"
     3/4)
    ((:scancode-z :scancode-x :scancode-c :scancode-v :scancode-b :scancode-n
      :scancode-m :scancode-comma :scancode-period :scancode-slash :scancode-rshift)
     "zxcvbnm,./⇧"
     5/4)))

(defparameter *unit* 50)

(defparameter *margin* 1/10)

(defun key-width (code)
  (case code
    (:scancode-return 7/4)
    (:scancode-rshift 9/4)
    (T 1)))

(defparameter *key-down-p* 'current-key-down-p)

(defun draw-key (code char)
  (let* ((state (if (funcall *key-down-p* code) :pressed :open))
         (x (* *unit* *margin*))
         (y x)
         (h (* *unit* (- 1 (* 2 *margin*))))
         (w (* *unit* (- (key-width code) (* 2 *margin*))))
         (s (princ-to-string char))
         (font-size (/ *unit* 2)))
    (case code
      (:scancode-return
       (setf s "Enter")
       (setf font-size (* *unit* 2/5)))
      (:scancode-rshift
       (setf s "Shift")
       (setf font-size (* *unit* 2/5))))
    (s:with-pen (s:make-pen :fill (c state))
      (s:rect x y w h))
    (s:with-font (s:make-font :color (c (let* ((onte (gethash code *ontes*))
                                               (note (and onte (note onte))))
                                          (if (and note
                                                   (member (mod note 12)
                                                           '(3)))
                                              :funkey
                                              :font)))
                              :size font-size)
      (s:text s (/ w 2) (/ h 3)))))

(defun draw-row (row)
  (destructuring-bind (keys letters shift) row
    (s:with-current-matrix
      (s:translate (* *unit* shift) 0)
      (loop for char across letters
            for code in keys
            do (draw-key code char)
               (s:translate (* (key-width code) *unit*) 0)))))

(defun keyboard-width ()
  (reduce #'max (mapcar (lambda (row)
                          (+ (reduce #'+ (mapcar #'key-width (car row)))
                             (caddr row)))
                        *rows*)))

(defun keyboard-height ()
  (length *rows*))

(defun draw-keyboard (&key max-width max-height)
  (let ((*unit* *unit*))
    (when max-width (alexandria:minf *unit* (/ max-width (keyboard-width))))
    (when max-height (alexandria:minf *unit* (/ max-height (+ 2 (keyboard-height)))))
    (s:with-pen (s:make-pen :fill (c :back-2))
      (s:rect 0 0
              (* *unit* (keyboard-width))
              (* *unit* (keyboard-height))))
    (s:with-current-matrix
      (dolist (row *rows*)
        (draw-row row)
        (s:translate 0 *unit*)))))

(defun make-onte (k)
  (make-instance 'fun-generator
                 :function 'fg-try
                 :frequency (* 440 (expt 2 (/ k 12)))
                 :synth *fg-synth*
                 :note k))

(defun play-onte (onte)
  (fg-reset onte)
  #+debug
  (setf (gethash (note onte) *clocks*) (sc:make-clock))
  (harmony:play onte :reset T :mixer :effect))

(defun stop-onte (onte)
  (release onte))

(defparameter *ontes* (make-hash-table))

(defun make-ontes (&optional (base -20) (d-base 5))
  (loop for (row-keys) in *rows*
        for base from base by d-base
        do (loop for key in row-keys
                 for k from base
                 do (setf (gethash key *ontes*)
                          (make-onte k)))))

(defun play-code (code)
  (let ((onte (gethash code *ontes*)))
    (when onte
      (play-onte onte))))

(defun stop-code (code)
  (let ((onte (gethash code *ontes*)))
    (when onte
      (stop-onte onte))))

(progn
  (defparameter *mode* :none)
  (defparameter *clock* (sc:make-clock :paused NIL))
  (defparameter *rec-time* 8)
  (defparameter *recording* ())
  (defparameter *last-time* 0)
  (defparameter *next-index* 0))

(defun save-recording (keysym state)
  (push (list (mod (sc:time *clock*) *rec-time*)
              (sdl2:scancode keysym)
              state
              (sdl2:scancode-value keysym))
        *recording*)
  (setf *recording* (sort *recording* #'< :key #'car)))

(defun play-recording (code state scancode-value)
  (kit.sdl2::keystate-update-value *tracker* state nil scancode-value)
  (case state
    (:keydown (play-code code))
    (:keyup   (stop-code code))))

(defun play-by-clock ()
  (when (eq *mode* :play)
    (let ((time (mod (sc:time *clock*) *rec-time*)))
      (when (> *last-time* time)
        (setf *next-index* 0))
      (setf *last-time* time)
      (loop while (< *next-index* (length *recording*))
            for (rec-time . recording) = (nth *next-index* *recording*)
            while (>= time rec-time)
            do (apply #'play-recording recording)
               (incf *next-index*)))))

(s:defsketch vroom ()
  (sf:fit 800 800 s:width s:height)
  (let ((s:width 800)
        (s:height 800))
    (close-silents)
    (s:background (c :back))
    (let ((*unit* 200))
      (draw-keyboard :max-width s:width
                     :max-height (* 1/2 s:height)))
    (let ((x (/ s:width 4))
          (y (/ s:height 6)))
      (draw-chooser *volume*   'fg-try (* 1/2 x) (* 2 y) x y)
      (draw-chooser *sin*      'fg-sin           0 (* 3 y) x y)
      (draw-chooser *sawtooth* 'fg-sawtooth      x (* 3 y) x y)
      (draw-chooser *square*   'fg-square        0 (* 4 y) x y)
      (draw-chooser *triangle* 'fg-triangle      x (* 4 y) x y)

      (draw-chooser *noise*    'fg-noise         0 (* 5 y) x y)

      (draw-chooser (/ *release-point* 2) *fg-synth* (* 3/2 x) (* 2 y) (* 2 x) y 1/100 2 *release-point*)

      (draw-chooser *attack* (constantly 0) (* 2 x) (* 3 y) x y)
      (s:with-font (s:make-font :color (c :fun) :align :center)
        (s:text "Attack" (* (+ 1/2 2) x) (* (+ 1/2 3) y)))
      (draw-chooser *decay* (constantly 0) (* 3 x) (* 3 y) x y)
      (s:with-font (s:make-font :color (c :fun) :align :center)
        (s:text "Decay" (* (+ 1/2 3) x) (* (+ 1/2 3) y)))
      (draw-chooser *sustain* (constantly 0) (* 2 x) (* 4 y) x y)
      (s:with-font (s:make-font :color (c :fun) :align :center)
        (s:text "Sustain" (* (+ 1/2 2) x) (* (+ 1/2 4) y)))
      (draw-chooser *release* (constantly 0) (* 3 x) (* 4 y) x y)
      (s:with-font (s:make-font :color (c :fun) :align :center)
        (s:text "Release" (* (+ 1/2 3) x) (* (+ 1/2 4) y)))
      (draw-chooser (/ *rec-time* 8) (constantly 0) (* 3/2 x) (* 5 y) x y)
      (s:with-font (s:make-font :color (c :fun) :align :center)
        (s:text "Record time" (* (+ 1/2 3/2) x) (* (+ 1/2 5) y)))
      (draw-chooser (/ (mod (sc:time *clock*) *rec-time*) *rec-time*) (constantly 0) (* 5/2 x) (* 5 y) x y)
      (s:with-font (s:make-font :color (c :fun) :align :center)
        (s:text "Current time" (* (+ 1/2 5/2) x) (* (+ 1/2 5) y)))))
  (s:with-font (s:make-font :color (c :fun) :align :center)
    (s:text (case *mode*
              (:play "Playing")
              (:record "Recording")
              (:none "Standby"))
            50 300)
    (play-by-clock)
    (s:text (princ-to-string *next-index*) 400 400)))

(defparameter *tracker* (make-instance 'kit.sdl2:keystate-tracker))

(defmethod kit.sdl2:keyboard-event ((app vroom) state ts rep? keysym)
  (unless rep?
    (case (sdl2:scancode keysym)
      (:scancode-kp-5 (when (eq state :keydown)
                        (sc:toggle *clock*)))
      (:scancode-kp-1 (when (eq state :keydown)
                        (setf *mode* :record
                              *recording* nil)))
      (:scancode-kp-2 (when (eq state :keydown)
                        (setf *mode* :none)))
      (:scancode-kp-3 (when (eq state :keydown)
                        (setf *mode* :play)))
      (T
       (unless (eq *mode* :play)
         (kit.sdl2:keystate-update *tracker* state rep? keysym)
         (when (eq *mode* :record)
           (save-recording keysym state))
         (case state
           (:keydown (play-code (sdl2:scancode keysym)))
           (:keyup   (stop-code (sdl2:scancode keysym)))))))))

(defmethod kit.sdl2:mousebutton-event ((app vroom) st ts button mx my)
  (when (eq st :mousebuttondown)
    (destructuring-bind (mx my) (sf:fit-point mx my
                                              800 800
                                              (s:sketch-width app) (s:sketch-height app))
      (let ((s:width 800)
            (s:height 800))
        (let ((neg (if (= button 3) -1 1)))
          (let ((x (/ s:width 4))
                (y (/ s:height 6)))
            (setf *volume*        (or (new-state mx my (* 1/2 x) (* 2 y)      x  y   1)    *volume*))
            (setf *sin*           (or (new-state mx my        0  (* 3 y)      x  y neg)    *sin*))
            (setf *sawtooth*      (or (new-state mx my        x  (* 3 y)      x  y neg)    *sawtooth*))
            (setf *square*        (or (new-state mx my        0  (* 4 y)      x  y neg)    *square*))
            (setf *triangle*      (or (new-state mx my        x  (* 4 y)      x  y neg)    *triangle*))
            (setf *noise*         (or (new-state mx my        0  (* 5 y)      x  y neg)    *noise*))
            (setf *attack*        (or (new-state mx my (*   2 x) (* 3 y)      x  y   1 20) *attack*))
            (setf *decay*         (or (new-state mx my (*   3 x) (* 3 y)      x  y   1 20) *decay*))
            (setf *sustain*       (or (new-state mx my (*   2 x) (* 4 y)      x  y   1 20) *sustain*))
            (setf *release*       (or (new-state mx my (*   3 x) (* 4 y)      x  y   1 20) *release*))
            (setf *release-point* (or (new-state mx my (* 3/2 x) (* 2 y) (* 2 x) y   2 50) *release-point*))
            (setf *rec-time*      (or (new-state mx my (* 3/2 x) (* 5 y)      x  y   8 8) *rec-time*))))))))

(defmethod kit.sdl2:close-window :after ((app vroom))
  (harmony:stop harmony:*server*))

(defun current-key-down-p (code)
  (not (kit.sdl2:key-down-p *tracker* code)))

(setf *attack* 0
      *decay* 1/10
      *sustain* 0
      *release* 0
      *sin* 1
      *sawtooth* 0
      *square* 1/4
      *triangle* 1/4
      *noise* 1/4)

(progn
  (h-restart)
  (make-ontes -21 8)
  (make-instance 'vroom :resizable t :width 750 :height 800))

(defun note-file (k)
  (pathname (format nil "~~/Downloads/temp/note-~a.wav" k)))

(ql:quickload :cl-mixed-wav)

(defun save-note (k &key (encoding :uint8))
  (let ((file (note-file k)))
    (mixed:with-objects ((note (make-instance 'fun-generator
                                              :function 'fg-try
                                              :frequency (* 440 (expt 2 (/ k 12)))
                                              :synth *fg-synth*
                                              :note k))
                         (drain (mixed:make-packer :encoding encoding))
                         (out (make-instance 'org.shirakumo.fraf.mixed.wav:file-drain
                                             :file (pathname file) :pack drain)))
      (mixed:with-buffers 500 (l r)
        (mixed:connect note :left drain :left l)
        (mixed:connect note :right drain :right r)
        (fg-reset note)
        (release note)
        (mixed:with-chain chain (note drain out)
          (format T "~&Writing to ~a with ~d channels @ ~dHz, ~a~%"
                  file (mixed:channels drain) (mixed:samplerate drain) (mixed:encoding drain))
          (loop until (fg-endp note)
                do (mixed:mix chain))
          (format T "Wrote ~d frames." (mixed:frame-position out)))))))

(defun save-notes (from to &rest args &key encoding)
  (declare (ignorable encoding))
  (loop for k from from to to
        do (apply #'save-note k args)))

#+nil
(save-notes (- 3 24) (+ 3 24) :encoding :int16)

#+nil
(progn
  (s:defsketch chooser ()
    (let ((x (/ s:width 2))
          (y (/ s:height 2)))
      (draw-chooser *sin*      'fg-sin      0 0 x y)
      (draw-chooser *sawtooth* 'fg-sawtooth x 0 x y)
      (draw-chooser *square*   'fg-square   0 y x y)
      (draw-chooser *triangle* 'fg-triangle x y x y)))

  (defmethod kit.sdl2:keyboard-event ((app chooser) state ts rep? keysym)
    (when (and (eq state :keydown)
               (not rep?)
               (eq (sdl2:scancode keysym) :scancode-r))
      (h-restart)))

  (defmethod kit.sdl2:mousebutton-event ((app chooser) st ts button mx my)
    (when (eq st :mousebuttondown)
      (let ((x (/ (s:sketch-width app) 2))
            (y (/ (s:sketch-height app) 2)))
        (setf *sin*      (or (new-state mx my 0 0 x y) *sin*))
        (setf *sawtooth* (or (new-state mx my x 0 x y) *sawtooth*))
        (setf *square*   (or (new-state mx my 0 y x y) *square*))
        (setf *triangle* (or (new-state mx my x y x y) *triangle*)))))

  (defmethod kit.sdl2:mousemotion-event ((app chooser) ts button-mask mx my xrel yrel)
    (unless (zerop button-mask)
      (let ((x (/ (s:sketch-width app) 2))
            (y (/ (s:sketch-height app) 2)))
        (setf *sin*      (or (new-state mx my 0 0 x y) *sin*))
        (setf *sawtooth* (or (new-state mx my x 0 x y) *sawtooth*))
        (setf *square*   (or (new-state mx my 0 y x y) *square*))
        (setf *triangle* (or (new-state mx my x y x y) *triangle*))))))
