(ql:quickload '(:harmony :cl-mixed :sketch :stopclock))

(progn
  (add-package-local-nickname '#:mixed   '#:org.shirakumo.fraf.mixed)
  (add-package-local-nickname '#:harmony '#:org.shirakumo.fraf.harmony)
  (add-package-local-nickname '#:s       '#:sketch))

(defun h-restart (&optional (drain :alsa))
  (when harmony:*server*
    (org.shirakumo.fraf.harmony.user:stop harmony:*server*)
    (setf harmony:*server* nil))
  (harmony:maybe-start-simple-server :drain drain))

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
  (defparameter *sin*      3/8)
  (defparameter *sawtooth* 1/8)
  (defparameter *square*   0)
  (defparameter *triangle* 1/8))

(defun fg-try (x)
  (mix (x)
    (*sin*      fg-sin)
    (*sawtooth* fg-sawtooth)
    (*square*   fg-square)
    (*triangle* fg-triangle)))

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
          (release-p (max 0 (* sustain (- 1 (if (zerop release)
                                                1
                                                (/ (- time (max release-p (+ attack decay)))
                                                   release))))))
          (T sustain))))

(defparameter *fg-synth* (fd-adsr 0 0.5 0 0))

;(make-ontes)

(defclass fun-generator (mixed:virtual)
  ((func :initarg :function :initform #'fg-sin :accessor func)
   (offset :initform 0 :accessor offset)
   (samplerate :initarg :samplerate :initform 44100 :accessor samplerate)
   (note :initarg :note :initform 0 :accessor note)
   (frequency :initarg :frequency :initform 440 :accessor frequency)
   (synth :initarg :synth :initform #'fg-synth :accessor synth)
   (volume :initarg :volume :initform 0.5 :accessor volume)
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

(defparameter *clocks* (make-hash-table))

(defmethod mixed:mix ((fg fun-generator))
  (with-slots (func offset samplerate frequency synth volume release-p fg-endp) fg
    (when (and (not fg-endp)
               release-p
               (zerop (funcall synth
                               (/ offset samplerate)
                               (/ release-p samplerate))))
      (setf fg-endp T)
      (push fg *fg-to-close*))
    (when (zerop offset)
      (print (list :start-at (float (stopclock:time (gethash (note fg)
                                                             *clocks*
                                                             (stopclock:make-clock))))
                   :note (note fg))))
    (mixed:with-buffer-tx (data start size (aref (mixed:outputs fg) 0) :direction :output)
      (loop repeat size
            for index from start
            for offset from offset
            for res = (coerce (* (funcall func (/ (mod (* offset frequency) samplerate)
                                                  samplerate))
                                 (funcall synth
                                          (/ offset samplerate)
                                          (and release-p (/ release-p samplerate)))
                                 volume)
                              'single-float)
            do (assert (<= -1.0 res 1.0))
               (setf (aref data index) res))
      (incf offset size)
      (mixed:finish size))))

(defmethod mixed:seek ((segment fun-generator) position &key)
  (setf (offset segment) position)
  segment)

(defun release (fg)
  (setf (release-p fg) (offset fg)))

(defmethod mixed:info ((segment fun-generator))
  (list :name "fun-generator"
        :description "Simple one-channel function generator"
        :flags 0
        :min-inputs 0
        :max-inputs 0
        :outputs 1
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

(defun draw-fun (fun x y w h &optional (dx 1/100) (maxx 1))
  (s:with-current-matrix
    (s:translate x y)
    (s:with-pen (s:make-pen :stroke (c :fun))
      (apply #'s:polyline
             (loop for x from 0 to maxx by dx
                   collect (/ (* x w) maxx)
                   collect (* h (/ (1+ (- (funcall fun x)))
                                   2)))))))

(defmacro with-margin ((var-x var-w c &optional (c2 c)) &body body
                       &aux (!c (gensym)) (!c2 (gensym)))
  `(let* ((,!c ,c)
          (,!c2 ,c2))
     (let ((,var-x (+ ,var-x (* ,!c ,var-w)))
           (,var-w (- ,var-w (* (+ ,!c2 ,!c) ,var-w))))
       ,@body)))

(defun draw-chooser (state fun x y w h &optional (dx 1/100))
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
              (draw-fun fun x y w h dx)))))
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
                                                           '(0)))
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
        (s:translate 0 *unit*))
      (draw-fun *fg-synth*
                (* *margin* *unit*)
                (* *margin* *unit*)
                (* *unit* (- (keyboard-width) (* 2 *margin*)))
                (* 2 (* *unit* (- 2 (* 2 *margin*))))
                (/ 10)
                10))))

(defun make-onte (k)
  (make-instance 'fun-generator
                 :function 'fg-try
                 :frequency (* 440 (expt 2 (/ k 12)))
                 :synth *fg-synth*
                 :volume 0.1
                 :note k))

(defun play-onte (onte)
  (fg-reset onte)
  (setf (gethash (note onte) *clocks*)
        (stopclock:make-clock))
  (harmony:play onte :reset T))

(defun stop-onte (onte)
  (release onte))

(defparameter *ontes* (make-hash-table))

(defun make-ontes ()
  (loop for (row-keys) in *rows*
        for base from -20 by 5
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

(s:defsketch vroom ()
  (close-silents)
  (s:background (c :back))
  (let ((*unit* 200))
    (draw-keyboard :max-width s:width
                   :max-height (* 1/2 s:height)))
  (let ((x (/ s:width 4))
        (y (/ s:height 6)))
    (draw-chooser 1 'fg-try (* 3/2 x) (* 3 y) x y)
    (draw-chooser *sin*      'fg-sin           x  (* 4 y) x y)
    (draw-chooser *sawtooth* 'fg-sawtooth (* 2 x) (* 4 y) x y)
    (draw-chooser *square*   'fg-square        x  (* 5 y) x y)
    (draw-chooser *triangle* 'fg-triangle (* 2 x) (* 5 y) x y)))

(defparameter *tracker* (make-instance 'kit.sdl2:keystate-tracker))

(defmethod kit.sdl2:keyboard-event ((app vroom) state ts rep? keysym)
  (kit.sdl2:keystate-update *tracker* state rep? keysym)
  (unless rep?
    (case state
      (:keydown (play-code (sdl2:scancode keysym)))
      (:keyup   (stop-code (sdl2:scancode keysym))))))

(defmethod kit.sdl2:mousebutton-event ((app vroom) st ts button mx my)
  (when (eq st :mousebuttondown)
    (if (= button 2)
        (let ((x (/ (s:sketch-width app) 4))
              (y (/ (s:sketch-height app) 6)))
          (setf *sin*      (print (or (new-state mx my      x  (* 4 y) x y -1) *sin*)))
          (setf *sawtooth* (or (new-state mx my (* 2 x) (* 4 y) x y -1) *sawtooth*))
          (setf *square*   (or (new-state mx my      x  (* 5 y) x y -1) *square*))
          (setf *triangle* (or (new-state mx my (* 2 x) (* 5 y) x y -1) *triangle*)))
        (let ((x (/ (s:sketch-width app) 4))
              (y (/ (s:sketch-height app) 6)))
          (setf *sin*      (or (new-state mx my      x  (* 4 y) x y) *sin*))
          (setf *sawtooth* (or (new-state mx my (* 2 x) (* 4 y) x y) *sawtooth*))
          (setf *square*   (or (new-state mx my      x  (* 5 y) x y) *square*))
          (setf *triangle* (or (new-state mx my (* 2 x) (* 5 y) x y) *triangle*))))))

(defun current-key-down-p (code)
  (not (kit.sdl2:key-down-p *tracker* code)))

(progn
  (h-restart)
  (make-ontes)
  (make-instance 'vroom :resizable t :width 750 :height 800))

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
