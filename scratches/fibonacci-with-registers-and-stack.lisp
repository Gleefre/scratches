(defun fib-iter (n)
  (declare (optimize speed (safety 0) (debug 0))
           (type fixnum n))
  (let ((stack (make-array (* 2 n) :element-type 'fixnum))
        (stack-pointer 0))
    (declare (type fixnum stack-pointer))
    ;; stack
    (let ((x n) (res 0) (continue 0))
      (declare (type fixnum res x continue))
      ;; registers
      (macrolet ((save (reg)
                   `(setf (aref stack stack-pointer) ,reg
                          stack-pointer (1+ stack-pointer)))
                 (restore (reg)
                   `(setf ,reg (aref stack (decf stack-pointer))))
                 (continue-dispatch ()
                   `(case continue
                      (0 (go done))
                      (1 (go after-fib-x-1))
                      (2 (go after-fib-x-2)))))
        ;; utility macros for dealing with stack
        (block nil
          (tagbody
           fib-rec
             (when (< x 2)
               (setf res x)
               (continue-dispatch))
             ;; prepare call to (fib (- n 1))
             (save continue)
             (save x)
             (setf x (the fixnum (- x 1)))
             (setf continue 1)
             (go fib-rec)
           after-fib-x-1
             ;; prepare call to (fib (- n 2))
             (restore x)
             (setf x (the fixnum (- x 2)))
             (save res)
             (setf continue 2)
             (go fib-rec)
           after-fib-x-2
             ;; add results together and return
             (setf x res)
             (restore res)
             (setf res (the fixnum (+ res x)))
             (restore continue)
             (continue-dispatch)
           done
             (return res)))))))

(defun fib (n)
  (declare (optimize speed (safety 0) (debug 0))
           (type fixnum n))
  (if (< n 2)
      n
      (the fixnum (+ (the fixnum (fib (the fixnum (- n 1))))
                     (the fixnum (fib (the fixnum (- n 2))))))))

(compile 'fib-iter)
(compile 'fib)

#|

| Implementation | (FIB-ITER 40) |  (FIB 40) | (FIB-ITER 30) | (FIB 30) |
|----------------+---------------+-----------+---------------+----------|
| sbcl           |      0.383268 |  0.598050 |      0.002998 | 0.004876 |
| cmucl          |      0.410142 |  2.236036 |      0.003314 | 0.018380 |
| ccl            |      1.725570 |  0.551618 |      0.014413 | 0.004546 |
| acl            |      1.287770 |  0.402217 |      0.010419 | 0.003286 |
| ecl            |      0.247000 |  1.265000 |      0.002000 | 0.010000 |
| clasp          |     37.323000 |  4.530000 |      0.295000 | 0.036000 |
| abcl           |      1.740000 |  0.750000 |      0.033000 | 0.013000 |
| clisp          |     38.812614 | 13.471093 |      0.305383 | 0.112609 |
| lispworks      |      0.430000 |  0.667000 |      0.004000 | 0.010000 |

|#

(time (fib-iter 40))
(time (fib 40))

#|
==================================================================
============================== SBCL ==============================
==================================================================
This is SBCL 2.3.6, an implementation of ANSI Common Lisp.

* (TIME (FIB-ITER 40))
Evaluation took:
  0.380 seconds of real time
  0.383268 seconds of total run time (0.382907 user, 0.000361 system)
  100.79% CPU
  1,032,898,743 processor cycles
  0 bytes consed
  
102334155
* (TIME (FIB 40))
Evaluation took:
  0.600 seconds of real time
  0.598050 seconds of total run time (0.598050 user, 0.000000 system)
  99.67% CPU
  1,611,735,624 processor cycles
  0 bytes consed
  
102334155
==================================================================
============================== SBCL END ==========================
==================================================================



==================================================================
============================== CMUCL =============================
==================================================================
CMU Common Lisp 21e (21E Unicode), running on grolter-T16

* (TIME (FIB-ITER 40))
; Compiling LAMBDA NIL: 
; Compiling Top-Level Form: 

; Evaluation took:
;   0.41 seconds of real time
;   0.410142 seconds of user run time
;   0.0 seconds of system run time
;   1,105,301,052 CPU cycles
;   0 page faults and
;   328 bytes consed.
; 
102334155
* (TIME (FIB 40))
; Compiling LAMBDA NIL: 
; Compiling Top-Level Form: 

; Evaluation took:
;   2.23 seconds of real time
;   2.236036 seconds of user run time
;   0.0 seconds of system run time
;   6,026,017,734 CPU cycles
;   0 page faults and
;   0 bytes consed.
; 
102334155
==================================================================
============================== CMUCL END =========================
==================================================================



==================================================================
==============================  CCL ==============================
==================================================================
Clozure Common Lisp Version 1.12 (v1.12) LinuxX8664

? (TIME (FIB-ITER 40))
took 1,725,570 microseconds (1.725570 seconds) to run.
During that period, and with 16 available CPU cores,
     1,725,790 microseconds (1.725790 seconds) were spent in user mode
            21 microseconds (0.000021 seconds) were spent in system mode
 656 bytes of memory allocated.
102334155
? (TIME (FIB 40))
took 551,618 microseconds (0.551618 seconds) to run.
During that period, and with 16 available CPU cores,
     551,679 microseconds (0.551679 seconds) were spent in user mode
           0 microseconds (0.000000 seconds) were spent in system mode
102334155
==================================================================
==============================  CCL END ==========================
==================================================================



==================================================================
==============================  ACL ==============================
==================================================================
International Allegro CL Free Express Edition
10.1 [64-bit Linux (x86-64)] (Jan 23, 2023 8:46)
Copyright (C) 1985-2023, Franz Inc., Oakland, CA, USA.  All Rights Reserved.

This development copy of Allegro CL is licensed to:
   Allegro CL 10.1 Express user

CL-USER(5): (TIME (FIB-ITER 40))
; cpu time (non-gc) 1.287770 sec user, 0.000000 sec system
; cpu time (gc)     0.000000 sec user, 0.000000 sec system
; cpu time (total)  1.287770 sec user, 0.000000 sec system
; real time  1.287782 sec (100.0%)
; space allocation:
;  136 cons cells, 4,512 other bytes, 0 static bytes
; Page Faults: major: 0 (gc: 0), minor: 5 (gc: 0)
102334155
CL-USER(6): (TIME (FIB 40))
; cpu time (non-gc) 0.402217 sec user, 0.000000 sec system
; cpu time (gc)     0.000000 sec user, 0.000000 sec system
; cpu time (total)  0.402217 sec user, 0.000000 sec system
; real time  0.402225 sec (100.0%)
; space allocation:
;  0 cons cells, 0 other bytes, 0 static bytes
; Page Faults: major: 0 (gc: 0), minor: 0 (gc: 0)
102334155
==================================================================
==============================  ACL END ==========================
==================================================================



==================================================================
==============================  ECL ==============================
==================================================================
ECL (Embeddable Common-Lisp) 21.2.1 (git:c646799145538997d84ed6d8755be7e7837eb7ef)

> (TIME (FIB-ITER 40))
real time : 0.247 secs
run time  : 0.248 secs
gc count  : 1 times
consed    : 672 bytes
102334155
> (TIME (FIB 40))
real time : 1.265 secs
run time  : 1.264 secs
gc count  : 1 times
consed    : 0 bytes
102334155
==================================================================
==============================  ECL END ==========================
==================================================================



==================================================================
============================== CLASP =============================
==================================================================
Starting clasp-boehmprecise-2.2.0 from base image

COMMON-LISP-USER> (TIME (FIB-ITER 40))
Time real(37.323 secs) run(37.323 secs) consed(664 bytes) unwinds(0)
102334155
COMMON-LISP-USER> (TIME (FIB 40))
Time real(4.530 secs) run(4.530 secs) consed(0 bytes) unwinds(0)
102334155
==================================================================
============================== CLASP END =========================
==================================================================



==================================================================
============================== ABCL ==============================
==================================================================
Armed Bear Common Lisp 1.9.1
Java 20.0.1 Private Build
OpenJDK 64-Bit Server VM

CL-USER(5): (TIME (FIB-ITER 40))
1.74 seconds real time
0 cons cells
102334155
CL-USER(6): (TIME (FIB 40))
0.75 seconds real time
0 cons cells
102334155
==================================================================
============================== ABCL END ==========================
==================================================================



==================================================================
============================== CLISP =============================
==================================================================
  i i i i i i i       ooooo    o        ooooooo   ooooo   ooooo
  I I I I I I I      8     8   8           8     8     o  8    8
  I  \ `+' /  I      8         8           8     8        8    8
   \  `-+-'  /       8         8           8      ooooo   8oooo
    `-__|__-'        8         8           8           8  8
        |            8     o   8           8     o     8  8
  ------+------       ooooo    8oooooo  ooo8ooo   ooooo   8

Welcome to GNU CLISP 2.49.93+ (2018-02-18) <http://clisp.org/>
[5]> (TIME (FIB-ITER 40))
Real time: 38.812614 sec.
Run time: 38.811913 sec.
Space: 992 Bytes
102334155
[6]> (TIME (FIB 40))
Real time: 13.471093 sec.
Run time: 13.470745 sec.
Space: 0 Bytes
102334155
==================================================================
============================== CLISP END =========================
==================================================================



==================================================================
============================== LISPWORKS =========================
==================================================================
CL-USER 5 > (TIME (FIB-ITER 40))
Timing the evaluation of (FIB-ITER 40)

User time    =        0.430
System time  =        0.007
Elapsed time =        0.384
Allocation   = 528624 bytes
0 Page faults
GC time      =        0.000
102334155

CL-USER 6 > (TIME (FIB 40))
Timing the evaluation of (FIB 40)

User time    =        0.667
System time  =        0.003
Elapsed time =        0.650
Allocation   = 135920 bytes
0 Page faults
GC time      =        0.000
102334155
==================================================================
============================== LISPWORKS END =====================
==================================================================
|#

(time (fib-iter 30))
(time (fib 30))

#|
==================================================================
============================== SBCL ==============================
==================================================================
This is SBCL 2.3.6, an implementation of ANSI Common Lisp.

* (TIME (FIB-ITER 30))
Evaluation took:
  0.000 seconds of real time
  0.002998 seconds of total run time (0.002679 user, 0.000319 system)
  100.00% CPU
  8,078,481 processor cycles
  0 bytes consed
  
832040
* (TIME (FIB 30))
Evaluation took:
  0.004 seconds of real time
  0.004876 seconds of total run time (0.004876 user, 0.000000 system)
  125.00% CPU
  13,139,820 processor cycles
  0 bytes consed
  
832040
==================================================================
============================== SBCL END ==========================
==================================================================



==================================================================
============================== CMUCL =============================
==================================================================
CMU Common Lisp 21e (21E Unicode), running on grolter-T16

* (TIME (FIB-ITER 30))
; Compiling LAMBDA NIL: 
; Compiling Top-Level Form: 

; Evaluation took:
;   0.01 seconds of real time
;   0.003314 seconds of user run time
;   0.0 seconds of system run time
;   8,930,871 CPU cycles
;   0 page faults and
;   248 bytes consed.
; 
832040
* (TIME (FIB 30))
; Compiling LAMBDA NIL: 
; Compiling Top-Level Form: 

; Evaluation took:
;   0.01 seconds of real time
;   0.01838 seconds of user run time
;   0.0 seconds of system run time
;   49,520,970 CPU cycles
;   0 page faults and
;   0 bytes consed.
; 
832040
==================================================================
============================== CMUCL END =========================
==================================================================



==================================================================
==============================  CCL ==============================
==================================================================
Clozure Common Lisp Version 1.12 (v1.12) LinuxX8664

? (TIME (FIB-ITER 30))
took 14,413 microseconds (0.014413 seconds) to run.
During that period, and with 16 available CPU cores,
     14,411 microseconds (0.014411 seconds) were spent in user mode
          0 microseconds (0.000000 seconds) were spent in system mode
 496 bytes of memory allocated.
832040
? (TIME (FIB 30))
took 4,546 microseconds (0.004546 seconds) to run.
During that period, and with 16 available CPU cores,
     4,547 microseconds (0.004547 seconds) were spent in user mode
         0 microseconds (0.000000 seconds) were spent in system mode
832040
==================================================================
==============================  CCL END ==========================
==================================================================



==================================================================
==============================  ACL ==============================
==================================================================
International Allegro CL Free Express Edition
10.1 [64-bit Linux (x86-64)] (Jan 23, 2023 8:46)
Copyright (C) 1985-2023, Franz Inc., Oakland, CA, USA.  All Rights Reserved.

This development copy of Allegro CL is licensed to:
   Allegro CL 10.1 Express user

CL-USER(5): (TIME (FIB-ITER 30))
; cpu time (non-gc) 0.010419 sec user, 0.000000 sec system
; cpu time (gc)     0.000000 sec user, 0.000000 sec system
; cpu time (total)  0.010419 sec user, 0.000000 sec system
; real time  0.010420 sec (99.99%)
; space allocation:
;  0 cons cells, 512 other bytes, 0 static bytes
; Page Faults: major: 0 (gc: 0), minor: 0 (gc: 0)
832040
CL-USER(6): (TIME (FIB 30))
; cpu time (non-gc) 0.003286 sec user, 0.000000 sec system
; cpu time (gc)     0.000000 sec user, 0.000000 sec system
; cpu time (total)  0.003286 sec user, 0.000000 sec system
; real time  0.003288 sec (99.94%)
; space allocation:
;  0 cons cells, 0 other bytes, 0 static bytes
; Page Faults: major: 0 (gc: 0), minor: 0 (gc: 0)
832040
==================================================================
==============================  ACL END ==========================
==================================================================



==================================================================
==============================  ECL ==============================
==================================================================
ECL (Embeddable Common-Lisp) 21.2.1 (git:c646799145538997d84ed6d8755be7e7837eb7ef)

> (TIME (FIB-ITER 30))
real time : 0.002 secs
run time  : 0.002 secs
gc count  : 1 times
consed    : 512 bytes
832040
> (TIME (FIB 30))
real time : 0.010 secs
run time  : 0.010 secs
gc count  : 1 times
consed    : 0 bytes
832040
==================================================================
==============================  ECL END ==========================
==================================================================



==================================================================
============================== CLASP =============================
==================================================================
Starting clasp-boehmprecise-2.2.0 from base image

COMMON-LISP-USER> (TIME (FIB-ITER 30))
Time real(0.295 secs) run(0.295 secs) consed(504 bytes) unwinds(0)
832040
COMMON-LISP-USER> (TIME (FIB 30))
Time real(0.036 secs) run(0.036 secs) consed(0 bytes) unwinds(0)
832040
==================================================================
============================== CLASP END =========================
==================================================================



==================================================================
============================== ABCL ==============================
==================================================================
Armed Bear Common Lisp 1.9.1
Java 20.0.1 Private Build
OpenJDK 64-Bit Server VM

CL-USER(5): (TIME (FIB-ITER 30))
0.033 seconds real time
0 cons cells
832040
CL-USER(6): (TIME (FIB 30))
0.013 seconds real time
0 cons cells
832040
==================================================================
============================== ABCL END ==========================
==================================================================



==================================================================
============================== CLISP =============================
==================================================================
  i i i i i i i       ooooo    o        ooooooo   ooooo   ooooo
  I I I I I I I      8     8   8           8     8     o  8    8
  I  \ `+' /  I      8         8           8     8        8    8
   \  `-+-'  /       8         8           8      ooooo   8oooo
    `-__|__-'        8         8           8           8  8
        |            8     o   8           8     o     8  8
  ------+------       ooooo    8oooooo  ooo8ooo   ooooo   8

Welcome to GNU CLISP 2.49.93+ (2018-02-18) <http://clisp.org/>

[5]> (TIME (FIB-ITER 30))
Real time: 0.305383 sec.
Run time: 0.30537 sec.
Space: 832 Bytes
832040
[6]> (TIME (FIB 30))
Real time: 0.112609 sec.
Run time: 0.11261 sec.
Space: 0 Bytes
832040
==================================================================
============================== CLISP END =========================
==================================================================



==================================================================
============================== LISPWORKS =========================
==================================================================
CL-USER 5 > (TIME (FIB-ITER 30))
Timing the evaluation of (FIB-ITER 30)

User time    =        0.004
System time  =        0.000
Elapsed time =        0.003
Allocation   = 116344 bytes
0 Page faults
GC time      =        0.000
832040

CL-USER 6 > (TIME (FIB 30))
Timing the evaluation of (FIB 30)

User time    =        0.010
System time  =        0.000
Elapsed time =        0.006
Allocation   = 88856 bytes
0 Page faults
GC time      =        0.000
832040
==================================================================
============================== LISPWORKS END =====================
==================================================================
|#
