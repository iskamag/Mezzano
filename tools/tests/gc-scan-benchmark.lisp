;;;; Load this before hot-loading the collector, then CAPTURE :BASELINE.
;;;; Load the changed collector wired and CAPTURE :CANDIDATE. RUN accepts
;;;; the unchanged Boehm GC-BENCHMARK function as its argument.
(defpackage :mezzano.gc-scan-benchmark
  (:use :cl) (:export :capture :select :run))
(in-package :mezzano.gc-scan-benchmark)

(defparameter *function-names*
  '(mezzano.internals::scavenge-many mezzano.internals::scavenge-object
    mezzano.internals::allocate-for-transport
    mezzano.internals::really-transport-object
    mezzano.internals::transport-object-start-update
    mezzano.internals::scavenge-dynamic
    mezzano.supervisor::physical-page-exists))
(defvar *baseline* nil)
(defvar *candidate* nil)

(defun capture (mode)
  (let ((functions (mapcar (lambda (name) (cons name (fdefinition name)))
                           *function-names*)))
    (ecase mode
      (:baseline (setf *baseline* functions))
      (:candidate (setf *candidate* functions))))
  mode)

(defun select (mode)
  ;; Call only while no benchmark or explicit collection is in flight.
  (let ((functions (ecase mode (:baseline *baseline*) (:candidate *candidate*))))
    (assert functions)
    (dolist (entry functions) (setf (fdefinition (car entry)) (cdr entry)))))

(defun run (benchmark &key (depth 23) (pairs 2))
  (unwind-protect
       (dotimes (pair pairs)
         (dolist (mode (if (evenp pair) '(:baseline :candidate) '(:candidate :baseline)))
           (select mode)
           (mezzano.internals::gc :full t)
           (let* ((thread (mezzano.supervisor:current-thread))
                  (bytes (mezzano.supervisor:thread-bytes-consed thread))
                  (gc-start mezzano.internals::*gc-time*)
                  (minors mezzano.internals::*gc-minor-cycles*)
                  (majors mezzano.internals::*gc-major-cycles*)
                  (real-start (get-internal-real-time))
                  (ticks (mezzano.supervisor:get-high-precision-timer)))
             (funcall benchmark depth)
             (let ((elapsed (- (mezzano.supervisor:get-high-precision-timer) ticks)))
               (format t "~&BOEHM pair=~D mode=~S depth=~D ticks=~D seconds=~,3F gc=~,3F bytes=~D minor=~D major=~D~%"
                       pair mode depth elapsed
                       (/ (- (get-internal-real-time) real-start)
                          (float internal-time-units-per-second))
                       (- mezzano.internals::*gc-time* gc-start)
                       (- (mezzano.supervisor:thread-bytes-consed thread) bytes)
                       (- mezzano.internals::*gc-minor-cycles* minors)
                       (- mezzano.internals::*gc-major-cycles* majors))
               (finish-output)))))
    (select :candidate)))
