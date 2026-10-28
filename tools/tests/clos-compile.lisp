;;;; Real-workload CLOS profiling: compile the user's unchanged Boehm program.
;;;; Run timing with no profiler/GDB; profile separately, retaining dispatch frames.
(defpackage :clos-compile (:use :cl) (:export :run :profile-run :compile-workload))
(defpackage :clos-compile-workload (:use :cl))
(in-package :clos-compile)
(defvar *profile* nil)
(defparameter *source* #P"REMOTE:/home/iskam/boehm-gc.lisp")
(defparameter *output* #P"REMOTE:/tmp/clos-cost/boehm.llf")

(defun compile-workload (&optional (repeats 1))
  (let ((*package* (find-package :clos-compile-workload)))
    (dotimes (i repeats)
      (multiple-value-bind (path warnings failure)
          (compile-file *source* :output-file *output* :verbose nil :print nil :parallel nil)
        (declare (ignore warnings))
        (assert (and path (not failure)))))))

(defun run (&optional (repeats 5))
  (format t "VERSION ~A SOURCE ~A~%" (lisp-implementation-version) *source*)
  (compile-workload)
  (dotimes (i repeats)
    (let* ((self (mezzano.supervisor:current-thread))
           (bytes (mezzano.supervisor:thread-bytes-consed self))
           (gc mezzano.internals::*gc-time*)
           (cpu (mezzano.supervisor:thread-run-time self))
           (start (get-internal-real-time)))
      (compile-workload)
      (format t "COMPILE ~D wall-ticks=~D cpu-ticks=~D bytes=~D gc-seconds=~,3F~%"
              i (- (get-internal-real-time) start)
              (- (mezzano.supervisor:thread-run-time self) cpu)
              (- (mezzano.supervisor:thread-bytes-consed self) bytes)
              (- mezzano.internals::*gc-time* gc))
      (finish-output))))

(defun profile-run (&optional (repeats 20))
  (compile-workload)
  (setf *profile*
        (mezzano.profiler:with-profiling (:thread t :ignore-functions nil :repeat repeats)
          (compile-workload)))
  (mezzano.profiler:save-profile "REMOTE:/tmp/clos-cost/compile-profile.txt" *profile*
                                 :order-by :direct :verbosity :full)
  (mezzano.profiler:save-profile "REMOTE:/tmp/clos-cost/compile-flame.txt" *profile*
                                 :verbosity :flame-graph)
  (format t "PROFILE DONE samples=~D~%" (length (mezzano.profiler::profile-data *profile*))))
