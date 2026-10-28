;;;; Compile real Mezzano sources on the booted image.
;;;; Compare matching runs from the baseline and candidate images; the old
;;;; function-swapping LAP benchmark cannot measure inlined Lisp accessors.
(defpackage :clos-fast-benchmark (:use :cl) (:export :run))
(in-package :clos-fast-benchmark)

(defun compile-one (source output)
  (let ((*package* (find-package :cl-user)))
    (multiple-value-bind (path warnings failure)
        (compile-file source :output-file output :verbose nil :print nil :parallel nil)
      (declare (ignore warnings))
      (assert (and path (not failure)))
      path)))

(defun run (&key (repeats 3)
                (sources '("SYS:SOURCE;COMPILER;BACKEND;ANALYSIS.LISP"
                           "SYS:SOURCE;COMPILER;BACKEND;CFG.LISP")))
  (format t "VERSION ~A repeats=~D units=~D~%"
          (lisp-implementation-version) repeats internal-time-units-per-second)
  (dolist (source sources)
    (let ((output (format nil "REMOTE:/tmp/clos-cost/~A.llf"
                          (string-downcase (pathname-name (pathname source))))))
      (compile-one source output)
      (mezzano.internals::gc :full t)
      (let* ((self (mezzano.supervisor:current-thread))
             (bytes (mezzano.supervisor:thread-bytes-consed self))
             (gc mezzano.internals::*gc-time*)
             (cpu (mezzano.supervisor:thread-run-time self))
             (start (get-internal-real-time)))
        (dotimes (i repeats) (compile-one source output))
        (format t "SOURCE ~A N ~D WALL ~D CPU ~D BYTES ~D GC ~F~%"
                source repeats (- (get-internal-real-time) start)
                (- (mezzano.supervisor:thread-run-time self) cpu)
                (- (mezzano.supervisor:thread-bytes-consed self) bytes)
                (- mezzano.internals::*gc-time* gc))
        (finish-output)))))
