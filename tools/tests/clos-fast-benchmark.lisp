;;;; Compile real Mezzano sources with the original Lisp helpers and new LAP.
;;;; Load after clos-fast-hotload.lisp. No profiler/GDB during timing trials.
;;;; The installed fast paths are restored even if compilation signals.
(defpackage :clos-fast-benchmark (:use :cl) (:export :run))
(in-package :clos-fast-benchmark)

(defun compile-one (source output)
  (let ((*package* (find-package :cl-user)))
    (multiple-value-bind (path warnings failure)
        (compile-file source :output-file output :verbose nil :print nil :parallel nil)
      (declare (ignore warnings))
      (assert (and path (not failure)))
      path)))

(defun run (&key (pairs 4) (repeats 3)
                (sources '("SYS:SOURCE;COMPILER;BACKEND;ANALYSIS.LISP"
                           "SYS:SOURCE;COMPILER;BACKEND;CFG.LISP")))
  (let* ((names '(mezzano.clos::single-dispatch-emf-entry-by-object
                  mezzano.clos::fast-slot-read mezzano.clos::fast-slot-write))
         (installed (mapcar #'fdefinition names))
         (baseline (mapcar #'fdefinition
                           '(mezzano.clos::single-dispatch-emf-entry-by-object-slow
                             mezzano.clos::fast-slot-read-slow
                             mezzano.clos::fast-slot-write-slow))))
    (format t "VERSION ~A repeats=~D pairs=~D units=~D~%"
            (lisp-implementation-version) repeats pairs internal-time-units-per-second)
    (unwind-protect
         (dolist (source sources)
           (dotimes (pair pairs)
             (dolist (fast (if (evenp pair) '(nil t) '(t nil)))
               (loop for name in names for fn in (if fast installed baseline)
                     do (setf (fdefinition name) fn))
               (let ((output (format nil "REMOTE:/tmp/clos-cost/~A-~A.llf"
                                     (string-downcase (pathname-name (pathname source)))
                                     (if fast "fast" "baseline"))))
                 (compile-one source output)
                 ;; Keep collection outside the comparison where possible;
                 ;; still report any collection during the measured block.
                 (mezzano.internals::gc :full t)
                 (let* ((self (mezzano.supervisor:current-thread))
                        (bytes (mezzano.supervisor:thread-bytes-consed self))
                        (gc mezzano.internals::*gc-time*)
                        (cpu (mezzano.supervisor:thread-run-time self))
                        (start (get-internal-real-time)))
                   (dotimes (i repeats) (compile-one source output))
                   (format t "SOURCE ~A FAST ~S PAIR ~D N ~D WALL ~D CPU ~D BYTES ~D GC ~F~%"
                           source fast pair repeats (- (get-internal-real-time) start)
                           (- (mezzano.supervisor:thread-run-time self) cpu)
                           (- (mezzano.supervisor:thread-bytes-consed self) bytes)
                           (- mezzano.internals::*gc-time* gc))
                   (finish-output))))))
      (loop for name in names for fn in installed do (setf (fdefinition name) fn)))
    (format t "A/B DONE; fast paths restored.~%")))

(run)
