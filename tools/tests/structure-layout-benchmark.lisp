;;;; Load structure-layout-benchmark-baseline.lisp wired, then compile and load
;;;; this file in a running x86-64 guest and call RUN.
;;;; Timings compare the upstream compatibility predicate and TYPEP expansion
;;;; against the current ones, using the same valid objects in one image.
(defpackage :mezzano.structure-layout-benchmark
  (:use :cl)
  (:export :run))
(in-package :mezzano.structure-layout-benchmark)

(defun baseline-expression (form)
  ;; The old TYPEP expansion had neither the new predicate nor class fallback.
  (cond ((eq form 'mezzano.internals::structure-layout-compatible-p)
         'mezzano.internals::layout-benchmark-upstream-compatible-p)
        ((atom form) form)
        ((eq (car form) 'quote) form)
        ((eq (car form) 'mezzano.internals::structure-layout-typep)
         (list 'mezzano.internals::layout-benchmark-upstream-compatible-p
               (second form) (third form)))
        ((eq (car form) 'mezzano.internals::structure-type-p) nil)
        (t (cons (baseline-expression (car form))
                 (baseline-expression (cdr form))))))

(defun contains-call-p (form name)
  (and (consp form)
       (not (eq (car form) 'quote))
       (or (eq (car form) name)
           (contains-call-p (car form) name)
           (contains-call-p (cdr form) name))))

(defun compile-typep-loop (expression)
  ;; The vector changes the object every iteration; keep the generated TYPEP
  ;; expression inside the loop, as in normal compiled application code.
  (compile nil
           `(lambda (objects repeats)
              (declare (optimize (speed 3) (safety 1) (debug 0))
                       (type simple-vector objects) (type fixnum repeats))
              (let ((count 0))
                (declare (type fixnum count))
                (dotimes (r repeats count)
                  (dotimes (i 64)
                    (let ((object (svref objects i)))
                      (when ,expression (incf count)))))))))

(defun time-trial (function objects repeats expected)
  (let* ((thread (mezzano.supervisor:current-thread))
         (bytes-before (mezzano.supervisor:thread-bytes-consed thread))
         (gc-before mezzano.internals::*gc-cycles*)
         (start (mezzano.supervisor:get-high-precision-timer))
         (value (funcall function objects repeats))
         (elapsed (- (mezzano.supervisor:get-high-precision-timer) start))
         (bytes (- (mezzano.supervisor:thread-bytes-consed thread) bytes-before))
         (gcs (- mezzano.internals::*gc-cycles* gc-before)))
    (assert (plusp elapsed))
    (assert (= value expected))
    (assert (zerop bytes))
    (assert (zerop gcs))
    elapsed))

(defun median (values)
  (let ((sorted (sort (copy-seq values) #'<)))
    (aref sorted (floor (length sorted) 2))))

(defun compare-paired (label old-function old-objects new-function new-objects repeats hits pairs)
  (let ((old (make-array pairs)) (new (make-array pairs))
        (ratios (make-array pairs))
        (expected (* repeats hits)))
    ;; Warm both compiled functions and all their inputs before sampling.
    (time-trial old-function old-objects 32 (* 32 hits))
    (time-trial new-function new-objects 32 (* 32 hits))
    (dotimes (i pairs)
      (if (evenp i)
          (setf (aref old i) (time-trial old-function old-objects repeats expected)
                (aref new i) (time-trial new-function new-objects repeats expected))
          (setf (aref new i) (time-trial new-function new-objects repeats expected)
                (aref old i) (time-trial old-function old-objects repeats expected)))
      (setf (aref ratios i) (/ (float (aref new i) 1d0) (aref old i)))
      (format t "~&LAYOUT-BENCH-PAIR ~S ~D old=~D new=~D new/old=~,4F~%"
              label i (aref old i) (aref new i) (aref ratios i)))
    (format t "~&LAYOUT-BENCH-MEDIAN ~S old=~D new=~D paired-new/old=~,4F~%"
            label (median old) (median new) (median ratios))
    (finish-output)))

(defun run (&key (repeats 40000) (accessor-repeats repeats) (pairs 11))
  (let ((*package* (find-package :mezzano.structure-layout-benchmark)))
    (eval '(defstruct parent value))
    (eval '(defstruct (child (:include parent)) child-value))
    (eval '(defstruct (grandchild (:include child)) grandchild-value))
    (eval '(defstruct unrelated value))
    (eval '(defstruct (legacy-child (:include parent)) legacy-value))
    (let* ((expansion (mezzano.internals::compile-typep-expression 'object 'parent))
           (old-expansion (baseline-expression expansion))
           (old (compile-typep-loop old-expansion))
           (new (compile-typep-loop expansion))
           (parent (make-parent :value 3))
           (child (make-child :value 3))
           (grandchild (make-grandchild :value 3))
           (unrelated (make-unrelated :value 3))
           (legacy (make-legacy-child :value 3))
           (legacy-layout (mezzano.clos:class-layout (find-class 'legacy-child)))
           (legacy-vector (mezzano.internals::layout-compatible legacy-layout))
           (accessor (compile nil
                              '(lambda (objects repeats)
                                 (declare (optimize (speed 3) (safety 1) (debug 0))
                                          (type simple-vector objects) (type fixnum repeats))
                                 (let ((sum 0))
                                   (declare (type fixnum sum))
                                   (dotimes (r repeats sum)
                                     (dotimes (i 64)
                                       (incf sum (parent-value (svref objects i)))))))))
           (mixed (make-array 64)))
      (assert (contains-call-p expansion 'mezzano.internals::structure-layout-typep))
      (assert (contains-call-p old-expansion 'mezzano.internals::layout-benchmark-upstream-compatible-p))
      (assert (not (contains-call-p old-expansion 'mezzano.internals::structure-layout-typep)))
      ;; Put both helpers in the wired function area for the comparison.
      (assert (< (mezzano.internals::lisp-object-address
                  #'mezzano.internals::layout-benchmark-upstream-compatible-p)
                 #x8000000000))
      (assert (< (mezzano.internals::lisp-object-address #'mezzano.internals::structure-layout-typep) #x8000000000))
      (dotimes (i 64)
        (setf (svref mixed i)
              (case (mod i 4) (0 parent) (1 child) (2 unrelated) (3 7))))
      (format t "~&LAYOUT-BENCH-BEGIN repeats=~D accessor-repeats=~D objects=64 pairs=~D~%"
              repeats accessor-repeats pairs)
      (format t "LAYOUT-BENCH-CODE-BYTES old=~D new=~D~%"
              (- (mezzano.internals::function-code-size old) 16)
              (- (mezzano.internals::function-code-size new) 16))
      (mezzano.internals::gc :full t)
      (dolist (case (list (list :exact (make-array 64 :initial-element parent) 64)
                          (list :child (make-array 64 :initial-element child) 64)
                          (list :grandchild (make-array 64 :initial-element grandchild) 64)
                          (list :miss-struct (make-array 64 :initial-element unrelated) 0)
                          (list :miss-fixnum (make-array 64 :initial-element 7) 0)
                          (list :mixed mixed 32)))
        (compare-paired (first case) old (second case) new (second case)
                        repeats (third case) pairs))
      ;; Emulate the original empty vector on an otherwise identical child,
      ;; and use the same compiled parent accessor for both cases.  Unlike
      ;; TYPEP, the old accessor finds the slot via its correct slow fallback.
      (unwind-protect
           (progn
             (setf (mezzano.internals::%struct-slot
                    legacy-layout 'mezzano.internals::layout 'mezzano.internals::compatible)
                   (make-array 0 :area :wired))
             (compare-paired :parent-accessor accessor (make-array 64 :initial-element legacy)
                             accessor (make-array 64 :initial-element child)
                             accessor-repeats (* 64 3) pairs))
        (setf (mezzano.internals::%struct-slot
               legacy-layout 'mezzano.internals::layout 'mezzano.internals::compatible)
              legacy-vector))
      (format t "LAYOUT-BENCH-PASSED~%"))))
