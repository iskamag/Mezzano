;;;; Load in a running Mezzano image, then call
;;;; (mezzano.structure-layout-test:run).
(defpackage :mezzano.structure-layout-test
  (:use :cl)
  (:export :run))
(in-package :mezzano.structure-layout-test)

;; These definitions and the TYPEP call are cross-compiled into cold images.
(defstruct static-parent value)
(defstruct (static-child (:include static-parent)) child-value)
(defun cold-compiled-static-parent-p (object)
  (typep object 'static-parent))

(defun class-layout (name)
  (mezzano.clos:class-layout (find-class name)))

(defun compiled-typep (name)
  (compile nil `(lambda (object) (typep object ',name))))

(defun check-layouts (name expected)
  (let* ((layout (class-layout name))
         (vector (mezzano.internals::layout-compatible layout)))
    (assert (= (length vector) (length expected)))
    (loop for actual across vector
          for class-name in expected
          do (assert (eq actual (class-layout class-name))))
    ;; The supervisor may inspect layout vectors without paging.
    (assert (< (mezzano.internals::lisp-object-address vector) #x8000000000))))

(defun run ()
  (let ((*package* (find-package :mezzano.structure-layout-test)))
    ;; Evaluate definitions here so layouts are constructed in the running
    ;; image even when this test file itself has been compiled.
    (eval '(defstruct parent value))
    (eval '(defstruct (child (:include parent)) child-value))
    (eval '(defstruct (grandchild (:include child)) grandchild-value))
    (eval '(defstruct unrelated other))
    (check-layouts 'parent '(parent structure-object))
    (check-layouts 'child '(child parent structure-object))
    (check-layouts 'grandchild '(grandchild child parent structure-object))
    (let* ((parentp (compiled-typep 'parent))
           (childp (compiled-typep 'child))
           (accessor (compile nil '(lambda (x) (parent-value x))))
           (object (make-grandchild :value 42)))
      (assert (funcall parentp object))
      (assert (funcall childp object))
      (assert (not (funcall parentp (make-unrelated))))
      (assert (not (funcall parentp 7)))
      (assert (mezzano.internals::structure-layout-compatible-p object (class-layout 'parent)))
      (assert (= (funcall accessor object) 42))
      (mezzano.internals::gc :full t)
      (check-layouts 'grandchild '(grandchild child parent structure-object))
      (assert (funcall parentp object))
      (assert (= (funcall accessor object) 42))
      (assert (handler-case (progn (funcall accessor (make-unrelated)) nil)
                (type-error () t))))
    ;; A dynamically created child may inherit from a cold-built structure.
    (eval '(defstruct (cold-child (:include mezzano.supervisor::acpi-table-header)) value))
    (check-layouts 'cold-child '(cold-child mezzano.supervisor::acpi-table-header structure-object))
    (assert (funcall (compiled-typep 'mezzano.supervisor::acpi-table-header)
                     (make-cold-child)))
    ;; A cold-compiled TYPEP also resolves the class of an obsolete target
    ;; layout after a runtime redefinition.
    (let ((object (make-static-child :value 5))
          (old-layout (class-layout 'static-parent)))
      (assert (cold-compiled-static-parent-p object))
      (eval '(defstruct static-parent value (added 9)))
      (assert (mezzano.internals::layout-obsolete old-layout))
      (assert (cold-compiled-static-parent-p object)))
    ;; Structure definitions have unpublished layouts before class conversion.
    (let* ((base (mezzano.internals::make-struct-definition
                  'definition-base nil nil nil 0 nil nil nil nil))
           (derived (mezzano.internals::make-struct-definition
                     'definition-derived nil base nil 0 nil nil nil nil))
           (base-layout (mezzano.internals::structure-definition-layout base))
           (derived-layout (mezzano.internals::structure-definition-layout derived))
           (compatible (mezzano.internals::layout-compatible derived-layout)))
      (assert (= (length compatible) 2))
      (assert (eq (svref compatible 0) derived-layout))
      (assert (eq (svref compatible 1) base-layout)))
    ;; Ordinary CLOS layouts contain themselves, not superclass layouts.
    (eval '(defclass standard-probe () ((value :initform 1))))
    (make-instance 'standard-probe)
    (let* ((layout (class-layout 'standard-probe))
           (compatible (mezzano.internals::layout-compatible layout)))
      (assert (= (length compatible) 1))
      (assert (eq (svref compatible 0) layout)))
    ;; Old objects and compiled TYPEP functions retain their class membership.
    (eval '(defstruct mutable value))
    (let* ((old (make-mutable :value 17))
           (old-layout (class-layout 'mutable))
           (old-predicate (compiled-typep 'mutable))
           (accessor (compile nil '(lambda (x) (mutable-value x)))))
      (eval '(defstruct mutable value (added 9)))
      (assert (mezzano.internals::layout-obsolete old-layout))
      (assert (funcall old-predicate old))
      (assert (funcall old-predicate (make-mutable :value 18)))
      (assert (funcall (compiled-typep 'mutable) old))
      (assert (= (funcall accessor old) 17))
      (assert (mezzano.internals::layout-new-instance
               (mezzano.internals::%instance-layout old)))
      (assert (not (mezzano.internals::structure-layout-compatible-p old (class-layout 'mutable))))
      (assert (= (mutable-value old) 17))
      (check-layouts 'mutable '(mutable structure-object)))
    ;; An obsolete intermediate layout invalidates the remaining ancestor path.
    (eval '(defstruct old-base))
    (eval '(defstruct new-base))
    (eval '(defstruct (middle (:include old-base)) marker))
    (eval '(defstruct (leaf (:include middle)) leaf-value))
    (let* ((object (make-leaf :marker 12))
           (old-p (compiled-typep 'old-base))
           (new-p (compiled-typep 'new-base))
           (old-middle (class-layout 'middle)))
      (assert (funcall old-p object))
      (assert (not (funcall new-p object)))
      (eval '(defstruct (middle (:include new-base)) marker))
      (assert (mezzano.internals::layout-obsolete old-middle))
      (check-layouts 'middle '(middle new-base structure-object))
      (assert (not (funcall old-p object)))
      (assert (funcall new-p object))
      (assert (not (mezzano.internals::structure-layout-compatible-p object (class-layout 'old-base)))))
    (format t "STRUCTURE-COMPATIBLE-LAYOUTS-PASSED~%")
    t))
