;;;; Metadata fast paths must use the current physical layout and honor MOP fallback.
(defpackage :clos-metadata-access-test (:use :cl) (:export :run))
(in-package :clos-metadata-access-test)

(defclass custom-gf (standard-generic-function) ((extra :initform :guard))
  (:metaclass mezzano.clos:funcallable-standard-class))

(defun run ()
 (let ((ordinary (ensure-generic-function (gensym) :lambda-list '(object)))
       (replacement (ensure-generic-function (gensym) :lambda-list '(object)))
       (custom (ensure-generic-function (gensym) :lambda-list '(object)
                                        :generic-function-class 'custom-gf)))
  ;; GFs with no methods start with NIL caches. Give these unused test GFs
  ;; distinct tables so the supersession checks can detect stale reads.
  (dolist (gf (list ordinary replacement custom))
    (setf (mezzano.clos::classes-to-emf-table gf)
          (mezzano.clos::make-single-dispatch-emf-table gf)))
  (assert (not (eq (mezzano.clos::classes-to-emf-table ordinary)
                   (mezzano.clos::classes-to-emf-table replacement))))
  (dolist (gf (list ordinary custom))
    (assert (eq (mezzano.clos::classes-to-emf-table gf)
                (mezzano.clos::std-slot-value gf 'mezzano.clos::classes-to-emf-table))))
  ;; The old GF's physical storage must not hide its replacement's table,
  ;; including replacement by a GF with a different layout.
  (let ((alias ordinary))
    (mezzano.runtime::supersede-instance alias replacement)
    (assert (eq (mezzano.clos::classes-to-emf-table alias)
                (mezzano.clos::classes-to-emf-table replacement)))
    (mezzano.runtime::supersede-instance alias custom)
    (assert (eq (mezzano.clos::classes-to-emf-table alias)
                (mezzano.clos::classes-to-emf-table custom)))
    (mezzano.internals::gc :full t)
    (assert (eq (mezzano.clos::classes-to-emf-table alias)
                (mezzano.clos::classes-to-emf-table custom))))
  ;; Standard, built-in, funcallable and structure metaclasses take distinct
  ;; paths; all must preserve the public MOP result.
  (dolist (name '(standard-object integer standard-generic-function
                 custom-gf mezzano.internals::layout))
    (let ((class (find-class name)))
      (assert (equal (mezzano.clos::safe-class-precedence-list class)
                     (mezzano.clos:class-precedence-list class)))))
  (dolist (object '(nil 42 :not-a-class))
    (assert (handler-case
                (progn (mezzano.clos::safe-class-precedence-list object) nil)
              (error () t))))
  (format t "CLOS-METADATA-ACCESS-PASSED~%")
  :passed))
