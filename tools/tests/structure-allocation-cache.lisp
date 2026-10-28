;;;; Load in a freshly cold-booted image; no manual cache initialization.
(defpackage :mezzano.structure-allocation-cache-test (:use :cl))
(in-package :mezzano.structure-allocation-cache-test)

(defun run ()
  (assert (= mezzano.runtime::*structure-class-slot-storage-layout-location*
             (mezzano.runtime::slot-location-in-layout
              mezzano.runtime::*structure-class-layout*
              'mezzano.clos::slot-storage-layout)))
  (eval '(defstruct node left right dummy1 dummy2))
  (let* ((class (find-class 'node))
         (layout (mezzano.clos:class-layout class))
         (node (funcall (symbol-function 'make-node)
                        :left 1 :right 2 :dummy1 3 :dummy2 4)))
    (assert (equal (mapcar (lambda (slot)
                            (mezzano.runtime::instance-access-by-name node slot))
                          '(left right dummy1 dummy2))
                   '(1 2 3 4)))
    (assert (equal (mapcar (lambda (slot)
                            (mezzano.runtime::slot-location-in-layout layout slot))
                          '(left right dummy1 dummy2))
                   '(0 128 256 384)))
    (assert (handler-case
                (progn (mezzano.runtime::slot-location-in-layout layout 'missing) nil)
              (error () t)))
    (eval '(defstruct node left right dummy1 dummy2 extra))
    (let ((new-layout (mezzano.clos:class-layout class)))
      (assert (not (eq layout new-layout)))
      ;; Both public entry forms must fetch the replacement layout.
      (dolist (designator (list class 'node))
        (assert (eq (mezzano.internals::%instance-layout
                     (mezzano.internals::%allocate-struct designator))
                    new-layout)))
      (assert (= (funcall (symbol-function 'node-extra)
                         (funcall (symbol-function 'make-node) :extra 42))
                 42))))
  (format t "~&STRUCTURE-ALLOCATION-CACHE-PASSED~%"))
(run)
