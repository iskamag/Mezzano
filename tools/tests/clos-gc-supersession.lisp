;;;; Deterministically interleave slot selection, supersession, and collection.
;;;; PROBE also runs on the old collector and reports the corruption safely.
(defpackage :clos-gc-supersession-test (:use :cl) (:export :probe :run))
(in-package :clos-gc-supersession-test)

(defclass before () ((x :initarg :x) (guard :initarg :guard)))
(defclass after () ((guard :initarg :guard) (x :initarg :x)))
(defclass callable-before () ((x :initarg :x) (guard :initarg :guard))
  (:metaclass mezzano.clos:funcallable-standard-class))
(defclass callable-after () ((guard :initarg :guard) (x :initarg :x))
  (:metaclass mezzano.clos:funcallable-standard-class))

(defun probe (&optional callable)
  (let* ((old (make-instance (if callable 'callable-before 'before)
                             :x (vector :old) :guard (vector :old-guard)))
         (replacement (make-instance (if callable 'callable-after 'after)
                                     :x (vector :new) :guard (vector :guard)))
         ;; This is the physical object and offset an interrupted access holds.
         (location (mezzano.clos::slot-location-using-layout
                    'x (mezzano.internals::%instance-layout old))))
    (assert (= location (mezzano.clos::slot-location-using-layout
                        'guard (mezzano.internals::%instance-layout replacement))))
    (when callable
      (mezzano.clos:set-funcallable-instance-function old (lambda () :called)))
    ;; Force the problematic ordering without relying on scheduler timing.
    (mezzano.runtime::supersede-instance old replacement)
    (mezzano.internals::gc :full t)
    (let ((preserved (not (eq old replacement)))
          (read-ok (equalp (mezzano.clos::standard-instance-access old location)
                           #(:old))))
      ;; Resume a pending write using its already selected physical location.
      (setf (mezzano.clos::standard-instance-access old location) (vector :written))
      (mezzano.internals::gc)
      (list :storage-preserved preserved :read-ok read-ok
            :write-ok (and (equalp (slot-value replacement 'guard) #(:guard))
                           (equalp (mezzano.clos::standard-instance-access old location)
                                   #(:written)))
            :forwarding-ok (equalp (slot-value old 'x) #(:new))
            :callable-ok (or (not callable) (eq (funcall old) :called))))))

(defun run ()
  (dolist (callable '(nil t))
    (let ((result (probe callable)))
      (format t "SUPERSESSION callable=~S ~S~%" callable result)
      (loop for (key value) on result by #'cddr do
        (assert value () "Supersession check ~S failed" key))))
  ;; GC must not detach the layout while SUPERSEDE-INSTANCE is updating it.
  (let* ((old (make-instance 'before :x 1 :guard 2))
         (first (make-instance 'after :x 3 :guard 4))
         (second (make-instance 'after :x 5 :guard 6)))
    (mezzano.runtime::supersede-instance old first)
    (let ((layout (mezzano.internals::%instance-layout old)))
      (mezzano.internals::gc :full t)
      (setf (mezzano.internals::layout-new-instance layout) second))
    (assert (= (slot-value old 'x) 5))
    (mezzano.runtime::supersede-instance old first)
    (mezzano.internals::gc :full t)
    (assert (= (slot-value old 'x) 3)))
  (format t "CLOS-GC-SUPERSESSION-PASSED~%")
  :passed)
