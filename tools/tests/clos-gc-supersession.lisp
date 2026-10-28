;;;; Deterministically interleave slot selection, supersession, and collection.
;;;; Requires the physical instance storage ABI.
(defpackage :clos-gc-supersession-test (:use :cl) (:export :probe :run :run-reclamation :run-weak-keys))
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
         ;; The storage lease precedes selection of a physical location.
         (storage (mezzano.internals::%instance-storage old))
         (location (mezzano.clos::slot-location-using-layout
                    'x (mezzano.internals::%storage-layout storage))))
    (assert (= location (mezzano.clos::slot-location-using-layout
                        'guard (mezzano.internals::%instance-layout replacement))))
    (when callable
      (mezzano.clos:set-funcallable-instance-function old (lambda () :called)))
    ;; Force the problematic ordering without relying on scheduler timing.
    (mezzano.runtime::supersede-instance old replacement)
    (mezzano.internals::gc :full t)
    (let ((preserved (and (eq old replacement)
                           (not (null (mezzano.internals::layout-new-instance
                                       (mezzano.internals::%storage-layout storage))))))
          (read-ok (equalp (mezzano.clos::standard-instance-access storage location)
                           #(:old))))
      ;; Resume a pending write using its already selected physical location.
      (setf (mezzano.clos::standard-instance-access storage location) (vector :written))
      (mezzano.internals::gc)
      (list :storage-preserved preserved :read-ok read-ok
            :write-ok (and (equalp (slot-value replacement 'guard) #(:guard))
                           (equalp (mezzano.clos::standard-instance-access storage location)
                                   #(:written)))
            :forwarding-ok (equalp (slot-value old 'x) #(:new))
            :callable-ok (or (not callable) (eq (funcall old) :called))))))

(defun run ()
  (dolist (callable '(nil t))
    (let ((result (probe callable)))
      (format t "SUPERSESSION callable=~S ~S~%" callable result)
      (loop for (key value) on result by #'cddr do
        (assert value () "Supersession check ~S failed" key))))
  ;; Repeated publication follows immutable replacement links across GC.
  (let* ((old (make-instance 'before :x 1 :guard 2))
         (first (make-instance 'after :x 3 :guard 4))
         (second (make-instance 'after :x 5 :guard 6)))
    (mezzano.runtime::supersede-instance old first)
    (mezzano.internals::gc :full t)
    (mezzano.runtime::supersede-instance old second)
    (assert (= (slot-value old 'x) 5))
    (mezzano.runtime::supersede-instance
     old (make-instance 'before :x 7 :guard 8))
    (mezzano.internals::gc :full t)
    (assert (= (slot-value old 'x) 7)))
  (format t "CLOS-GC-SUPERSESSION-PASSED~%")
  :passed)


(defvar *reclamation-root*)
(defvar *reclamation-aliases*)
(defvar *reclamation-replacement*)
(defvar *reclamation-lease* nil)
(defvar *reclamation-payload*)

(defun prepare-reclamation (leasep)
  (let* ((payload (make-array (* 8 1024 1024)
                              :element-type '(unsigned-byte 8)
                              :initial-element 7))
         (old (make-instance 'before :x payload :guard :old-guard))
         (replacement (make-instance 'after :x :new :guard :new-guard)))
    (setf *reclamation-root* old
          *reclamation-aliases* (vector old (list old))
          *reclamation-replacement* replacement
          *reclamation-payload* (mezzano.internals::make-weak-pointer payload)
          *reclamation-lease* (when leasep
                                (mezzano.internals::%instance-storage old)))
    (mezzano.runtime::supersede-instance old replacement))
  (values))

(defun check-reclamation-aliases ()
  (assert (and (eq *reclamation-root* *reclamation-replacement*)
               (eq (aref *reclamation-aliases* 0) *reclamation-root*)
               (eq (car (aref *reclamation-aliases* 1)) *reclamation-root*)))
  (values))

(defvar *reclamation-continuation*)

(defun prepare-continuation-reclamation ()
  (setf *reclamation-root* (make-instance 'before :x :old-value :guard :old-guard)
        *reclamation-replacement* (make-instance 'after :x :new :guard :new-guard))
  (let ((tag (mezzano.delimited-continuations:make-prompt-tag)))
    (assert
     (eq (mezzano.delimited-continuations:call-with-prompt
          tag
          (lambda ()
            (let* ((storage (mezzano.internals::%instance-storage *reclamation-root*))
                   (location (mezzano.clos::slot-location-using-layout
                              'x (mezzano.internals::%storage-layout storage))))
              (mezzano.delimited-continuations:abort-to-prompt tag :suspended)
              (assert (eq (mezzano.clos::standard-instance-access storage location)
                          :old-value))
              :resumed))
          (lambda (continuation value)
            (setf *reclamation-continuation* continuation)
            value))
         :suspended)))
  (mezzano.runtime::supersede-instance *reclamation-root* *reclamation-replacement*)
  (values))

(defun run-reclamation ()
  ;; Logical reachability alone must not retain an 8 MiB discarded slot.
  (prepare-reclamation nil)
  (mezzano.internals::gc :full t)
  (check-reclamation-aliases)
  (assert (not (mezzano.internals::weak-pointer-live-p *reclamation-payload*)))
  ;; An explicit physical lease preserves slots but never changes logical EQ.
  (prepare-reclamation t)
  (mezzano.internals::gc :full t)
  (check-reclamation-aliases)
  (assert (mezzano.internals::weak-pointer-live-p *reclamation-payload*))
  (setf *reclamation-lease* nil)
  (mezzano.internals::gc :full t)
  (assert (not (mezzano.internals::weak-pointer-live-p *reclamation-payload*)))
  ;; The ordinary continuation scanner preserves physical leases too.
  (prepare-continuation-reclamation)
  (mezzano.internals::gc :full t)
  (assert (eq *reclamation-root* *reclamation-replacement*))
  (assert (eq (funcall *reclamation-continuation*) :resumed))
  (setf *reclamation-continuation* nil)
  ;; Callable state follows the logical identity, before and after collection.
  (let* ((original (make-instance 'callable-before :x 1 :guard 2))
         (alias original)
         (middle (make-instance 'callable-after :x 3 :guard 4))
         (last (make-instance 'callable-before :x 5 :guard 6)))
    (mezzano.clos:set-funcallable-instance-function original (lambda () :first))
    (mezzano.runtime::supersede-instance original middle)
    (mezzano.runtime::supersede-instance original last)
    (assert (eq (funcall original) :first))
    (assert (eq (funcall alias) :first))
    (mezzano.clos:set-funcallable-instance-function original (lambda () :second))
    (assert (eq (funcall original) :second))
    (assert (eq (funcall alias) :second))
    (mezzano.internals::gc :full t)
    (assert (and (eq original alias) (eq original last)))
    (assert (eq (funcall original) :second))
    (mezzano.clos:set-funcallable-instance-function alias (lambda () :third))
    (assert (eq (funcall original) :third))
    (mezzano.internals::gc :full t)
    (assert (eq (funcall alias) :third)))
  (let* ((original (make-instance 'callable-before :x 1 :guard 2))
         (alias original))
    (mezzano.clos:set-funcallable-instance-function original (lambda () :before-change))
    (change-class original 'callable-after)
    (change-class original 'callable-before)
    (assert (eq (funcall alias) :before-change))
    (mezzano.clos:set-funcallable-instance-function alias (lambda () :after-change))
    (assert (eq (funcall original) :after-change))
    (mezzano.internals::gc :full t)
    (assert (eq (funcall alias) :after-change)))
  (format t "CLOS-GC-RECLAMATION-PASSED~%")
  :passed)

(defvar *weak-order-roots* nil)
(defvar *weak-order-pointer*)

(defun prepare-weak-key-order (order)
  (let* ((old (make-instance 'before :x :old :guard :old-guard))
         (replacement (make-instance 'after :x :new :guard :new-guard))
         (storage (mezzano.internals::%instance-storage old)))
    (setf *weak-order-pointer*
          (mezzano.internals::make-weak-pointer old :value :weak-value))
    (mezzano.runtime::supersede-instance old replacement)
    ;; The vector scan order is deterministic and is the only strong owner.
    (setf *weak-order-roots*
          (ecase order
            (:logical-only (vector old))
            (:logical-first (vector old storage))
            (:physical-first (vector storage old))
            (:weak-only nil))))
  (values))

(defun run-weak-keys ()
  (dolist (order '(:logical-only :logical-first :physical-first))
    (prepare-weak-key-order order)
    (mezzano.internals::gc :full t)
    (multiple-value-bind (key livep)
        (mezzano.internals::weak-pointer-key *weak-order-pointer*)
      (assert livep)
      (assert (eq key (aref *weak-order-roots*
                           (if (eq order :physical-first) 1 0))))))
  (setf *weak-order-roots* nil)
  (prepare-weak-key-order :weak-only)
  (mezzano.internals::gc :full t)
  (assert (not (mezzano.internals::weak-pointer-live-p *weak-order-pointer*)))
  (format t "CLOS-GC-WEAK-KEYS-PASSED~%")
  :passed)
