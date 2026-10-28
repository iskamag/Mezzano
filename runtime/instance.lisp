;;;; Low-level support functions for instances.
;;;;
;;;; Instances are the underlying representation of STRUCTURE-OBJECTs,
;;;; STANDARD-OBJECTs, and FUNCALLABLE-STANDARD-OBJECTs.

(in-package :mezzano.runtime)

;;; Location encoding.

(defconstant +location-type+ (byte 4 0))
(defconstant +location-type-t+                0)
(defconstant +location-type-unsigned-byte-8+  1)
(defconstant +location-type-unsigned-byte-16+ 2)
(defconstant +location-type-unsigned-byte-32+ 3)
(defconstant +location-type-unsigned-byte-64+ 4)
(defconstant +location-type-signed-byte-8+    5)
(defconstant +location-type-signed-byte-16+   6)
(defconstant +location-type-signed-byte-32+   7)
(defconstant +location-type-signed-byte-64+   8)
(defconstant +location-type-single-float+     9)
(defconstant +location-type-double-float+     10)
(defconstant +location-type-short-float+      11)

(defun sys.int::%instance-layout (object)
  "Return an instance's direct layout."
  (sys.int::%instance-layout object))

(defun sys.int::%fast-instance-layout-eq-p (object instance-header)
  "Test if an instance's direct layout & tag match instance-header."
  (sys.int::%fast-instance-layout-eq-p object instance-header))

(declaim (inline sys.int::instance-or-funcallable-instance-p))
(defun sys.int::instance-or-funcallable-instance-p (object)
  (and (sys.int::%value-has-tag-p object sys.int::+tag-object+)
       (sys.int::%instance-or-funcallable-instance-p object)))

(defun sys.int::%instance-or-funcallable-instance-p (object)
  (sys.int::%instance-or-funcallable-instance-p object))

(declaim (inline sys.int::instance-p sys.int::funcallable-instance-p))

(defun sys.int::instance-p (object)
  (sys.int::%object-of-type-p object sys.int::+object-tag-instance+))

(defun sys.int::funcallable-instance-p (object)
  (sys.int::%object-of-type-p object sys.int::+object-tag-funcallable-instance+))

(eval-when (:compile-toplevel :load-toplevel :execute)

(defun location-types ()
  '(#.+location-type-t+
    #.+location-type-unsigned-byte-8+
    #.+location-type-unsigned-byte-16+
    #.+location-type-unsigned-byte-32+
    #.+location-type-unsigned-byte-64+
    #.+location-type-signed-byte-8+
    #.+location-type-signed-byte-16+
    #.+location-type-signed-byte-32+
    #.+location-type-signed-byte-64+
    #.+location-type-single-float+
    #.+location-type-double-float+
    #.+location-type-short-float+))

(defun make-location (type offset)
  (logior (dpb type +location-type+ 0)
          (ash offset 4)))

(defun location-type (location)
  (ldb +location-type+ location))

(declaim (inline location-offset-t))
(defun location-offset-t (location)
  "Return the location offset scaled appropriately for %object-ref-t"
  (declare (type fixnum location))
  (ash location -7))

(declaim (inline location-offset))
(defun location-offset (location)
  (ash location -4))

(defun location-type-scale (location-type)
  (ecase location-type
    (#.+location-type-t+                1)
    (#.+location-type-unsigned-byte-8+  1)
    (#.+location-type-unsigned-byte-16+ 2)
    (#.+location-type-unsigned-byte-32+ 4)
    (#.+location-type-unsigned-byte-64+ 8)
    (#.+location-type-signed-byte-8+    1)
    (#.+location-type-signed-byte-16+   2)
    (#.+location-type-signed-byte-32+   4)
    (#.+location-type-signed-byte-64+   8)
    (#.+location-type-single-float+     4)
    (#.+location-type-double-float+     8)
    (#.+location-type-short-float+      2)))

(defun location-type-accessor (location-type)
  (ecase location-type
    (#.+location-type-t+
     'sys.int::%object-ref-t)
    (#.+location-type-unsigned-byte-8+
     'sys.int::%object-ref-unsigned-byte-8-unscaled)
    (#.+location-type-unsigned-byte-16+
     'sys.int::%object-ref-unsigned-byte-16-unscaled)
    (#.+location-type-unsigned-byte-32+
     'sys.int::%object-ref-unsigned-byte-32-unscaled)
    (#.+location-type-unsigned-byte-64+
     'sys.int::%object-ref-unsigned-byte-64-unscaled)
    (#.+location-type-signed-byte-8+
     'sys.int::%object-ref-signed-byte-8-unscaled)
    (#.+location-type-signed-byte-16+
     'sys.int::%object-ref-signed-byte-16-unscaled)
    (#.+location-type-signed-byte-32+
     'sys.int::%object-ref-signed-byte-32-unscaled)
    (#.+location-type-signed-byte-64+
     'sys.int::%object-ref-signed-byte-64-unscaled)
    (#.+location-type-single-float+
     'sys.int::%object-ref-single-float-unscaled)
    (#.+location-type-double-float+
     'sys.int::%object-ref-double-float-unscaled)
    (#.+location-type-short-float+
     'sys.int::%object-ref-short-float-unscaled)))

(defun storage-location-type-accessor (location-type)
  (intern (concatenate 'string "%STORAGE-REF-"
                       (subseq (symbol-name (location-type-accessor location-type))
                               (length "%OBJECT-REF-")))
          :mezzano.internals))

)

;;; Physical storage is a distinct internal reference to the same allocation.
;;; Converting before selecting a layout keeps that storage stable across GC.
(defun sys.int::%instance-storage (object)
  (sys.int::%instance-storage object))
(defun sys.int::%storage-layout (storage)
  (sys.int::%storage-layout storage))
(defun sys.int::%fast-storage-layout-eq-p (storage instance-header)
  (sys.int::%fast-storage-layout-eq-p storage instance-header))
(defun sys.int::%storage-ref-t (storage index)
  (sys.int::%storage-ref-t storage index))
(defun (setf sys.int::%storage-ref-t) (value storage index)
  (setf (sys.int::%storage-ref-t storage index) value))
(declaim (inline (sys.int::cas sys.int::%storage-ref-t)))
(defun (sys.int::cas sys.int::%storage-ref-t) (old new storage index)
  (multiple-value-bind (successp actual-value)
      (sys.int::%cas-storage storage index old new)
    (declare (ignore successp))
    actual-value))

(sys.int::define-object-ref sys.int::%storage-ref-unsigned-byte-8 sys.int::%%storage-ref-unsigned-byte-8
  sys.int::%storage-ref-unsigned-byte-8-unscaled sys.int::%%storage-ref-unsigned-byte-8-unscaled (unsigned-byte 8))
(sys.int::define-object-ref sys.int::%storage-ref-unsigned-byte-16 sys.int::%%storage-ref-unsigned-byte-16
  sys.int::%storage-ref-unsigned-byte-16-unscaled sys.int::%%storage-ref-unsigned-byte-16-unscaled (unsigned-byte 16))
(sys.int::define-object-ref sys.int::%storage-ref-unsigned-byte-32 sys.int::%%storage-ref-unsigned-byte-32
  sys.int::%storage-ref-unsigned-byte-32-unscaled sys.int::%%storage-ref-unsigned-byte-32-unscaled (unsigned-byte 32))
(sys.int::define-object-ref sys.int::%storage-ref-unsigned-byte-64 sys.int::%%storage-ref-unsigned-byte-64
  sys.int::%storage-ref-unsigned-byte-64-unscaled sys.int::%%storage-ref-unsigned-byte-64-unscaled (unsigned-byte 64))
(sys.int::define-object-ref sys.int::%storage-ref-signed-byte-8 sys.int::%%storage-ref-signed-byte-8
  sys.int::%storage-ref-signed-byte-8-unscaled sys.int::%%storage-ref-signed-byte-8-unscaled (signed-byte 8))
(sys.int::define-object-ref sys.int::%storage-ref-signed-byte-16 sys.int::%%storage-ref-signed-byte-16
  sys.int::%storage-ref-signed-byte-16-unscaled sys.int::%%storage-ref-signed-byte-16-unscaled (signed-byte 16))
(sys.int::define-object-ref sys.int::%storage-ref-signed-byte-32 sys.int::%%storage-ref-signed-byte-32
  sys.int::%storage-ref-signed-byte-32-unscaled sys.int::%%storage-ref-signed-byte-32-unscaled (signed-byte 32))
(sys.int::define-object-ref sys.int::%storage-ref-signed-byte-64 sys.int::%%storage-ref-signed-byte-64
  sys.int::%storage-ref-signed-byte-64-unscaled sys.int::%%storage-ref-signed-byte-64-unscaled (signed-byte 64))
(sys.int::define-object-ref sys.int::%storage-ref-short-float sys.int::%%storage-ref-short-float
  sys.int::%storage-ref-short-float-unscaled sys.int::%%storage-ref-short-float-unscaled short-float)
(sys.int::define-object-ref sys.int::%storage-ref-single-float sys.int::%%storage-ref-single-float
  sys.int::%storage-ref-single-float-unscaled sys.int::%%storage-ref-single-float-unscaled single-float)
(sys.int::define-object-ref sys.int::%storage-ref-double-float sys.int::%%storage-ref-double-float
  sys.int::%storage-ref-double-float-unscaled sys.int::%%storage-ref-double-float-unscaled double-float)

(sys.int::define-coercing-float-object-ref
    sys.int::%%storage-ref-short-float sys.int::%%storage-ref-unsigned-byte-16
  sys.int::%%storage-ref-short-float-unscaled sys.int::%%storage-ref-unsigned-byte-16-unscaled
  sys.int::%short-float-as-integer sys.int::%integer-as-short-float)
#-arm64
(sys.int::define-coercing-float-object-ref
    sys.int::%%storage-ref-single-float sys.int::%%storage-ref-unsigned-byte-32
  sys.int::%%storage-ref-single-float-unscaled sys.int::%%storage-ref-unsigned-byte-32-unscaled
  sys.int::%single-float-as-integer sys.int::%integer-as-single-float)
#-arm64
(sys.int::define-coercing-float-object-ref
    sys.int::%%storage-ref-double-float sys.int::%%storage-ref-unsigned-byte-64
  sys.int::%%storage-ref-double-float-unscaled sys.int::%%storage-ref-unsigned-byte-64-unscaled
  sys.int::%double-float-as-integer sys.int::%integer-as-double-float)

#-x86-64
(defun sys.int::%atomic-fixnum-logand-storage (storage index mask)
  (loop for old = (sys.int::%storage-ref-t storage index)
        when (eql (sys.int::cas (sys.int::%storage-ref-t storage index)
                               old (logand old mask)) old)
          do (return (values))))

(macrolet ((def (name-base args)
             `(defun ,(if name-base
                          (list name-base 'instance-access)
                          'instance-access)
                  (,@args object location &optional (index 0))
                (let ((storage (sys.int::%instance-storage object)))
                  (ecase (location-type location)
                  (#.+location-type-t+
                   (funcall
                    #',(if name-base (list name-base 'sys.int::%storage-ref-t) 'sys.int::%storage-ref-t)
                    ,@args
                    storage (+ (location-offset-t location) index)))
                  ,@(loop
                       for type in (location-types)
                       when (not (eql type +location-type-t+))
                       collect `(,type (funcall #',(if name-base
                                                       (list name-base (storage-location-type-accessor type))
                                                       (storage-location-type-accessor type))
                                                ,@args
                                                storage
                                                (+ (location-offset location)
                                                   (* index ,(location-type-scale type)))))))))))
  (def nil ())
  (def setf (value))
  (def sys.int::cas (new old)))

(defun %instance-dcas (instance offset old-1 old-2 new-1 new-2)
  (sys.int::%dcas-storage (sys.int::%instance-storage instance)
                         offset old-1 old-2 new-1 new-2))

(defun instance-dcas (instance loc-1 loc-2 old-1 old-2 new-1 new-2)
  ;; TODO: Eventually combinations of same-width locations could be supported.
  ;; T and UB64, or UB32 and SINGLE-FLOAT, etc
  (when (not (eql (location-type loc-1) (location-type loc-2)))
    (error "DCAS is not supported on locations of disparate types"))
  ;; TODO: Support DCAS on unboxed slots at all.
  (when (not (eql (location-type loc-1) +location-type-t+))
    (error "DCAS is only supported on locations of type T"))
  ;; The compiler has some trouble compiling this function because of the
  ;; large number of registers used by the dcas instruction, exacerbated
  ;; by the non-constant offset.
  ;; Call out to an external function to help it out.
  (let ((ofs-1 (location-offset-t loc-1))
        (ofs-2 (location-offset-t loc-2)))
    (cond ((eql (1+ ofs-1) ofs-2)
           ;; Normal location ordering.
           (%instance-dcas instance ofs-1 old-1 old-2 new-1 new-2))
          ((eql (1+ ofs-2) ofs-1)
           ;; Reversed location ordering.
           (multiple-value-bind (successp value-2 value-1)
               (%instance-dcas instance ofs-2 old-2 old-1 new-2 new-1)
             (values successp value-1 value-2)))
          (t
           (error "DCAS is not supported on discontigious locations ~S and ~S" loc-1 loc-2)))))

(defun slot-location-in-layout (layout slot-name)
  ;; This runs once per allocation on the structure-constructor path, so it
  ;; must stay a bounded fixnum loop. Declaring the index keeps the generic
  ;; >= (and its bignum path) out of the loop the compiler emits.
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let ((instance-slots (sys.int::layout-instance-slots layout)))
    (loop
       ;; Avoid calling length on the slot vector, it's a simple-vector.
       for i of-type fixnum from 0
         below (the fixnum (sys.int::%object-header-data instance-slots))
         by 2
       when (eq slot-name (sys.int::%object-ref-t instance-slots i))
       do (return (sys.int::%object-ref-t instance-slots (the fixnum (1+ i))))
       finally (error "Instance slot ~S missing from layout ~S" slot-name layout))))

(defun instance-slot-location (object slot-name)
  (slot-location-in-layout (sys.int::%storage-layout
                            (sys.int::%instance-storage object))
                           slot-name))

(defun instance-access-by-name (object slot-name &optional (index 0))
  (let ((storage (sys.int::%instance-storage object)))
    (instance-access storage (instance-slot-location storage slot-name) index)))

(defun (setf instance-access-by-name) (value object slot-name &optional (index 0))
  (let ((storage (sys.int::%instance-storage object)))
    (setf (instance-access storage (instance-slot-location storage slot-name) index) value)))

(defun (sys.int::cas instance-access-by-name) (old new object slot-name &optional (index 0))
  (let ((storage (sys.int::%instance-storage object)))
    (sys.int::cas (instance-access storage (instance-slot-location storage slot-name) index) old new)))

(defun sys.int::instance-header-p (object)
  (sys.int::%value-has-tag-p object sys.int::+tag-instance-header+))

(defun %make-instance-header (layout &optional (tag sys.int::+object-tag-instance+))
  (check-type layout sys.int::layout)
  (with-live-objects (layout)
    (sys.int::%%assemble-value
     (logior (ash (sys.int::lisp-object-address layout)
                  sys.int::+object-data-shift+)
             (ash tag
                  sys.int::+object-type-shift+))
     sys.int::+tag-instance-header+)))

(defun %unpack-instance-header (instance-header)
  (sys.int::%%assemble-value
   (ash (sys.int::lisp-object-address instance-header)
        (- sys.int::+object-data-shift+))
   0))

;; Even though LAYOUT has :allocation :wired, we want to avoid allocating individual
;; layouts used by SUPERSEDE-INSTANCE in the wired area if we can avoid it.
(defun copy-layout-with-appropriate-area (layout)
  (let* ((layout-layout (sys.int::%instance-layout layout)) ; quick hack to get the layout of LAYOUT (the layout struct, not the argument)
         (new-layout (%allocate-object sys.int::+object-tag-instance+
                                       (sys.int::lisp-object-address layout-layout)
                                       (sys.int::layout-heap-size layout-layout)
                                       (if (eql (sys.int::layout-area layout) :wired)
                                           :wired
                                           :pinned))))
    (setf (sys.int::%struct-slot new-layout 'sys.int::layout 'sys.int::class)
          (sys.int::layout-class layout))
    (setf (sys.int::%struct-slot new-layout 'sys.int::layout 'sys.int::hash)
          (sys.int::layout-hash layout))
    (setf (sys.int::%struct-slot new-layout 'sys.int::layout 'sys.int::obsolete)
          (sys.int::layout-obsolete layout))
    (setf (sys.int::%struct-slot new-layout 'sys.int::layout 'sys.int::heap-size)
          (sys.int::layout-heap-size layout))
    (setf (sys.int::%struct-slot new-layout 'sys.int::layout 'sys.int::heap-layout)
          (sys.int::layout-heap-layout layout))
    (setf (sys.int::%struct-slot new-layout 'sys.int::layout 'sys.int::area)
          (sys.int::layout-area layout))
    (setf (sys.int::%struct-slot new-layout 'sys.int::layout 'sys.int::instance-slots)
          (sys.int::layout-instance-slots layout))
    (setf (sys.int::%struct-slot new-layout 'sys.int::layout 'sys.int::new-instance)
          (sys.int::layout-new-instance layout))
    (setf (sys.int::%struct-slot new-layout 'sys.int::layout 'sys.int::compatible)
          (sys.int::layout-compatible layout))
    new-layout))

(defun supersede-instance (old-instance replacement)
  ;; Published replacement links are immutable. Follow them to a target which
  ;; cannot itself be snapped while its new private layout is being prepared.
  ;; CAS retries if another updater publishes a replacement in the meantime.
  (when (sys.int::layout-new-instance
         (sys.int::%storage-layout (sys.int::%instance-storage replacement)))
    (error "A supersession replacement must not itself be superseded."))
  (loop with target = old-instance
        for storage = (sys.int::%instance-storage target)
        for layout = (sys.int::%storage-layout storage)
        for next = (sys.int::layout-new-instance layout)
        do (when (eq target replacement)
             (error "An instance cannot supersede itself."))
           (cond (next (setf target next))
                 (t
                  (let* ((callablep (sys.int::funcallable-instance-p target))
                         (function (when callablep
                                     (sys.int::%storage-ref-t
                                      storage sys.int::+funcallable-instance-function+)))
                         (forwarder (when callablep
                                      (lambda (&rest arguments)
                                        (apply replacement arguments))))
                         (new-layout (copy-layout-with-appropriate-area layout))
                         (tag (ash (if callablep
                                       sys.int::+object-tag-funcallable-instance+
                                       sys.int::+object-tag-instance+)
                                   sys.int::+object-type-shift+))
                         (expected (logior
                                    (ash (sys.int::lisp-object-address layout)
                                         sys.int::+object-data-shift+)
                                    tag))
                         (new-header (logior
                                      (ash (sys.int::lisp-object-address new-layout)
                                           sys.int::+object-data-shift+)
                                      tag)))
                    ;; Callable state belongs to the newest logical instance.
                    ;; Old callable objects forward until GC snaps their aliases.
                    ;; The collector never copies stale function state along a
                    ;; chain, so the result is independent of scanning order.
                    (when callablep
                      (setf (sys.int::funcallable-instance-function replacement)
                            function))
                    (setf (sys.int::layout-new-instance new-layout) replacement)
                    (with-live-objects (storage new-layout)
                      (when (sys.int::%cas-storage-header storage expected new-header)
                        (when callablep
                          (setf (sys.int::%storage-ref-t
                                 storage sys.int::+funcallable-instance-function+)
                                forwarder))
                        (return)))))))
  (values))

(in-package :mezzano.internals)

;; Data sharing warning!
;; Freshly allocated objects all share the same layout for their class.
;; A fresh object that's being superseded must allocate a new layout for
;; that specific instance, otherwise setting the new-instance field will
;; supersede **all** instances of that class!
(defstruct (layout
             (:area :wired)
             :sealed
             :slot-locations)
  (class nil :read-only t)
  (hash nil :read-only t)
  (obsolete nil)
  (heap-size nil)
  (heap-layout nil)
  (area nil)
  (instance-slots nil)
  (new-instance nil)
  (compatible nil :read-only t))
