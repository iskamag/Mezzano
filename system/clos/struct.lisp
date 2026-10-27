;;;; MOP support for structures.

(in-package :mezzano.clos)

(defmethod slot-definition-allocation ((slot-definition structure-slot-definition))
  :instance)
(defmethod slot-definition-initargs ((slot-definition structure-slot-definition))
  '())
(defmethod slot-definition-initform ((slot-definition structure-slot-definition))
  (declare (notinline slot-value)) ; bootstrap hack
  (slot-value slot-definition 'initform))
(defmethod slot-definition-initfunction ((slot-definition structure-slot-definition))
  nil)
(defmethod slot-definition-name ((slot-definition structure-slot-definition))
  (declare (notinline slot-value)) ; bootstrap hack
  (slot-value slot-definition 'name))
(defmethod slot-definition-type ((slot-definition structure-slot-definition))
  (declare (notinline slot-value)) ; bootstrap hack
  (slot-value slot-definition 'type))
(defmethod slot-definition-readers ((direct-slot-definition structure-direct-slot-definition))
  '())
(defmethod slot-definition-writers ((direct-slot-definition structure-direct-slot-definition))
  '())
(defmethod slot-definition-location ((effective-slot-definition structure-effective-slot-definition))
  (declare (notinline slot-value)) ; bootstrap hack
  (slot-value effective-slot-definition 'location))

(defgeneric structure-slot-definition-read-only (slot-definition)
  (:method ((slot-definition structure-slot-definition))
    (declare (notinline slot-value)) ; bootstrap hack
    (slot-value slot-definition 'read-only)))
(defgeneric structure-slot-definition-fixed-vector (slot-definition)
  (:method ((slot-definition structure-slot-definition))
    (declare (notinline slot-value)) ; bootstrap hack
    (slot-value slot-definition 'fixed-vector)))
(defgeneric structure-slot-definition-align (slot-definition)
  (:method ((slot-definition structure-slot-definition))
    (declare (notinline slot-value)) ; bootstrap hack
    (slot-value slot-definition 'align)))
(defgeneric structure-slot-definition-dcas-sibling (slot-definition)
  (:method ((slot-definition structure-slot-definition))
    (declare (notinline slot-value)) ; bootstrap hack
    (slot-value slot-definition 'dcas-sibling)))

(defmethod allocate-instance ((class structure-class) &rest initargs)
  (declare (ignore initargs))
  (sys.int::%allocate-struct class))

(defmethod direct-slot-definition-class ((class structure-class) &rest initargs)
  (declare (ignore initargs))
  (find-class 'structure-direct-slot-definition))
(defmethod effective-slot-definition-class ((class structure-class) &rest initargs)
  (declare (ignore initargs))
  (find-class 'structure-effective-slot-definition))

(defmethod initialize-instance :before ((class structure-class) &rest initargs)
  (declare (ignore initargs))
  (error "Cannot initialize structure classes."))

(defmethod reinitialize-instance :before ((class structure-class) &rest initargs)
  (declare (ignore initargs))
  (error "Cannot reinitialize structure classes."))

(defmethod slot-value-using-class ((class structure-class) instance (slot structure-effective-slot-definition))
  (let ((fv (structure-slot-definition-fixed-vector slot)))
    (cond (fv
           (let ((value (make-array fv)))
             (dotimes (i fv)
               (setf (aref value i)
                     (sys.int::%struct-vector-slot instance class slot i)))
             value))
          (t
           (sys.int::%struct-slot instance class slot)))))

(defmethod (setf slot-value-using-class) (new-value (class structure-class) instance (slot structure-effective-slot-definition))
  (when (structure-slot-definition-read-only slot)
    (error "The slot ~S in class ~S is read-only."
           (slot-definition-name slot)
           (class-name class)))
  (let ((fv (structure-slot-definition-fixed-vector slot)))
    (cond (fv
           (dotimes (i fv)
             (setf (sys.int::%struct-vector-slot instance class slot i)
                   (aref new-value i)))
           new-value)
          (t
           (setf (sys.int::%struct-slot instance class slot) new-value)))))

(defmethod (sys.int::cas slot-value-using-class) (old new (class structure-class) instance (slot structure-effective-slot-definition))
  (when (structure-slot-definition-read-only slot)
    (error "The slot ~S in class ~S is read-only."
           (slot-definition-name slot)
           (class-name class)))
  (let ((fv (structure-slot-definition-fixed-vector slot)))
    (cond (fv
           (error "CAS on fixed-vector slots not supported"))
          (t
           (sys.int::cas (sys.int::%struct-slot instance class slot) old new)))))

(defmethod slot-boundp-using-class ((class structure-class) instance (slot structure-effective-slot-definition))
  t)

(defmethod slot-makunbound-using-class ((class structure-class) instance (slot structure-effective-slot-definition))
  (error "Cannot make structure slots unbound."))

(defmethod slot-exists-p-using-class ((class structure-class) object slot-name)
  (std-slot-exists-p object slot-name))

;;; Structure redefinition.

(defun refresh-structure-layout-compatibility (class)
  ;; Compiled structure checks compare layouts.  Preserve each current layout
  ;; when storage is unchanged, but replace its ancestry vector after a move.
  (let* ((layouts (loop for ancestor in (safe-class-precedence-list class)
                        for layout = (safe-class-slot-storage-layout ancestor)
                        when layout collect layout))
         (layout (safe-class-slot-storage-layout class)))
    ;; The slot has no public writer; use the same internal setter used when
    ;; copying layouts, publishing a complete wired vector in one update.
    (setf (sys.int::%struct-slot layout 'sys.int::layout 'sys.int::compatible)
          (make-array (length layouts) :initial-contents layouts :area :wired))))

(defun refresh-structure-subclasses-after-redefinition (class parent-changed)
  ;; Structure classes have a single parent.  Their descendants' precedence
  ;; lists must be refreshed before their cached method dispatch is reused.
  (when parent-changed
    (refresh-structure-layout-compatibility class))
  (dolist (subclass (safe-class-direct-subclasses class))
    (cond ((mezzano.runtime::structure-class-p subclass)
           (setf (safe-class-precedence-list subclass)
                 (cons subclass (safe-class-precedence-list class)))
           (refresh-structure-subclasses-after-redefinition subclass parent-changed))
          (t
           ;; A non-structure subclass uses the usual class finalization path.
           (std-after-reinitialization-for-classes subclass))))
  (dolist (gf (safe-specializer-direct-generic-functions class))
    (reset-gf-emf-table gf))
  (flush-emf-tables-on-class-redefinition class)
  (flush-initarg-caches class))

(defun structure-descendants-compatible-p (class sdef old-layout new-layout)
  ;; Descendants retain their own slot definitions and layouts.  Until those
  ;; can be rebuilt, only reparent a class with descendants when the complete
  ;; effective slot description remains compatible.
  (and (class-layouts-compatible-p old-layout new-layout)
       (= (length (safe-class-slots class))
          (length (sys.int::structure-definition-slots sdef)))
       ;; Cold-built effective slots can be ordered differently from an SDEF.
       ;; Use the linear paired path when possible and otherwise match by name.
       (loop with old-slots = (safe-class-slots class)
             for paired-slot in old-slots
             for new-slot in (sys.int::structure-definition-slots sdef)
             for old-slot = (if (eql (safe-slot-definition-name paired-slot)
                                     (sys.int::structure-slot-definition-name new-slot))
                                paired-slot
                                (find (sys.int::structure-slot-definition-name new-slot)
                                      old-slots :key #'safe-slot-definition-name))
             always (and old-slot
                         (sys.int::structure-slot-definitions-trivially-compatible-p old-slot new-slot)
                         (equal (std-slot-value old-slot 'documentation)
                                (sys.int::structure-slot-definition-documentation new-slot))))))

;; This does not use the normal REINITIALIZE-INSTANCE machinery as
;; the new structure definition has a specific layout that it is expecting
;; the redefined class to follow.
(defun redefine-structure-type (existing-class sdef)
  (assert (not (eql existing-class (find-class 'structure-object)))) ; Can't redefine structure-object.
  (assert (not (class-sealed existing-class)) () "Cannot redefined sealed struct ~S" existing-class)
  (let* ((old-parent (std-slot-value existing-class 'parent))
         (parent-definition (sys.int::structure-definition-parent sdef))
         (new-parent (if parent-definition
                         (sys.int::%defstruct parent-definition)
                         (find-class 'structure-object)))
         (parent-changed (not (eq old-parent new-parent)))
         (prev-layout (class-layout existing-class))
         (new-layout (sys.int::make-layout :class existing-class
                                           :hash (safe-class-hash existing-class)
                                           :obsolete nil
                                           :heap-size (sys.int::structure-definition-size sdef)
                                           :heap-layout (sys.int::layout-heap-layout (sys.int::structure-definition-layout sdef))
                                           :area (sys.int::structure-definition-area sdef)
                                           :instance-slots (sys.int::convert-structure-definition-instance-slots sdef)
                                           ;; Filled before publication if storage changes.
                                           :compatible nil))
         (layout-changed (not (class-layouts-compatible-p prev-layout new-layout))))
    (when (and parent-changed
               (member existing-class (safe-class-precedence-list new-parent)))
      (error "Cannot make ~S a subclass of its own descendant ~S."
             (class-name existing-class) (class-name new-parent)))
    (when (and parent-changed
               (safe-class-direct-subclasses existing-class)
               (not (structure-descendants-compatible-p existing-class sdef prev-layout new-layout)))
      (error "Cannot reparent structure ~S with subclasses while changing its slots."
             (class-name existing-class)))
    (sys.int::populate-struct-class-from-structure-defintion existing-class sdef new-parent)
    (when parent-changed
      (when old-parent
        (remove-direct-subclass old-parent existing-class))
      (add-direct-subclass new-parent existing-class))
    (when layout-changed
      ;; Existing storage must be migrated when its physical layout changes.
      (setf (sys.int::%struct-slot new-layout 'sys.int::layout 'sys.int::compatible)
            (sys.int::make-structure-compatible-layouts new-parent))
      (sys.int::finish-compatible-layouts new-layout)
      (setf (mezzano.runtime::instance-access-by-name existing-class 'mezzano.clos::slot-storage-layout)
            new-layout)
      (setf (sys.int::layout-obsolete prev-layout) new-layout))
    (when (or parent-changed layout-changed)
      (refresh-structure-subclasses-after-redefinition existing-class parent-changed)))
  existing-class)

(defmethod update-instance-for-redefined-class ((instance structure-object) added-slots discarded-slots property-list &rest initargs)
  (check-update-instance-for-redefined-class-initargs instance added-slots discarded-slots property-list initargs)
  (apply #'shared-initialize instance added-slots initargs))

(defmethod shared-initialize ((instance structure-object) slot-names &rest all-keys)
  (dolist (slot (class-slots (class-of instance)))
    (let ((slot-name (slot-definition-name slot)))
      (multiple-value-bind (init-key init-value foundp)
            (get-properties
             all-keys
             (list (intern (string slot-name) 'keyword)))
         (declare (ignore init-key))
         (cond (foundp
                (setf (slot-value instance slot-name) init-value))
               ((or (eq slot-names t)
                    (member slot-name slot-names))
                (setf (slot-value instance slot-name) (eval (slot-definition-initform slot))))))))
  instance)

(defmethod make-instance ((class structure-class) &rest initargs &key &allow-other-keys)
  (when (not (slot-value class 'has-standard-constructor))
    (error "Structure class ~S does not have a standard constructor" class))
  ;; TODO: Permit initargs based on allocate-instance/initialize-instance/shared-initialize as well, like STD-CLASS.
  (unless (getf initargs :allow-other-keys)
    (let ((invalid-initargs
           (loop
              with slots = (class-slots class)
              for (name value) on initargs by #'cddr
              unless (or (eql name :allow-other-keys)
                         (and (keywordp name)
                              (find name slots
                                    :key #'slot-definition-name
                                    :test #'string=)))
              collect name)))
      (when invalid-initargs
        (error "Invalid initargs ~:S when creating instance of ~S (~S)"
               invalid-initargs class (class-name class)))))
  (let ((instance (apply #'allocate-instance class initargs)))
    (apply #'initialize-instance instance initargs)
    instance))

(defmethod initialize-instance ((instance structure-object) &rest initargs)
  (apply #'shared-initialize instance t initargs))
