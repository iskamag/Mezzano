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

(defun structure-definition-layout-compatible-p (layout sdef)
  ;; Compare the prospective layout without allocating it.  The instance
  ;; slots contain alternating names and encoded locations.
  (let ((instance-slots (sys.int::layout-instance-slots layout))
        (new-slots (sys.int::structure-definition-slots sdef)))
    (and (eql (sys.int::layout-heap-size layout)
              (sys.int::structure-definition-size sdef))
         (equal (sys.int::layout-heap-layout layout)
                (sys.int::layout-heap-layout (sys.int::structure-definition-layout sdef)))
         (equal (sys.int::layout-area layout) (sys.int::structure-definition-area sdef))
         (vectorp instance-slots)
         (= (length instance-slots) (* 2 (length new-slots)))
         (loop for slot in new-slots
               for i from 0 by 2
               always (and (eql (aref instance-slots i)
                                (sys.int::structure-slot-definition-name slot))
                           (eql (aref instance-slots (1+ i))
                                (sys.int::structure-slot-definition-location slot)))))))

(defun structure-descendants-compatible-p (class sdef layout-compatible-p)
  ;; If the complete inherited slot description is unchanged, descendants
  ;; can retain their existing metadata and layouts.
  (and layout-compatible-p
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
             always (sys.int::structure-slot-definitions-trivially-compatible-p old-slot new-slot t))))

(defun structure-has-direct-subclasses-p (class)
  ;; Avoid materializing a weak list when it is already empty.  For a
  ;; nonempty list use the normal reader, which discards dead references.
  (let ((subclasses (std-slot-value class 'direct-subclasses)))
    (and subclasses
         (mezzano.garbage-collection.weak-objects::weak-list-objects subclasses)
         (safe-class-direct-subclasses class))))

(defun structure-accessors-compatible-p (class definition)
  ;; A fixed vector can grow inside the same final machine word, and two
  ;; different slot types can use the same boxed representation.  Both
  ;; changes require invalidating compiled accessors even when offsets match.
  (let ((old (safe-class-slots class)))
    (loop for paired-slot in old
          for slot in (sys.int::structure-definition-slots definition)
          for name = (sys.int::structure-slot-definition-name slot)
          for old-slot = (if (eql (safe-slot-definition-name paired-slot) name)
                             paired-slot
                             (find name old :key #'safe-slot-definition-name))
          always (and old-slot
                      (equal (sys.int::structure-slot-definition-type slot)
                             (mezzano.runtime::instance-access-by-name old-slot 'type))
                      (eql (sys.int::structure-slot-definition-fixed-vector slot)
                           (mezzano.runtime::instance-access-by-name old-slot 'fixed-vector))))))

(defun structure-accessor-flags-compatible-p (class definition)
  ;; With the same layout, type and vector length, only READ-ONLY changes
  ;; which accessor functions need to be generated.
  (let ((old (safe-class-slots class)))
    (loop for slot in (sys.int::structure-definition-slots definition)
          for old-slot = (find (sys.int::structure-slot-definition-name slot) old
                               :key #'safe-slot-definition-name)
          always (and old-slot
                      (eql (sys.int::structure-slot-definition-read-only slot)
                           (mezzano.runtime::instance-access-by-name old-slot 'read-only))))))

(defun structure-slot-description-name (description)
  (if (symbolp description) description (car description)))

(defun rebuild-structure-descendants (class parent-definition parent-recipe)
  ;; Construct all prospective definitions before mutating any class.  In
  ;; particular an old :INCLUDE override may no longer name a parent slot.
  (loop for child in (safe-class-direct-subclasses class)
        when (mezzano.runtime::structure-class-p child)
        nconc
        (let* ((recipe (std-slot-value child 'definition-recipe))
               (direct (getf recipe :direct-slots))
               (overrides (getf recipe :included-slots))
               (parent-names (mapcar #'sys.int::structure-slot-definition-name
                                     (sys.int::structure-definition-slots parent-definition))))
          (unless recipe
            (error "No definition information available for structure ~S." (class-name child)))
          (dolist (description direct)
            (when (member (structure-slot-description-name description) parent-names)
              (error "Slot ~S of structure ~S conflicts with its included structure."
                     (structure-slot-description-name description) (class-name child))))
          (let ((*package* (find-package (getf recipe :package))))
            (multiple-value-bind (slots layout size)
                (sys.int::compute-defstruct-slots (getf recipe :conc-name) direct
                                                  parent-definition overrides t)
              (let* ((definition
                       (sys.int::make-struct-definition
                        (class-name child) slots parent-definition
                        (class-allocation-area child) size layout (class-sealed child)
                        (documentation child t) (std-slot-value child 'has-standard-constructor)))
                     (new-recipe (copy-list recipe)))
                (setf (getf new-recipe :constructor-token)
                      (make-symbol "STRUCTURE-CONSTRUCTOR-VERSION"))
                (setf (getf new-recipe :initializers)
                      (loop for slot in slots
                            for name = (sys.int::structure-slot-definition-name slot)
                            for own = (or (find name direct :key #'structure-slot-description-name)
                                          (find-if (lambda (description)
                                                     (and (eql name (structure-slot-description-name description))
                                                          (cdr description)))
                                                   overrides))
                            for source = (if own recipe (or parent-recipe recipe))
                            for initializer = (assoc name (getf source :initializers))
                            when initializer collect initializer))
                (cons (list child definition new-recipe nil)
                      (rebuild-structure-descendants child definition new-recipe))))))))

(defun current-structure-constructor-default (name slot-name old-form old-default token)
  (let* ((class (find-class name))
         (slot (find slot-name (class-slots class) :key #'slot-definition-name))
         (recipe (std-slot-value class 'definition-recipe))
         (old-recipe (and token (boundp token) (symbol-value token)))
         (old-initializer (cdr (assoc slot-name (getf old-recipe :initializers))))
         (initializer (cdr (assoc slot-name (getf recipe :initializers)))))
    (cond ((null slot) (funcall old-default))
          ((and (equal old-form (slot-definition-initform slot))
                (eq old-initializer initializer))
           (funcall old-default))
          (initializer (funcall initializer))
          (t (eval (slot-definition-initform slot))))))

(defun construct-rebuilt-standard (name initargs)
  (let* ((class (find-class name))
         (arguments
           (loop for slot in (class-slots class)
                 for slot-name = (slot-definition-name slot)
                 for key = (intern (string slot-name) 'keyword)
                 append (multiple-value-bind (found-key value foundp)
                            (get-properties initargs (list key))
                          (declare (ignore found-key))
                          (when foundp (list (cons slot-name value)))))))
    (construct-rebuilt-boa name arguments nil)))

(defun construct-rebuilt-boa (name arguments old-defaults &optional token)
  (let* ((class (find-class name))
         (recipe (std-slot-value class 'definition-recipe))
         (object (sys.int::%allocate-struct class)))
    (dolist (slot (class-slots class))
      (let* ((slot-name (slot-definition-name slot))
             (argument (assoc slot-name arguments))
             (old-default (assoc slot-name old-defaults))
             (initializer (cdr (assoc slot-name (getf recipe :initializers))))
             (value (cond (argument (cdr argument))
                          ((and old-default
                                (equal (second old-default) (slot-definition-initform slot))
                                (eq (cdr (assoc slot-name
                                                (getf (and token (boundp token) (symbol-value token))
                                                      :initializers)))
                                    initializer))
                           (funcall (third old-default)))
                          (initializer (funcall initializer))
                          (t (eval (slot-definition-initform slot)))))
             (fixed (structure-slot-definition-fixed-vector slot)))
        (unless (typep value (slot-definition-type slot))
          (error "The value ~S for slot ~S is not of type ~S."
                 value slot-name (slot-definition-type slot)))
        (if fixed
            (dotimes (i fixed)
              (setf (sys.int::%struct-vector-slot object class slot i) value))
            (setf (sys.int::%struct-slot object class slot) value))))
    object))

(defun rebuilt-boa-default-form (initializer variables)
  (if (and (consp initializer) (eq (car initializer) :constant))
      (cdr initializer)
      `(funcall ',initializer ,@variables)))

(defun rebuilt-boa-lambda-list (blueprint)
  (append (getf blueprint :required)
          (when (getf blueprint :optional)
            (cons '&optional
                  (loop for (name initializer variables supplied) in (getf blueprint :optional)
                        collect (append (list name (rebuilt-boa-default-form initializer variables))
                                        (when supplied (list supplied))))))
          (when (getf blueprint :rest) (list '&rest (getf blueprint :rest)))
          (when (getf blueprint :enable-keys)
            (cons '&key
                  (loop for (pair initializer variables supplied) in (getf blueprint :keys)
                        collect (append (list pair (rebuilt-boa-default-form initializer variables))
                                        (when supplied (list supplied))))))
          (when (getf blueprint :allow-other-keys) (list '&allow-other-keys))
          (when (getf blueprint :aux)
            (cons '&aux
                  (loop for (name initializer variables) in (getf blueprint :aux)
                        collect (list name (rebuilt-boa-default-form initializer variables)))))
          (when (getf blueprint :closure) (list 'sys.int::&closure (getf blueprint :closure)))
          (when (getf blueprint :count) (list 'sys.int::&count (getf blueprint :count)))))

(defun rebuild-boa-constructor (class definition recipe blueprint)
  (let* ((variables (getf blueprint :variables))
         (original-initializers (getf recipe :boa-original-initializers))
         (initializers (getf recipe :initializers))
         (original-initforms
           (loop for slot in (sys.int::structure-definition-slots definition)
                 collect (cons (sys.int::structure-slot-definition-name slot)
                               (sys.int::structure-slot-definition-initform slot))))
         (slots
           (loop for slot in (sys.int::structure-definition-slots definition)
                 for name = (sys.int::structure-slot-definition-name slot)
                 for initform = (sys.int::structure-slot-definition-initform slot)
                 for factory = (assoc name (getf blueprint :slot-defaults))
                 for initializer = (cdr (assoc name initializers))
                 collect
                 (sys.int::make-struct-slot-definition
                  name (sys.int::structure-slot-definition-accessor slot)
                  (cond ((and factory (equal (second factory) initform)
                              (eq (cdr (assoc name original-initializers)) initializer))
                         `(funcall ',(third factory) ,@variables))
                        (initializer `(funcall ',initializer))
                        (t initform))
                  (sys.int::structure-slot-definition-type slot)
                  (sys.int::structure-slot-definition-read-only slot)
                  (sys.int::structure-slot-definition-location slot)
                  (sys.int::structure-slot-definition-fixed-vector slot)
                  (sys.int::structure-slot-definition-align slot)
                  (sys.int::structure-slot-definition-dcas-sibling slot)
                  (sys.int::structure-slot-definition-documentation slot))))
         (constructor-definition
           (sys.int::make-struct-definition
            (class-name class) slots (sys.int::structure-definition-parent definition)
            (sys.int::structure-definition-area definition)
            (sys.int::structure-definition-size definition)
            (sys.int::layout-heap-layout (sys.int::structure-definition-layout definition))
            (sys.int::structure-definition-sealed definition)
            (sys.int::structure-definition-docstring definition)
            (sys.int::structure-definition-has-standard-constructor definition)))
         (name (getf blueprint :name)))
    (eval (sys.int::generate-defstruct-constructor
           (class-name class) constructor-definition name (rebuilt-boa-lambda-list blueprint)
           (sys.int::structure-definition-area definition) (getf recipe :constructor-token)
           original-initforms))
    (compile name)))

(defun rebuild-structure-descendant-functions (class definition recipe &optional (rebuild-accessors t))
  ;; Recompile the usual constructors with the new keyword list and slots.
  ;; The initializer functions retain the lexical environment of DEFSTRUCT.
  (let* ((*package* (find-package (getf recipe :package)))
         (slots (loop for slot in (sys.int::structure-definition-slots definition)
                      for initializer = (cdr (assoc (sys.int::structure-slot-definition-name slot)
                                                    (getf recipe :initializers)))
                      collect (sys.int::make-struct-slot-definition
                               (sys.int::structure-slot-definition-name slot)
                               (sys.int::structure-slot-definition-accessor slot)
                               (if (and initializer
                                        (not (constantp (sys.int::structure-slot-definition-initform slot))))
                                   `(funcall ',initializer)
                                   (sys.int::structure-slot-definition-initform slot))
                               (sys.int::structure-slot-definition-type slot)
                               (sys.int::structure-slot-definition-read-only slot)
                               (sys.int::structure-slot-definition-location slot)
                               (sys.int::structure-slot-definition-fixed-vector slot)
                               (sys.int::structure-slot-definition-align slot)
                               (sys.int::structure-slot-definition-dcas-sibling slot)
                               (sys.int::structure-slot-definition-documentation slot))))
         (constructor-definition
           (sys.int::make-struct-definition
            (class-name class) slots (sys.int::structure-definition-parent definition)
            (sys.int::structure-definition-area definition)
            (sys.int::structure-definition-size definition)
            (sys.int::layout-heap-layout (sys.int::structure-definition-layout definition))
            (sys.int::structure-definition-sealed definition)
            (sys.int::structure-definition-docstring definition)
            (sys.int::structure-definition-has-standard-constructor definition))))
    (when rebuild-accessors
      (dolist (slot (sys.int::structure-definition-slots definition))
        (eval (if (sys.int::structure-slot-definition-fixed-vector slot)
                  (sys.int::generate-normal-defstruct-slot-vector-accessor
                   (class-name class) definition slot)
                  (sys.int::generate-normal-defstruct-slot-accessor
                   (class-name class) definition slot)))
        (let ((accessor (sys.int::structure-slot-definition-accessor slot)))
          (compile accessor)
          (unless (sys.int::structure-slot-definition-read-only slot)
            (compile `(setf ,accessor))
            (compile `(sys.int::cas ,accessor))))))
    (dolist (constructor (getf recipe :constructors))
      (if (symbolp constructor)
          (progn
            (eval (sys.int::generate-simple-defstruct-constructor
                   (class-name class) constructor-definition constructor
                   (sys.int::structure-definition-area definition)
                   (getf recipe :constructor-token)))
            (compile constructor))
          (let ((blueprint (find (first constructor) (getf recipe :boa-blueprints)
                                 :key (lambda (item) (getf item :name)))))
            (if blueprint
                (rebuild-boa-constructor class definition recipe blueprint)
                (setf (fdefinition (first constructor))
                      (cdr (assoc (first constructor) (getf recipe :boa-functions))))))))))

(defun finish-repeated-structure-definition (name original-token)
  ;; Re-executing one compiled DEFSTRUCT site reuses its literal token.  The
  ;; earlier functions must stay invalidated, while this installation gets a
  ;; fresh token and newly compiled constructors.
  (let* ((class (find-class name))
         (recipe (std-slot-value class 'definition-recipe)))
    (unless (eq original-token (getf recipe :constructor-token))
      (rebuild-structure-descendant-functions
       class (sys.int::convert-structure-class-to-structure-definition class) recipe nil))))

;; This does not use the normal REINITIALIZE-INSTANCE machinery as
;; the new structure definition has a specific layout that it is expecting
;; the redefined class to follow.
(defun redefine-structure-type (existing-class sdef &optional recipe refresh-initializers)
  (assert (not (eql existing-class (find-class 'structure-object)))) ; Can't redefine structure-object.
  (assert (not (class-sealed existing-class)) () "Cannot redefined sealed struct ~S" existing-class)
  (let* ((old-parent (std-slot-value existing-class 'parent))
         (parent-definition (sys.int::structure-definition-parent sdef))
         (new-parent (if parent-definition
                         (sys.int::%defstruct parent-definition)
                         (find-class 'structure-object)))
         (parent-changed (not (eq old-parent new-parent)))
         (prev-layout (class-layout existing-class))
         (layout-compatible-p (and (structure-definition-layout-compatible-p prev-layout sdef)
                                   (structure-accessors-compatible-p existing-class sdef)))
         (rebuild-descendants nil))
    (when (and parent-changed
               (member existing-class (safe-class-precedence-list new-parent)))
      (error "Cannot make ~S a subclass of its own descendant ~S."
             (class-name existing-class) (class-name new-parent)))
    (when (and (structure-has-direct-subclasses-p existing-class)
               (or refresh-initializers
                   (not (structure-descendants-compatible-p existing-class sdef layout-compatible-p))))
      (setf rebuild-descendants
            (rebuild-structure-descendants
             existing-class sdef (or recipe (std-slot-value existing-class 'definition-recipe)))))
    ;; Once preflight succeeds, prevent old constructor bodies from using the
    ;; fast path while this class and its descendants are being changed.
    (let* ((old-recipe (std-slot-value existing-class 'definition-recipe))
           (token (getf old-recipe :constructor-token)))
      (when token
        (setf (symbol-value token) old-recipe)))
    (unless layout-compatible-p
      (setf (gethash prev-layout *obsolete-structure-layout-slots*)
            (safe-class-slots existing-class)))
    (sys.int::populate-struct-class-from-structure-defintion existing-class sdef new-parent)
    (when parent-changed
      (when old-parent
        (remove-direct-subclass old-parent existing-class))
      (add-direct-subclass new-parent existing-class))
    (when (not layout-compatible-p)
      ;; Existing storage must be migrated when its physical layout changes.
      (let ((new-layout
              (sys.int::finish-compatible-layouts
               (sys.int::make-layout :class existing-class
                                     :hash (safe-class-hash existing-class)
                                     :obsolete nil
                                     :heap-size (sys.int::structure-definition-size sdef)
                                     :heap-layout (sys.int::layout-heap-layout (sys.int::structure-definition-layout sdef))
                                     :area (sys.int::structure-definition-area sdef)
                                     :instance-slots (sys.int::convert-structure-definition-instance-slots sdef)
                                     :compatible (sys.int::make-structure-compatible-layouts new-parent)))))
        (setf (mezzano.runtime::instance-access-by-name existing-class 'mezzano.clos::slot-storage-layout)
              new-layout)
        (setf (sys.int::layout-obsolete prev-layout) new-layout)))
    ;; A stale constructor can run while descendant functions are compiled.
    ;; Publish the new defaults as soon as this class's own layout is ready.
    (when recipe
      (setf (std-slot-value existing-class 'definition-recipe) recipe))
    (dolist (entry rebuild-descendants)
      (destructuring-bind (child child-definition child-recipe &optional rebuild-accessors) entry
        (declare (ignore rebuild-accessors))
        (let* ((parent (std-slot-value child 'parent))
               (old-layout (class-layout child))
               (same-layout (and (structure-definition-layout-compatible-p old-layout child-definition)
                                 (structure-accessors-compatible-p child child-definition))))
          (setf (fourth entry)
                (not (and same-layout (structure-accessor-flags-compatible-p child child-definition))))
          (unless same-layout
            (setf (gethash old-layout *obsolete-structure-layout-slots*)
                  (safe-class-slots child)))
          (let* ((previous-recipe (std-slot-value child 'definition-recipe))
                 (token (getf previous-recipe :constructor-token)))
            (when token
              (setf (symbol-value token) previous-recipe)))
          (sys.int::populate-struct-class-from-structure-defintion child child-definition parent)
          (setf (std-slot-value child 'definition-recipe) child-recipe)
          (unless same-layout
            (let ((new-layout
                    (sys.int::finish-compatible-layouts
                     (sys.int::make-layout :class child :hash (safe-class-hash child)
                                           :obsolete nil
                                           :heap-size (sys.int::structure-definition-size child-definition)
                                           :heap-layout (sys.int::layout-heap-layout
                                                         (sys.int::structure-definition-layout child-definition))
                                           :area (sys.int::structure-definition-area child-definition)
                                           :instance-slots (sys.int::convert-structure-definition-instance-slots child-definition)
                                           :compatible (sys.int::make-structure-compatible-layouts parent)))))
              (setf (std-slot-value child 'slot-storage-layout) new-layout
                    (sys.int::layout-obsolete old-layout) new-layout))))))
    (when (or parent-changed (not layout-compatible-p) rebuild-descendants)
      (refresh-structure-subclasses-after-redefinition existing-class t))
    (dolist (entry rebuild-descendants)
      (apply #'rebuild-structure-descendant-functions entry)))
  existing-class)

(defmethod update-instance-for-redefined-class ((instance structure-object) added-slots discarded-slots property-list &rest initargs)
  (check-update-instance-for-redefined-class-initargs instance added-slots discarded-slots property-list initargs)
  (apply #'shared-initialize instance added-slots initargs))

(defmethod shared-initialize ((instance structure-object) slot-names &rest all-keys)
  (let* ((class (class-of instance))
         (recipe (std-slot-value class 'definition-recipe)))
    (dolist (slot (class-slots class))
      (let ((slot-name (slot-definition-name slot)))
        (multiple-value-bind (init-key init-value foundp)
            (get-properties all-keys (list (intern (string slot-name) 'keyword)))
          (declare (ignore init-key))
          (cond (foundp
                 (if (structure-slot-definition-read-only slot)
                     (let ((fixed (structure-slot-definition-fixed-vector slot)))
                       (if fixed
                           (dotimes (i fixed)
                             (setf (sys.int::%struct-vector-slot instance class slot i)
                                   (aref init-value i)))
                           (setf (sys.int::%struct-slot instance class slot) init-value)))
                     (setf (slot-value instance slot-name) init-value)))
                ((or (eq slot-names t)
                     (member slot-name slot-names))
                 (let* ((initializer (cdr (assoc slot-name (getf recipe :initializers))))
                        (value (if initializer (funcall initializer)
                                   (eval (slot-definition-initform slot))))
                        (fixed (structure-slot-definition-fixed-vector slot)))
                   (if fixed
                       (dotimes (i fixed)
                         (setf (sys.int::%struct-vector-slot instance class slot i) value))
                       (if (structure-slot-definition-read-only slot)
                           (setf (sys.int::%struct-slot instance class slot) value)
                           (setf (slot-value instance slot-name) value))))))))))
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
