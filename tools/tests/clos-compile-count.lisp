;;;; Count operations during one real compilation, separately from timing.
(in-package :clos-compile)
(defvar *counts* nil)
(defvar *read-layouts* nil)

(defun count-operations ()
  (let* ((names '(mezzano.clos::fast-slot-read mezzano.clos::fast-slot-write
                  mezzano.clos::single-dispatch-emf-entry-by-object
                  mezzano.clos::fast-class-hash-table-entry-known-hash
                  mezzano.clos::emf-cache-lookup
                  mezzano.clos::slow-single-dispatch-method-lookup
                  mezzano.clos::slow-single-dispatch-method-lookup*
                  mezzano.clos::slow-method-lookup))
         (originals (mapcar (lambda (name) (cons name (fdefinition name))) names)))
    (compile-workload)
    (unwind-protect
         (progn
           (dolist (entry originals)
             (let ((name (car entry)) (original (cdr entry)))
               (setf (fdefinition name)
                     (lambda (&rest args)
                       (declare (dynamic-extent args))
                       (when *counts*
                         (let ((counts *counts*) (layouts *read-layouts*) (*counts* nil))
                           (incf (gethash name counts 0))
                           (when (eq name 'mezzano.clos::fast-slot-read)
                             (incf (gethash (mezzano.internals::%instance-layout (first args))
                                            layouts 0)))
                           (when (eq name 'mezzano.clos::fast-class-hash-table-entry-known-hash)
                             (let ((storage (mezzano.clos::fast-class-hash-table-table (first args))))
                               (incf (gethash (cond ((null storage) :empty)
                                                   ((mezzano.extensions:weak-pointer-p storage) :single)
                                                   (t :hashed)) counts 0))
                               (when (simple-vector-p storage)
                                 (incf (gethash (length storage) counts 0))
                                 (let* ((class (second args)) (hash (third args))
                                        (mask (1- (length storage)))
                                        (slot (logand hash #xffffffff))
                                        (perturb hash) (probes 0))
                                   (loop
                                     (incf probes)
                                     (let ((entry (svref storage (logand slot mask))))
                                       (when (or (null entry)
                                                 (and (not (eq entry t))
                                                      (eq class (mezzano.internals::%weak-pointer-key entry))))
                                         (return)))
                                     (setf slot (logand #xffffffff (+ (* slot 5) perturb 1))
                                           perturb (ash perturb -5)))
                                   (incf (gethash :probe-sum counts 0) probes)
                                   (setf (gethash :max-probes counts 0)
                                         (max probes (gethash :max-probes counts 0)))
                                   (when (> probes 1) (incf (gethash :collision counts 0)))))))))
                       (apply original args)))))
           (let* ((*counts* (make-hash-table)) (*read-layouts* (make-hash-table))
                  (counts *counts*) (layouts *read-layouts*))
             (compile-workload)
             (let ((*counts* nil))
               (maphash (lambda (name count) (format t "COUNT ~S ~D~%" name count)) counts)
               (let ((ordered nil))
                 (maphash (lambda (layout count) (push (cons layout count) ordered)) layouts)
                 (loop for (layout . count) in (subseq (sort ordered #'> :key #'cdr) 0
                                                     (min 20 (length ordered)))
                       do (format t "READ-CLASS ~S ~D~%"
                                  (class-name (mezzano.internals::layout-class layout)) count))))))
      (dolist (entry originals) (setf (fdefinition (car entry)) (cdr entry))))))

(count-operations)
