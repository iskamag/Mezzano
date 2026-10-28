;;;; Faster hash table for doing single-dispatch class to effective method lookup.

(in-package :mezzano.clos)

(defstruct (single-dispatch-emf-table
             (:constructor %make-single-dispatch-emf-table (update-lock))
             :sealed)
  update-lock
  (table (make-fast-class-hash-table)))

(defun make-single-dispatch-emf-table (generic-function)
  (%make-single-dispatch-emf-table (mezzano.supervisor:make-mutex `(single-dispatch-emf-cache ,generic-function))))

(defun single-dispatch-emf-table-count (emf-table)
  (fast-class-hash-table-count
   (single-dispatch-emf-table-table emf-table)))

(defun single-dispatch-emf-entry (emf-table class)
  (fast-class-hash-table-entry
   (single-dispatch-emf-table-table emf-table)
   class))

(declaim (inline single-dispatch-emf-entry-by-object))
(defun single-dispatch-emf-entry-by-object (emf-table object)
  (declare (optimize (speed 3) (safety 0) (debug 1))
           (type single-dispatch-emf-table emf-table))
  (let ((table (single-dispatch-emf-table-table emf-table)))
    (if (sys.int::instance-or-funcallable-instance-p object)
        (loop for current = object then replacement
              for storage = (sys.int::%instance-storage current)
              for layout of-type sys.int::layout = (sys.int::%storage-layout storage)
              for replacement = (sys.int::layout-new-instance layout)
              unless replacement
                do (return (fast-class-hash-table-entry-known-hash
                            table (sys.int::layout-class layout)
                            (sys.int::layout-hash layout))))
        ;; If it's not an instance, then it must be a built-in class
        (let* ((class (built-in-class-of object))
               ;; This is effectively inlining safe-class-hash when we know
               ;; class is a built-in-class.
               (hash (sys.int::%object-ref-t
                      class (mezzano.runtime::location-offset-t
                             *built-in-class-hash-location*))))
              (fast-class-hash-table-entry-known-hash
               table
               class
               hash)))))

(defun (setf single-dispatch-emf-entry) (value emf-table class)
  (mezzano.supervisor:with-mutex ((single-dispatch-emf-table-update-lock emf-table))
    (setf (fast-class-hash-table-entry
           (single-dispatch-emf-table-table emf-table)
           class)
          value)))

(defun clear-single-dispatch-emf-table (emf-table)
  (mezzano.supervisor:with-mutex ((single-dispatch-emf-table-update-lock emf-table))
    (clear-fast-class-hash-table (single-dispatch-emf-table-table emf-table))))
