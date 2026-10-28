;;;; A weak-key hash-table specialized on classes.
;;;; This efficiently maps from classes to arbitrary values.

;;;; Warning:
;;;; Concurrent reads are thread-safe, but updates must be protected by a lock.
;;;; These functions are unsafe and must be called with the proper arguments,
;;;; ie the table must be a fast-class-hash-table and the class must be a class.

;;;; TODO: Be smarter/more proactive pruning dead weak pointers.

(in-package :mezzano.clos)

;; Must be a power of two.
(sys.int::defglobal *default-fast-class-hash-table-size* 16)

(defstruct (fast-class-hash-table
             (:constructor make-fast-class-hash-table ())
             :sealed)
  ;; Table can use one of three possible representation:
  ;; NIL - There are no entries.
  ;; A weak-pointer - There is a single entry. The key is the class and the value is the value.
  ;; A simple-vector - There are many entries.
  (table nil)
  (count 0))

(declaim (inline fast-class-hash-table-cell-known-hash))
(defun fast-class-hash-table-cell-known-hash (table class hash)
  (declare (optimize speed (safety 0) (debug 1))
           (type fast-class-hash-table table)
           (type fixnum hash))
  (let ((storage (fast-class-hash-table-table table)))
    (cond ((mezzano.extensions:weak-pointer-p storage)
           ;; Single entry.
           ;; TODO: Replace with WEAK-POINTER-PAIR when optimizations are implemented
           (if (eq class (sys.int::%weak-pointer-key (the mezzano.extensions:weak-pointer storage)))
               storage
               nil))
          ((not storage)
           ;; No entries.
           nil)
          (t
           (locally
               (declare (type simple-vector storage))
             ;; Full hash table.
             (do* ((size (the fixnum (sys.int::%object-header-data storage)))
                   (mask (1- size))
                   ;; This hash implementation is inspired by the Python dict implementation.
                   (slot (logand hash #xFFFFFFFF)
                         (logand #xFFFFFFFF
                                 (the fixnum
                                      (+ (the fixnum
                                              (+ (the fixnum
                                                      (* slot 5))
                                                 perturb))
                                         1))))
                   (perturb hash (ash perturb -5)))
                  (nil)
               (declare (type fixnum hash size mask slot perturb))
               (let* ((offset (logand slot mask))
                      ;; The power-of-two table size and masked offset establish bounds.
                      (slot (sys.int::%object-ref-t storage offset)))
                 (declare (type fixnum offset))
                 (when (eq slot nil)
                   ;; Unbound value marks the end of this run.
                   (return nil))
                 (when (and (not (eq slot t))
                            (eq class (sys.int::%weak-pointer-key (the mezzano.extensions:weak-pointer slot))))
                   (return slot)))))))))

(declaim (inline fast-class-hash-table-entry-known-hash))
(defun fast-class-hash-table-entry-known-hash (table class hash)
  (declare (type fast-class-hash-table table)
           (type fixnum hash))
  (let ((cell (fast-class-hash-table-cell-known-hash table class hash)))
    (when cell
      (sys.int::%weak-pointer-value (the mezzano.extensions:weak-pointer cell)))))

(declaim (inline invalidate-fast-class-hash-table-cell))
(defun invalidate-fast-class-hash-table-cell (cell)
  ;; Readers may retain cells outside the backing table. Invalidate a detached
  ;; cell BEFORE publishing its replacement/removal, and reread its value on
  ;; every hit. This also releases its old value without retaining the key.
  ;; Rehashing transfers the same live cells and must not invalidate them.
  (declare (type mezzano.extensions:weak-pointer cell))
  (setf (sys.int::%object-ref-t cell sys.int::+weak-pointer-value+) nil))

(defun fast-class-hash-table-entry (table class)
  (fast-class-hash-table-entry-known-hash table class (safe-class-hash class)))

(defun get-fast-class-hash-table-slot-offset (storage class)
  (do* ((hash (safe-class-hash class))
        (size (sys.int::simple-vector-length storage))
        (free-slot nil)
        ;; This hash implementation is inspired by the Python dict implementation.
        (slot (logand hash #xFFFFFFFF) (logand #xFFFFFFFF (+ (* slot 5) perturb 1)))
        (perturb hash (ash perturb -5)))
       (nil)
    (let* ((offset (rem slot size))
           (slot (svref storage offset)))
      (when (and (not free-slot)
                 (or (eq slot nil)
                     (eq slot t)))
        (setf free-slot offset))
      (when (eq slot nil)
        ;; Unbound value marks the end of this run.
        (return (values nil free-slot)))
      (when (and (not (eq slot t))
                 (eq class (mezzano.extensions:weak-pointer-key slot)))
        (return (values slot offset))))))

(defun (setf fast-class-hash-table-entry) (value table class)
  (cond ((null (fast-class-hash-table-table table))
         (when (not (eql value nil))
           (setf (fast-class-hash-table-table table) (mezzano.extensions:make-weak-pointer class :value value))
           (setf (fast-class-hash-table-count table) 1)))
        ((mezzano.extensions:weak-pointer-p (fast-class-hash-table-table table))
         ;; Take a strong reference to the existing class now.
         (let ((existing-class (mezzano.extensions:weak-pointer-key (fast-class-hash-table-table table))))
           (cond ((or (eql existing-class class)
                      (not existing-class))
                  ;; Same class or the existing entry was actually dead, replace the pointer.
                  (invalidate-fast-class-hash-table-cell (fast-class-hash-table-table table))
                  (cond (value
                         (setf (fast-class-hash-table-table table)
                               (mezzano.extensions:make-weak-pointer class :value value)))
                        (t
                         (setf (fast-class-hash-table-table table) nil)
                         (setf (fast-class-hash-table-count table) 0))))
                 (value
                  ;; Promote to full hash-table
                  (let* ((storage (make-array *default-fast-class-hash-table-size* :initial-element nil)))
                    (multiple-value-bind (existing-slot slot-offset)
                        (get-fast-class-hash-table-slot-offset storage existing-class)
                      (declare (ignore existing-slot))
                      (setf (svref storage slot-offset) (fast-class-hash-table-table table)))
                    (multiple-value-bind (existing-slot slot-offset)
                        (get-fast-class-hash-table-slot-offset storage class)
                      (declare (ignore existing-slot))
                      (setf (svref storage slot-offset) (mezzano.extensions:make-weak-pointer class :value value)))
                    (setf (fast-class-hash-table-table table) storage)
                    (incf (fast-class-hash-table-count table)))))))
        (t
         (multiple-value-bind (existing-slot slot-offset)
             (get-fast-class-hash-table-slot-offset (fast-class-hash-table-table table) class)
           (cond
             ((eql value nil)
              ;; Removing a value.
              (when existing-slot
                (invalidate-fast-class-hash-table-cell existing-slot)
                (decf (fast-class-hash-table-count table))
                ;; Replace with a tombstone marker
                (setf (svref (fast-class-hash-table-table table) slot-offset) t)))
             (existing-slot
              ;; Updating an existing slot.
              (invalidate-fast-class-hash-table-cell existing-slot)
              (setf (svref (fast-class-hash-table-table table) slot-offset) (mezzano.extensions:make-weak-pointer class :value value)))
             ((eql (1+ (fast-class-hash-table-count table))
                   (length (fast-class-hash-table-table table)))
              ;; This would fill the table. Expand it.
              (let* ((old-table (fast-class-hash-table-table table))
                     (new-table (make-array (* (length old-table) 2) :initial-element nil)))
                (setf (fast-class-hash-table-count table) 0)
                (dotimes (i (length old-table))
                  (let ((slot (svref old-table i)))
                    (when (mezzano.extensions:weak-pointer-p slot)
                      (let ((slot-class (mezzano.extensions:weak-pointer-key slot)))
                        (when slot-class ; Check for and prune dead pointers.
                          (multiple-value-bind (existing-slot slot-offset)
                              (get-fast-class-hash-table-slot-offset new-table slot-class)
                            (incf (fast-class-hash-table-count table))
                            (assert (not existing-slot))
                            (setf (svref new-table slot-offset) slot)))))))
                ;; And the new entry.
                (multiple-value-bind (existing-slot slot-offset)
                    (get-fast-class-hash-table-slot-offset new-table class)
                  (assert (not existing-slot))
                  (setf (svref new-table slot-offset) (mezzano.extensions:make-weak-pointer class :value value)))
                (incf (fast-class-hash-table-count table))
                ;; Switch to new table.
                (setf (fast-class-hash-table-table table) new-table)))
             (t ;; Adding a new entry.
              (setf (svref (fast-class-hash-table-table table) slot-offset)
                    (mezzano.extensions:make-weak-pointer class :value value))
              (incf (fast-class-hash-table-count table)))))))
  value)

(defun map-fast-class-hash-table (fn table)
  (let ((table (fast-class-hash-table-table table)))
    (cond ((not table)
           nil)
          ((consp table)
           (funcall fn (car table) (cdr table)))
          (t
           (dotimes (i (length table))
             (let ((slot (svref table i)))
               (when (mezzano.extensions:weak-pointer-p slot)
                 (multiple-value-bind (class value)
                     (mezzano.extensions:weak-pointer-pair slot)
                   (when class
                     (funcall fn class value))))))))))

(defun clear-fast-class-hash-table (table)
  ;; A discriminator can still hold a cell from this table after the table is
  ;; cleared, so dropping the backing storage alone does not invalidate it.
  (let ((storage (fast-class-hash-table-table table)))
    (cond ((mezzano.extensions:weak-pointer-p storage)
           (invalidate-fast-class-hash-table-cell storage))
          (storage
           (dotimes (i (length storage))
             (let ((cell (svref storage i)))
               (when (mezzano.extensions:weak-pointer-p cell)
                 (invalidate-fast-class-hash-table-cell cell)))))))
  (setf (fast-class-hash-table-table table) nil
        (fast-class-hash-table-count table) 0))
