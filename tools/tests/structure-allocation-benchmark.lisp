;;;; Load the unchanged boehm-gc.lisp in CL-USER before loading this file.
;;;; Rep 0 warms caches; compare subsequent reps with the same VM settings.
(in-package :cl-user)
(defun measure-structure-allocation (&optional (depth 20) (repetitions 4))
  (dotimes (rep repetitions)
    (mezzano.internals::gc :full t)
    (let ((start (get-internal-real-time))
          (gc-start mezzano.internals::*gc-time*)
          (bytes (mezzano.supervisor:thread-bytes-consed
                  (mezzano.supervisor:current-thread)))
          (minor mezzano.internals::*gc-minor-cycles*)
          (major mezzano.internals::*gc-major-cycles*))
      (gc-benchmark depth)
      (format t "~&REP ~D depth=~D elapsed=~,3F gc=~,3F bytes=~D minor=~D major=~D~%"
              rep depth
              (/ (- (get-internal-real-time) start)
                 (float internal-time-units-per-second))
              (- mezzano.internals::*gc-time* gc-start)
              (- (mezzano.supervisor:thread-bytes-consed
                  (mezzano.supervisor:current-thread)) bytes)
              (- mezzano.internals::*gc-minor-cycles* minor)
              (- mezzano.internals::*gc-major-cycles* major))
      (finish-output))))

;; Preserve the previous implementation for comparisons in one image.
(defun allocate-structure-by-name (structure-class)
  ;; Match the cold compiler default, independent of boehm-gc.lisp policy.
  (declare (optimize (speed 1) (safety 3) (debug 3)))
  (when (symbolp structure-class)
    (setf structure-class (find-class structure-class)))
  (assert (mezzano.runtime::structure-class-p structure-class))
  (let ((layout (mezzano.runtime::instance-access-by-name
                 structure-class 'mezzano.clos::slot-storage-layout)))
    (mezzano.internals::%allocate-instance layout)))

(defun compare-structure-allocation (&optional (depth 20) (pairs 4))
  ;; Run in a quiescent guest. Replacing the function also affects other
  ;; threads, although both implementations have identical semantics.
  (let* ((name 'mezzano.internals::%allocate-struct)
         (cached (symbol-function name))
         (baseline #'allocate-structure-by-name))
    (unwind-protect
         (dotimes (pair pairs)
           (dolist (variant (if (evenp pair) '(:baseline :cached) '(:cached :baseline)))
             (setf (symbol-function name)
                   (ecase variant (:baseline baseline) (:cached cached)))
             (format t "~&PAIR ~D VARIANT ~S~%" pair variant)
             (measure-structure-allocation depth 1)))
      (setf (symbol-function name) cached))))
