;;;; Live single-thread CONS comparison. Load cons-tlab-shared-baseline.lisp
;;;; wired, then this file, and call RUN in a one-vCPU guest through its REPL.
(defpackage :mezzano.cons-tlab-benchmark (:use :cl) (:export :run))
(in-package :mezzano.cons-tlab-benchmark)

(defvar *sink* nil)

(defmacro define-allocation-loop (name allocator)
  ;; Amortize loop bookkeeping across 16 real CONS calls in both modes.
  ;; The per-thread byte check below detects any eliminated allocations.
  `(defun ,name (count)
     (declare (optimize (speed 3) (safety 0) (debug 0)) (type fixnum count))
     (let ((head nil) (groups (ash count -4)))
       (declare (type fixnum groups))
       (dotimes (i groups)
         (declare (type fixnum i))
         ,@(loop repeat 16 collect `(setf head (,allocator nil nil))))
       (dotimes (i (logand count 15) head)
         (declare (type fixnum i))
         (setf head (,allocator nil nil))))))

(define-allocation-loop allocate-conses cons)
(define-allocation-loop allocate-shared-conses mezzano.runtime::benchmark-shared-cons)

(defun pre-touch-cons-space (count)
  ;; Commit the existing young area before timing so initial zero-fill page
  ;; faults do not drown out the allocator. Preserve all existing contents.
  (mezzano.supervisor:without-footholds
    (mezzano.supervisor:inhibit-thread-pool-blocking-hijack
      (mezzano.supervisor:with-mutex (mezzano.runtime::*allocator-lock*)
        (mezzano.supervisor:with-pseudo-atomic
          (unless (mezzano.runtime::expand-allocation-area
                   :cons (* (+ count 8192) 16)
                   'mezzano.runtime::*cons-area-expansion-granularity*
                   'mezzano.internals::*cons-area-young-gen-limit*
                   mezzano.internals::+address-tag-cons+)
            (mezzano.supervisor:panic "Unable to prepare cons benchmark area"))))))
  (mezzano.supervisor:with-pseudo-atomic
    (let ((base (logior (ash mezzano.internals::+address-tag-cons+
                             mezzano.internals::+address-tag-shift+)
                       mezzano.internals::*young-gen-newspace-bit*)))
      (loop for offset from 0 below mezzano.internals::*cons-area-young-gen-limit* by 4096
            do (let ((address (+ base offset)))
                 (setf (mezzano.internals::memref-unsigned-byte-8 address)
                       (mezzano.internals::memref-unsigned-byte-8 address)))))))

(defun time-trial (function count)
  (setf *sink* nil)
  (mezzano.internals::gc :full t)
  (pre-touch-cons-space count)
  (setf *sink* (funcall function 4096))
  (let* ((thread (mezzano.supervisor:current-thread))
         (before (mezzano.supervisor:thread-bytes-consed thread))
         (gc-before mezzano.internals::*gc-cycles*)
         (start (mezzano.supervisor:get-high-precision-timer))
         (head (funcall function count))
         (elapsed (- (mezzano.supervisor:get-high-precision-timer) start))
         (bytes (- (mezzano.supervisor:thread-bytes-consed thread) before))
         (gcs (- mezzano.internals::*gc-cycles* gc-before)))
    (setf *sink* head)
    (unless (and (zerop gcs) (= bytes (* count 16)) (consp head)
                 (null (car head)) (null (cdr head)))
      (mezzano.supervisor:panic "Invalid cons benchmark sample"))
    elapsed))

(defun run (&key (count 500000) (pairs 11))
  (let ((shared (make-array pairs)) (tlab (make-array pairs)))
    (format t "~&CONS-LIVE-BEGIN count=~D pairs=~D~%" count pairs)
    (dotimes (pair pairs)
      (if (evenp pair)
          (setf (aref shared pair) (time-trial #'allocate-shared-conses count)
                (aref tlab pair) (time-trial #'allocate-conses count))
          (setf (aref tlab pair) (time-trial #'allocate-conses count)
                (aref shared pair) (time-trial #'allocate-shared-conses count)))
      (format t "CONS-LIVE-PAIR ~D shared=~D tlab=~D speedup=~,4F~%"
              pair (aref shared pair) (aref tlab pair)
              (/ (aref shared pair) (float (aref tlab pair)))))
    (let ((ratios (make-array pairs)))
      (dotimes (pair pairs)
        (setf (aref ratios pair) (/ (aref shared pair) (float (aref tlab pair)))))
      (sort shared #'<) (sort tlab #'<) (sort ratios #'<)
      (format t "CONS-LIVE-MEDIAN shared=~D tlab=~D paired-speedup=~,4F~%"
              (aref shared (floor pairs 2)) (aref tlab (floor pairs 2))
              (aref ratios (floor pairs 2))))
    (format t "CONS-LIVE-PASSED~%")))
