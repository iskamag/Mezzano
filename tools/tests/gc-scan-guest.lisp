;;;; Load into a guest after hot-loading system/gc.lisp wired, then call RUN.
(defpackage :mezzano.gc-scan-test (:use :cl) (:export :run))
(in-package :mezzano.gc-scan-test)

(defstruct (scan-node :sealed) left right payload)
(defstruct (scan-mixed :sealed)
  child (raw 0 :type (unsigned-byte 64)))
(defstruct (scan-pinned (:area :pinned) :sealed) child)
(defvar *root* nil)

(defun install-nil-root (key)
  ;; The only enduring reference to these objects is through NIL's plist.
  (setf (get nil key) (vector (list 17 29) (make-scan-node :payload 43))))

(defun check-nil-root (key)
  (let ((value (get nil key)))
    (assert (equal (aref value 0) '(17 29)))
    (assert (= (scan-node-payload (aref value 1)) 43))))

(defun make-graph ()
  (let* ((shared (list 1 2 3))
         (node (make-scan-node :payload shared))
         (mixed (make-scan-mixed :child node :raw #xffffffffffffffff))
         (pinned (make-scan-pinned :child node))
         (vector (make-array 10000 :initial-element nil))
         (bytes (make-array 70001 :element-type '(unsigned-byte 8)
                                  :initial-element 173)))
    (setf (scan-node-left node) node
          (scan-node-right node) vector
          (aref vector 0) node
          (aref vector 1) shared
          (aref vector 2) mixed
          (aref vector 3) pinned
          (aref vector 4) bytes
          (aref vector 5) #\Z
          (aref vector 6) 1.25f0
          (aref vector 7) most-positive-fixnum
          (aref vector 8) most-negative-fixnum
          (aref vector 9999) node)
    vector))

(defun check-graph ()
  (let* ((v *root*) (node (aref v 0)))
    (assert (eq (scan-node-left node) node))
    (assert (eq (scan-node-right node) v))
    (assert (eq (scan-node-payload node) (aref v 1)))
    (assert (equal (aref v 1) '(1 2 3)))
    (assert (eq (scan-mixed-child (aref v 2)) node))
    (assert (= (scan-mixed-raw (aref v 2)) #xffffffffffffffff))
    (assert (eq (scan-pinned-child (aref v 3)) node))
    (assert (eq (aref v 9999) node))
    (assert (char= (aref v 5) #\Z))
    (assert (= (aref v 6) 1.25f0))
    (assert (= (aref v 7) most-positive-fixnum))
    (assert (= (aref v 8) most-negative-fixnum))
    (assert (every (lambda (x) (= x 173)) (aref v 4)))
    (loop for i from 9 below 9999 do (assert (null (aref v i))))))

(defun run (&key check-nil-plist)
  ;; Enable this additional check after applying the weak NIL-key hash fix.
  ;; NIL's plist also exercises weak-table lookup across deletion and GC.
  (let ((key (when check-nil-plist (gensym "GC-SCAN-NIL-ROOT")))
        (minors mezzano.internals::*gc-minor-cycles*)
        (majors mezzano.internals::*gc-major-cycles*))
    (unwind-protect
         (progn
           (mezzano.internals::scavenge-many 0 0 :minor)
           (mezzano.internals::scavenge-many 0 -1 :major)
           (setf *root* (make-graph))
           (dotimes (round 5)
             ;; Promote the graph, then install young references in old and
             ;; pinned objects and NIL to exercise the remembered set.
             (mezzano.internals::gc :full t)
             (check-graph)
             (when check-nil-plist (install-nil-root key))
             (setf (aref *root* 9998) (make-scan-node :payload round))
             (setf (scan-pinned-child (aref *root* 3))
                   (make-scan-node :payload round))
             (mezzano.internals::gc)
             (assert (= (scan-node-payload (scan-pinned-child (aref *root* 3))) round))
             (setf (scan-pinned-child (aref *root* 3)) (aref *root* 0))
             (when check-nil-plist (check-nil-root key))
             (assert (= (scan-node-payload (aref *root* 9998)) round))
             (setf (aref *root* 9998) nil)
             (check-graph)
             (when check-nil-plist (install-nil-root key))
             (mezzano.internals::gc :full t)
             (when check-nil-plist (check-nil-root key))
             (check-graph))
           (assert (> mezzano.internals::*gc-minor-cycles* minors))
           (assert (> mezzano.internals::*gc-major-cycles* majors))
           (format t "~&GC-SCAN-TEST-PASSED minor=~D major=~D~%"
                   (- mezzano.internals::*gc-minor-cycles* minors)
                   (- mezzano.internals::*gc-major-cycles* majors))
           :passed)
      (when check-nil-plist (remprop nil key))
      (setf *root* nil))))
