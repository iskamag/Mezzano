;;;; The upstream compatibility predicate from slop before this change.
;;;; Compile and load wired before structure-layout-benchmark.lisp.
(in-package :mezzano.internals)

(defun layout-benchmark-upstream-compatible-p (object layout)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (when (instance-p object)
    (loop with object-layout of-type layout = (%instance-layout object)
          with compatible of-type simple-vector = (layout-compatible object-layout)
          for l across compatible
          thereis (eq l layout))))
