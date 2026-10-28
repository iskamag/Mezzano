;;;; Load structure-allocation-benchmark.lisp before this file.
(in-package :cl-user)

;; The a06ac410 allocator, with its original policy and helper calls.
(defun allocate-struct-helper-baseline (class)
  (declare (optimize (speed 1) (safety 3) (debug 3))
           (notinline mezzano.runtime::structure-class-p mezzano.internals::%allocate-instance))
  (when (symbolp class) (setf class (find-class class)))
  (assert (mezzano.runtime::structure-class-p class))
  (let ((layout (mezzano.internals::%object-ref-t
                 class (mezzano.runtime::location-offset-t
                        (the fixnum mezzano.runtime::*structure-class-slot-storage-layout-location*)))))
    (mezzano.internals::%allocate-instance layout)))

(defun compare-allocation-helpers (source &optional (depth 20) (pairs 4))
  (let* ((helper 'mezzano.runtime::%allocate-struct-known-class)
         (original-helper (symbol-function helper))
         (original-benchmark (and (fboundp 'gc-benchmark) (symbol-function 'gc-benchmark)))
         (variants nil))
    (unwind-protect
         (progn
           (dolist (mode '(:baseline :inline))
             (proclaim `(,(if (eq mode :inline) 'inline 'notinline) ,helper))
             (let* ((package-name (format nil "MEZZANO.HELPER-BENCHMARK.~A" mode))
                    (*package* (or (find-package package-name)
                                   (make-package package-name :use '(:cl)))))
               (load (compile-file source
                                   :output-file (merge-pathnames
                                                 (format nil "allocation-helper-~A.llf" mode)
                                                 (user-homedir-pathname))))
               (push (cons mode (symbol-function (find-symbol "GC-BENCHMARK" *package*))) variants)))
           (proclaim `(inline ,helper))
           (dotimes (pair pairs)
             (dolist (mode (if (evenp pair) '(:baseline :inline) '(:inline :baseline)))
               (setf (symbol-function helper)
                     (if (eq mode :baseline) #'allocate-struct-helper-baseline original-helper)
                     (symbol-function 'gc-benchmark) (cdr (assoc mode variants)))
               (format t "~&PAIR ~D ~S~%" pair mode)
               (measure-structure-allocation depth 1))))
      (setf (symbol-function helper) original-helper)
      (proclaim `(inline ,helper))
      (if original-benchmark
          (setf (symbol-function 'gc-benchmark) original-benchmark)
          (fmakunbound 'gc-benchmark)))))
