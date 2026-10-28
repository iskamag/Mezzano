;;;; Load structure-allocation-benchmark.lisp first, then call
;;;; (compare-structure-constructors #P"REMOTE:/path/to/boehm-gc.lisp").
;;;; The benchmark source is compiled unchanged in two separate packages so
;;;; redefining one variant cannot obsolete the other variant's constructors.
(in-package :cl-user)
(defun compare-structure-constructors (source &optional (depth 20) (pairs 4))
  (let* ((generator 'mezzano.internals::generate-simple-defstruct-constructor)
         (original-generator (symbol-function generator))
         (original-benchmark (and (fboundp 'gc-benchmark) (symbol-function 'gc-benchmark)))
         (variants nil))
    (unwind-protect
         (progn
           (dolist (mode '(:ordinary :inline))
             (let* ((package-name (format nil "MEZZANO.CONSTRUCTOR-BENCHMARK.~A" mode))
                    (*package* (or (find-package package-name)
                                   (make-package package-name :use '(:cl)))))
               (setf (symbol-function generator)
                     (if (eq mode :inline)
                         original-generator
                         (lambda (&rest args)
                           (let ((definition (apply original-generator args)))
                             (setf (caadr (second definition)) 'mezzano.internals::maybe-inline)
                             definition))))
               (load (compile-file source
                                   :output-file (merge-pathnames
                                                 (format nil "constructor-~A.llf" mode)
                                                 (user-homedir-pathname))))
               (push (cons mode (symbol-function (find-symbol "GC-BENCHMARK" *package*))) variants)))
           (setf (symbol-function generator) original-generator)
           (dotimes (pair pairs)
             (dolist (mode (if (evenp pair) '(:ordinary :inline) '(:inline :ordinary)))
               (setf (symbol-function 'gc-benchmark) (cdr (assoc mode variants)))
               (format t "~&PAIR ~D ~S~%" pair mode)
               (measure-structure-allocation depth 1))))
      (setf (symbol-function generator) original-generator)
      (if original-benchmark
          (setf (symbol-function 'gc-benchmark) original-benchmark)
          (fmakunbound 'gc-benchmark)))))
