;;;; COMPILE-FILE must not copy imported uninterned inline literals.
(defpackage :mezzano.inline-literal-identity-test (:use :cl))
(in-package :mezzano.inline-literal-identity-test)

(defun run ()
  (let* ((*package* (find-package :mezzano.inline-literal-identity-test))
         (sentinel (make-symbol "SENTINEL"))
         (source (merge-pathnames "inline-literal-identity-test.lisp" (user-homedir-pathname)))
         (output (compile-file-pathname source)))
    (eval '(declaim (inline imported-p)))
    (eval `(defun imported-p (value) (eq value ',sentinel)))
    (unwind-protect
         (progn
           (with-open-file (out source :direction :output :if-exists :supersede)
             ;; Explicit reader labels preserve sharing even on printers that
             ;; don't label repeated uninterned symbols with *PRINT-CIRCLE*.
             (write-string
              "(progn
                 (defun imported-call (value) (imported-p value))
                 (declaim (inline local-p))
                 (defun local-p (value) (eq value '#1=#:sentinel))
                 (defun local-value () '#1#)
                 (defun local-call (value) (local-p value)))"
              out))
           (dolist (parallel '(nil :force))
             (load (compile-file source :output-file output :parallel parallel))
             (assert (funcall (symbol-function 'imported-call) sentinel))
             (assert (not (funcall (symbol-function 'imported-call) (make-symbol "SENTINEL"))))
             (assert (funcall (symbol-function 'local-call)
                              (funcall (symbol-function 'local-value))))))
      (when (probe-file source) (delete-file source))
      (when (probe-file output) (delete-file output))))
  (format t "~&INLINE-LITERAL-IDENTITY-PASSED~%"))
(run)
