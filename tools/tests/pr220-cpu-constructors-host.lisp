;;; Run with: sbcl --script tools/tests/pr220-cpu-constructors-host.lisp
;;; Compare constructor keywords in the real secondary CPU paths with the
;;; declared architecture and inherited CPU slots, without emulating Mezzano's
;;; custom DEFSTRUCT options on the host.

(defpackage :sys.int (:use :cl))
(defpackage :mezzano.supervisor (:use :cl))
(in-package :mezzano.supervisor)

(defun read-source-form (path prefix)
  (with-open-file (stream (merge-pathnames path *load-truename*))
    (loop for position = (file-position stream)
          for line = (read-line stream nil)
          while line
          when (search prefix line)
            do (file-position stream position)
               (return (read stream)))))

(defun structure-slots (form)
  (assert (eq (first form) 'defstruct))
  (mapcar (lambda (entry) (if (consp entry) (first entry) entry))
          (cddr form)))

(defun find-constructor (tree name)
  (cond ((atom tree) nil)
        ((eq (first tree) name) tree)
        (t (or (find-constructor (first tree) name)
               (find-constructor (rest tree) name)))))

(defun form-present-p (tree wanted)
  (cond ((equal tree wanted) t)
        ((atom tree) nil)
        (t (or (form-present-p (first tree) wanted)
               (form-present-p (rest tree) wanted)))))

(defun constructor-unknown-keywords (constructor slots)
  (let ((arguments (rest constructor)))
    (assert (evenp (length arguments)))
    (loop for (key nil) on arguments by #'cddr
          unless (and (keywordp key)
                      (find (symbol-name key) slots
                            :key #'symbol-name :test #'string-equal))
            collect key)))

(let ((base-slots (structure-slots
                   (read-source-form "../../supervisor/thread.lisp"
                                     "(defstruct (cpu"))))
  (dolist (spec '(("../../supervisor/x86-64/cpu.lisp"
                   "(defstruct (x86-64-cpu" "(defun register-secondary-cpu "
                   make-x86-64-cpu)
                  ("../../supervisor/arm64/cpu.lisp"
                   "(defstruct (arm64-cpu" "(defun register-secondary-cpu "
                   make-arm64-cpu)))
    (destructuring-bind (path struct-prefix function-prefix constructor-name) spec
      (let* ((slots (append base-slots
                            (structure-slots (read-source-form path struct-prefix))))
             (function (read-source-form path function-prefix))
             (constructor (find-constructor function constructor-name)))
        (assert constructor)
        (assert (null (constructor-unknown-keywords constructor slots))))))
  ;; The old split's removed mode slot must be diagnosed by the same check.
  (assert (equal (constructor-unknown-keywords
                  '(make-x86-64-cpu :apic-in-x2apic-mode t)
                  base-slots)
                 '(:apic-in-x2apic-mode)))
  (assert (form-present-p
           (read-source-form "../../supervisor/arm64/cpu.lisp"
                             "(defun initialize-boot-cpu ")
           '(setf *tlb-shootdown-in-progress* nil))))
(format t "PASS: CPU constructors match slots and ARM boot initializes TLB state.~%")
