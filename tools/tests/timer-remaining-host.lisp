;;;; Run with sbcl --script tools/tests/timer-remaining-host.lisp.
(defpackage :timer-remaining-test
  (:use :cl)
  (:shadow #:get-internal-run-time #:internal-time-units-per-second))
(in-package :timer-remaining-test)

(defconstant internal-time-units-per-second 1000)
(defvar *now* 10000)
(defun get-internal-run-time () *now*)

(let* ((path (merge-pathnames "../../supervisor/time.lisp" *load-truename*))
       (source (with-open-file (in path)
                 (let ((text (make-string (file-length in))))
                   (read-sequence text in)
                   text)))
       (offset (search "(defun convert-deadline-to-remaining-time" source)))
  (assert offset)
  (eval (read-from-string source nil nil :start offset)))

(assert (= (convert-deadline-to-remaining-time 10500) 1/2))
(assert (= (convert-deadline-to-remaining-time 70000) 60))
(assert (= (convert-deadline-to-remaining-time 10000) 0))
(assert (= (convert-deadline-to-remaining-time 8000) 0))
(format t "Timer remaining host regressions: 4 checks passed.~%")
