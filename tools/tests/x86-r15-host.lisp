;;;; From the Mezzano source directory:
;;;; sbcl --non-interactive --load tools/tests/x86-r15-host.lisp
(require :asdf)
(push *default-pathname-defaults* asdf:*central-registry*)
(asdf:load-system :lispos)
(cold-generator:set-up-cross-compiler :architecture :x86-64)

(defvar *r15-test-lap*)
(defmethod mezzano.compiler.backend:perform-target-lap-generation :around
    (function debug-map spills layout (target mezzano.compiler:x86-64-target))
  (multiple-value-bind (lap environment) (call-next-method)
    (push lap *r15-test-lap*)
    (values lap environment)))

(defun r15-check-generated-function (lap)
  (let* ((save (find-if (lambda (inst)
                          (and (consp inst)
                               (eq (first inst) 'mezzano.lap.x86:mov64)
                               (consp (second inst))
                               (eq (first (second inst)) :stack)
                               (eq (third inst) :r15)))
                        lap))
         (slot (and save (second (second save)))))
    (when save
      ;; The saved caller register must be rooted and available to the debugger.
      (assert (find (list :r15 slot :value :hidden t)
                    (second (find :debug lap :key (lambda (inst) (and (consp inst) (car inst)))))
                    :test #'equal))
      (assert (find-if (lambda (inst)
                        (and (consp inst) (eq (car inst) :gc)
                             (second (member :layout (cdr inst)))
                             (< slot (length (second (member :layout (cdr inst)))))
                             (= 1 (bit (second (member :layout (cdr inst))) slot))))
                      lap))
      ;; Every normal epilogue, including both tail-call forms, restores it.
      (loop for tail on lap
            when (and (consp (car tail))
                      (eq (caar tail) 'mezzano.lap.x86:leave))
              do (let ((index (- (length lap) (length tail))))
                   ;; Argument-error tail exits occur before R15 is touched.
                   (unless (and (consp (third tail))
                                (eq (car (third tail)) 'mezzano.lap.x86:jmp)
                                (equal (second (third tail))
                                       '(:named-call mezzano.internals::raise-invalid-argument-error)))
                     (assert (member (list 'mezzano.lap.x86:mov64 :r15 (list :stack slot))
                                     (subseq lap (max 0 (- index 3)) index)
                                     :test #'equal))))))
    (not (null save))))

(let ((target (make-instance 'mezzano.compiler:x86-64-target))
      (call (make-instance 'mezzano.compiler.backend:call-instruction)))
  (assert (member :r15
                  (mezzano.compiler.backend.register-allocator:valid-physical-registers-for-kind :value target)))
  (assert (not (member :r14
                       (mezzano.compiler.backend.register-allocator:valid-physical-registers-for-kind :value target))))
  (setf (mezzano.compiler.backend::call-involved-in-nlx call) nil)
  (assert (not (member :r15
                       (mezzano.compiler.backend.register-allocator:instruction-clobbers call target))))
  (setf (mezzano.compiler.backend::call-involved-in-nlx call) t)
  (assert (member :r15
                  (mezzano.compiler.backend.register-allocator:instruction-clobbers call target))))

(let ((*r15-test-lap* nil))
  (dolist (mezzano.compiler::*perform-tce* '(nil t))
    (dolist (form
           '((lambda (x) (list x (car x)))
             (lambda (a b c d) (unknown-function (car a) a b c d))
             (lambda (a b c d e f) (unknown-function (car a) a b c d e f))
             (lambda (fn a b c d) (funcall fn (car a) a b c d))
             (lambda (fn a b c d e f) (funcall fn (car a) a b c d e f))
             (lambda (x) (values x (car x) (cdr x)))
             (lambda (a b c d e f g) (values a b c d e f g))
             (lambda (a b c d e &optional f g) (list a b c d e f g))
             (lambda (a b c d e &rest rest) (list a b c d e rest))
             (lambda (a b c d e &optional (f :default fp) (g :default gp))
               (list a b c d e f fp g gp))
             (lambda (a b c d e f &key (x :default xp) (y :default yp))
               (list a b c d e f x xp y yp))
             (lambda (fn) (multiple-value-bind (a b c d e f g) (funcall fn)
                            (list a b c d e f g)))
             (lambda (x) (block done (funcall (lambda () (return-from done x)))))))
      (mezzano.compiler::compile-lambda form)))
  (assert (plusp (count-if #'r15-check-generated-function *r15-test-lap*)))
  (format t "R15 HOST PASS: ~D generated functions checked.~%" (length *r15-test-lap*)))
