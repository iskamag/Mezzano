;;;; Compile the actual thread owner with the native x86-64 compiler. This
;;;; checks the new entry's emitted code, not native execution or residency.
;;;; MEZZANO_TEST_DEPS names the directory containing the six ASDF dependencies.
(require :asdf)
(defparameter *world-stop-source*
  (uiop:ensure-directory-pathname
   (merge-pathnames "../../" (uiop:pathname-directory-pathname *load-truename*))))
(defparameter *world-stop-deps*
  (uiop:ensure-directory-pathname
   (or (uiop:getenv "MEZZANO_TEST_DEPS") (error "Set MEZZANO_TEST_DEPS."))))
(defparameter *world-stop-output*
  (uiop:ensure-directory-pathname
   (or (uiop:getenv "MEZZANO_TEST_OUTPUT") "/tmp/mezzano-world-stop-cross/")))
(asdf:initialize-source-registry
 `(:source-registry (:directory ,*world-stop-source*)
   ,@(loop for (name variable) in '(("nibbles/" "MEZZANO_TEST_NIBBLES")
                                   ("cl-ppcre/" nil)
                                   ("iterate-1.4.3/" "MEZZANO_TEST_ITERATE")
                                   ("alexandria/" nil) ("closer-mop/" nil)
                                   ("trivial-gray-streams/" nil))
           for override = (and variable (uiop:getenv variable))
           collect `(:directory ,(if override (uiop:ensure-directory-pathname override)
                                    (merge-pathnames name *world-stop-deps*))))
   :ignore-inherited-configuration))
(asdf:initialize-output-translations
 ;; Preserve source directories: flattening to a cache mixes distinct
 ;; dependencies' PACKAGE.FASL files across repeated compiler invocations.
 `(:output-translations ("/" ,(namestring (merge-pathnames "asdf/" *world-stop-output*)))
   :ignore-inherited-configuration))
(asdf:load-system :lispos)
(let ((*default-pathname-defaults* *world-stop-source*))
  (cold-generator:set-up-cross-compiler :architecture :x86-64))
(defparameter *world-stop-final-form* nil)
(let ((original (fdefinition 'mezzano.compiler::codegen-lambda))
      (mezzano.compiler::*target-architecture* :x86-64)
      (mezzano.compiler::*perform-tce* nil)
      (cross-cl:*features* (cons :x86-64 cross-cl:*features*))
      (output (merge-pathnames "thread.llf" *world-stop-output*)))
  (ensure-directories-exist output)
  (unwind-protect
       (progn
         (setf (fdefinition 'mezzano.compiler::codegen-lambda)
               (lambda (information &optional architecture)
                 (when (eq (mezzano.compiler::lambda-information-name information)
                           'mezzano.supervisor:call-with-world-stopped-or-panic)
                   (setf *world-stop-final-form*
                         (mezzano.compiler::unparse-compiler-form information)))
                 (funcall original information architecture)))
         (mezzano.compiler::cross-compile-file
          (merge-pathnames "supervisor/thread.lisp" *world-stop-source*)
          :output-file output :verbose t :print nil))
    (setf (fdefinition 'mezzano.compiler::codegen-lambda) original))
  (with-open-file (in output :element-type '(unsigned-byte 8))
    (mezzano.cold-generator.load:validate-llf-header in :x86-64)))
(assert *world-stop-final-form*)
(defun world-stop-heap-environments (form)
  ;; Inspect the actual final body entering codegen, including nested lambdas,
  ;; not a printed shape or an earlier lowering stage. Callees and the caller's
  ;; worker still need their own admission and allocation evidence.
  (labels ((walk (node)
             (when (consp node)
               (unless (and (symbolp (first node)) (string= "QUOTE" (symbol-name (first node))))
                 (append (when (and (symbolp (first node))
                                    (member (symbol-name (first node))
                                            '("MAKE-SIMPLE-VECTOR" "MAKE-CLOSURE") :test #'string=))
                           (list (first node)))
                         (walk (car node)) (walk (cdr node)))))))
    (walk form)))
(assert (null (world-stop-heap-environments *world-stop-final-form*)))
;; Compiler variable/tag/layout objects in the unparsed form are intentionally
;; diagnostic objects, not a readable serialized or executable source artifact.
(with-open-file (out (merge-pathnames "entry-final.txt" *world-stop-output*)
                     :direction :output :if-exists :supersede)
  (let ((*print-readably* nil) (*print-circle* t) (*print-length* nil) (*print-level* nil))
    (write *world-stop-final-form* :stream out) (terpri out)))
(format t "WORLD-STOP NATIVE CROSS-COMPILE PASSED: TCE=NIL; no emitted heap environments (no execution claim).~%")
