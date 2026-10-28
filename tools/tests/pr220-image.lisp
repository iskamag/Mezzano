;;;; Build a private SMP regression image from the repaired split stack.
;;;; Run from Mezzano/: sbcl --non-interactive --load tools/tests/pr220-image.lisp
;;;; MEZZANO_PR220_ARCH may be x86-64 (default) or arm64.
(require :asdf)
(push *default-pathname-defaults* asdf:*central-registry*)
(asdf:load-system :lispos)
(let ((architecture (or (uiop:getenv "MEZZANO_PR220_ARCH") "x86-64")))
  (cold-generator:set-up-cross-compiler
   :architecture (cond ((equal architecture "x86-64") :x86-64)
                       ((equal architecture "arm64") :arm64)
                       (t (error "Unknown architecture ~S" architecture)))))
(let* ((tests '("tools/tests/pr220-tlb-guest.lisp"
                "tools/tests/mcs-guest.lisp"
                "tools/tests/mutex-pi-guest.lisp"))
       (output (uiop:ensure-directory-pathname
                (or (uiop:getenv "MEZZANO_PR220_OUTPUT") "/tmp/mezzano-pr220-guest/")))
       (loader (merge-pathnames "run.lisp" output)))
  (dolist (test tests)
    (mezzano.compiler::load-for-cross-compiler test))
  (ensure-directories-exist loader)
  (with-open-file (out loader :direction :output :if-exists :supersede)
    (write-line "(mezzano.supervisor:debug-print-line \"PR220-GUEST-BEGIN\")" out)
    (write-line "(mezzano.pr220-tlb-test:run)" out)
    (write-line "(mezzano.tools.mcs-guest:run)" out)
    (write-line "(mezzano.mutex-pi-test:run)" out)
    (write-line "(mezzano.supervisor:debug-print-line \"PR220-GUEST-PASSED\")" out))
  (let ((cold-generator::*warm-source-files*
          (append (remove "ipl.lisp" cold-generator::*warm-source-files* :test #'equal)
                  tests (list (namestring loader)))))
    (cold-generator::make-image
     (merge-pathnames "native" output) :image-size (* 5 1024 1024 1024)
     :header-path "tools/disk-header.bin"))
  (format t "PR220-PRIVATE-IMAGE-BUILT~%"))
