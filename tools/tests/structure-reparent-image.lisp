;;;; From Mezzano/: sbcl --non-interactive --load tools/tests/structure-reparent-image.lisp
;;;; Set MEZZANO_REPARENT_TEST_OUTPUT to choose a private output directory.
(require :asdf)
(push *default-pathname-defaults* asdf:*central-registry*)
(asdf:load-system :lispos)
(cold-generator:set-up-cross-compiler :architecture :x86-64)
(dolist (file '("tools/tests/structure-reparent-cold-classes.lisp"
                "tools/tests/structure-compatible-layouts-guest.lisp"
                "tools/tests/structure-layout-benchmark.lisp"
                "tools/tests/structure-reparent-guest.lisp"
                "tools/tests/structure-reparent-benchmark.lisp"
                "tools/tests/structure-migration-benchmark.lisp"))
  (mezzano.compiler::load-for-cross-compiler file))
(let* ((output (uiop:ensure-directory-pathname
                (or (uiop:getenv "MEZZANO_REPARENT_TEST_OUTPUT")
                    "/tmp/mezzano-structure-reparent/")))
       (loader (merge-pathnames "run.lisp" output)))
  (ensure-directories-exist loader)
  (with-open-file (out loader :direction :output :if-exists :supersede)
    (write-line "(mezzano.supervisor:debug-print-line \"STRUCTURE-REPARENT-TEST-BEGIN\")" out)
    (write-line "(mezzano.structure-layout-test:run)" out)
    (write-line "(mezzano.structure-reparent-test:run)" out)
    (write-line "(mezzano.structure-reparent-test::run-cold)" out)
    (write-line "(mezzano.structure-reparent-benchmark:run)" out)
    (write-line "(mezzano.structure-migration-benchmark:run :count 100000)" out)
    (write-line "(mezzano.structure-migration-benchmark::run-boa)" out)
    (write-line "(mezzano.structure-migration-benchmark::run-guard-overhead)" out)
    (write-line "(mezzano.structure-migration-benchmark::run-redefinition)" out)
    (write-line "(mezzano.structure-migration-benchmark::run-metadata)" out)
    (write-line "(mezzano.structure-layout-benchmark:run :repeats 40000 :accessor-repeats 10000 :pairs 11)" out)
    (write-line "(mezzano.supervisor:debug-print-line \"STRUCTURE-REPARENT-ALL-PASSED\")" out))
  (let ((cold-generator::*supervisor-source-files*
          (append cold-generator::*supervisor-source-files*
                  (list "tools/tests/structure-layout-benchmark-baseline.lisp")))
        (cold-generator::*source-files*
          (append cold-generator::*source-files*
                  (list "tools/tests/structure-reparent-cold-classes.lisp")))
        (cold-generator::*warm-source-files*
          (append (remove "ipl.lisp" cold-generator::*warm-source-files* :test #'equal)
                  (list "tools/tests/structure-compatible-layouts-guest.lisp"
                        "tools/tests/structure-layout-benchmark.lisp"
                        "tools/tests/structure-reparent-guest.lisp"
                        "tools/tests/structure-reparent-benchmark.lisp"
                        "tools/tests/structure-migration-benchmark.lisp"
                        (namestring loader)))))
    (cold-generator::make-image
     (merge-pathnames "native" output) :image-size (* 5 1024 1024 1024)
     :header-path "tools/disk-header.bin"))
  (format t "STRUCTURE-REPARENT-IMAGE-BUILT~%"))
