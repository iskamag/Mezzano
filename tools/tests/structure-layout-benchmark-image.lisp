;;;; From Mezzano/: sbcl --non-interactive --load tools/tests/structure-layout-benchmark-image.lisp
;;;; The private image runs the regression and paired benchmark at cold boot.
;;;; Set MEZZANO_LAYOUT_BENCH_OUTPUT to choose the output directory.
(require :asdf)
(push *default-pathname-defaults* asdf:*central-registry*)
(asdf:load-system :lispos)
(cold-generator:set-up-cross-compiler :architecture :x86-64)
(dolist (file '("tools/tests/structure-compatible-layouts-guest.lisp"
                "tools/tests/structure-layout-benchmark.lisp"))
  (mezzano.compiler::load-for-cross-compiler file))
(let* ((output (uiop:ensure-directory-pathname
                (or (uiop:getenv "MEZZANO_LAYOUT_BENCH_OUTPUT")
                    "/tmp/mezzano-layout-benchmark/")))
       (loader (merge-pathnames "run.lisp" output)))
  (ensure-directories-exist loader)
  (with-open-file (out loader :direction :output :if-exists :supersede)
    (write-line "(mezzano.supervisor:debug-print-line \"STRUCTURE-LAYOUT-REGRESSION-BEGIN\")" out)
    (write-line "(mezzano.structure-layout-test:run)" out)
    (write-line "(mezzano.supervisor:debug-print-line \"STRUCTURE-LAYOUT-REGRESSION-PASSED\")" out)
    (write-line "(mezzano.structure-layout-benchmark:run :repeats 10000 :pairs 11)" out)
    (write-line "(mezzano.supervisor:debug-print-line \"STRUCTURE-LAYOUT-ALL-PASSED\")" out))
  (let ((cold-generator::*supervisor-source-files*
          (append cold-generator::*supervisor-source-files*
                  (list "tools/tests/structure-layout-benchmark-baseline.lisp")))
        (cold-generator::*warm-source-files*
          (append (remove "ipl.lisp" cold-generator::*warm-source-files* :test #'equal)
                  (list "tools/tests/structure-compatible-layouts-guest.lisp"
                        "tools/tests/structure-layout-benchmark.lisp"
                        (namestring loader)))))
    (cold-generator::make-image
     (merge-pathnames "native" output) :image-size (* 5 1024 1024 1024)
     :header-path "tools/disk-header.bin"))
  (format t "STRUCTURE-LAYOUT-BENCHMARK-IMAGE-BUILT~%"))
