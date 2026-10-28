;;;; From Mezzano/: sbcl --non-interactive --load tools/tests/cons-tlab-image.lisp
;;;; Private regression image, or an early serial REPL with MEZZANO_CONS_REPL=1.
;;;; Once booted, use the native compiler and wired LLF loading for iteration.
(require :asdf)
(push *default-pathname-defaults* asdf:*central-registry*)
(asdf:load-system :lispos)
(cold-generator:set-up-cross-compiler :architecture :x86-64)
(mezzano.compiler::load-for-cross-compiler "tools/tests/cons-tlab-guest.lisp")
(let* ((output (uiop:ensure-directory-pathname
                (or (uiop:getenv "MEZZANO_CONS_TEST_OUTPUT") "/tmp/mezzano-cons-tlab-test/")))
       (loader (merge-pathnames "run.lisp" output)))
  (ensure-directories-exist loader)
  (with-open-file (out loader :direction :output :if-exists :supersede)
    (cond ((uiop:getenv "MEZZANO_CONS_REPL")
           (write-line "(mezzano.file-system.remote:add-remote-file-host :remote mezzano.internals::*file-server-host-ip*)" out)
           (write-line "(mezzano.internals::debug-serial-repl-start)" out)
           (write-line "(loop (sleep 60))" out))
          (t (write-line "(mezzano.cons-tlab-test:run)" out))))
  (let ((cold-generator::*warm-source-files*
          (append (remove "ipl.lisp" cold-generator::*warm-source-files* :test #'equal)
                  (list "tools/tests/cons-tlab-guest.lisp" (namestring loader)))))
    (cold-generator::make-image
     (merge-pathnames "native" output) :image-size (* 5 1024 1024 1024)
     :header-path "tools/disk-header.bin"))
  (format t "CONS-TLAB-PRIVATE-IMAGE-BUILT~%"))
