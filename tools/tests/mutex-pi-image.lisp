;;;; From Mezzano/: sbcl --non-interactive --load tools/tests/mutex-pi-image.lisp
;;;; Private x86-64 image with RUN replacing the normal IPL.
(require :asdf)
(push *default-pathname-defaults* asdf:*central-registry*)
(asdf:load-system :lispos)
(cold-generator:set-up-cross-compiler :architecture :x86-64)
(mezzano.compiler::load-for-cross-compiler "tools/tests/mutex-pi-guest.lisp")
(let* ((output (uiop:ensure-directory-pathname
                (or (uiop:getenv "MEZZANO_MUTEX_PI_OUTPUT") "/tmp/mezzano-mutex-pi-test/")))
       (loader (merge-pathnames "run.lisp" output)))
  (ensure-directories-exist loader)
  (with-open-file (out loader :direction :output :if-exists :supersede)
    (write-line "(mezzano.mutex-pi-test:run)" out))
  (let ((cold-generator::*warm-source-files*
          (append (remove "ipl.lisp" cold-generator::*warm-source-files* :test #'equal)
                  (list "tools/tests/mutex-pi-guest.lisp" (namestring loader)))))
    (cold-generator::make-image
     (merge-pathnames "native" output) :image-size (* 5 1024 1024 1024)
     :header-path "tools/disk-header.bin"))
  (format t "MUTEX-PI-PRIVATE-IMAGE-BUILT~%"))
