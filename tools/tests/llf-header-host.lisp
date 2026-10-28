;;;; Run from the Mezzano source directory:
;;;; sbcl --non-interactive --load tools/tests/llf-header-host.lisp
;;;; Exercise the actual runtime header reader using host binary streams.
(require :asdf)
(push *default-pathname-defaults* asdf:*central-registry*)
(asdf:load-system :lispos)

(let ((*package* (find-package :mezzano.internals)))
  (with-open-file (source "system/load.lisp")
    (loop for form = (read source nil nil)
          while form
          when (and (consp form) (eq (first form) 'defun)
                    (member (symbol-name (second form))
                            '("LLF-ARCHITECTURE-NAME" "CHECK-LLF-HEADER"
                              "LLF-HEADER-COMPATIBLE-P" "LOAD-INTEGER")
                            :test #'equal))
            do (eval form))))
(setf (symbol-function 'mezzano.internals::%read-byte) #'read-byte
      (symbol-function 'mezzano.internals::current-architecture)
      (lambda () :x86-64))

(let ((path (merge-pathnames
             (format nil "mezzano-llf-header-~D.bin" (get-universal-time))
             #p"/tmp/")))
  (unwind-protect
       (dolist (case `((t #x4c #x4c #x46 1 ,mezzano.internals::*llf-version* 1)
                      (nil #x4c #x4c #x46 1 36 1)
                      (nil #x4c #x4c #x46 1 ,mezzano.internals::*llf-version* 2)
                      (nil #x4c #x4c #x46 1 ,mezzano.internals::*llf-version*)
                      (nil #x4c #x4c #x46 1)
                      (nil #x4c #x4c)
                      (nil #x4c #x4c #x46 2 ,mezzano.internals::*llf-version* 1)
                      (nil #x4c #x4c #x46 1 #xff #x40 1)))
         (with-open-file (stream path :direction :output :if-exists :supersede
                                :element-type '(unsigned-byte 8))
           (dolist (byte (rest case)) (write-byte byte stream)))
         (with-open-file (stream path :element-type '(unsigned-byte 8))
           (assert (eql (first case)
                        (mezzano.internals::llf-header-compatible-p stream)))))
    (when (probe-file path) (delete-file path))))
(format t "LLF HEADER HOST PASS: 8 compatibility fixtures checked.~%")
