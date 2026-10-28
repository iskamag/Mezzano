;;;; Hotload compiler/CLOS refinements after booting the physical-storage ABI.
;;;; This does not change structures or install the storage tag/backend primitives.
;;;; Temporary source/LLFs use the same directory as the compilation benchmark.
(defpackage :clos-fast-hotload (:use :cl))
(in-package :clos-fast-hotload)

(progn
(unless (and (= mezzano.internals::*llf-version* 38)
             (fboundp 'mezzano.internals::%instance-storage))
  (error "Boot a matching physical-storage cold image before hotloading."))
(defun clos-extract-definitions (source output package names &optional wired)
 (let ((*package* (find-package package)))
  (with-open-file (in source)
   (with-open-file (out output :direction :output :if-exists :supersede)
    (print `(in-package ,package) out)
    (labels ((emit (form)
               (when (consp form)
                 (cond ((eq (car form) 'eval-when)
                        (mapc #'emit (cddr form)))
                       ((or (eq (car form) 'declaim)
                            (and (member (car form) '(defun defmacro))
                                 (or (eq names t)
                                     (member (if (consp (second form))
                                                 (second (second form))
                                                 (second form)) names))))
                        (print form out))))))
      (loop for form = (read in nil :eof) until (eq form :eof)
            do (emit form)))))
  (multiple-value-bind (llf warnings failure)
    (compile-file output :verbose nil :print nil :parallel nil)
   (declare (ignore warnings)) (assert (not failure)) (load llf :wired wired))))
(clos-extract-definitions "SYS:SOURCE;COMPILER;SIMPLIFY.LISP" "REMOTE:/tmp/clos-cost/simplify-hotload.lisp" :mezzano.compiler
 '(mezzano.compiler::struct-type-test-form mezzano.compiler::simplify-struct-slot
   mezzano.compiler::simplify-setf-struct-slot mezzano.compiler::simplify-cas-struct-slot
   mezzano.compiler::simplify-atomic-fixnum-op-struct-slot mezzano.compiler::simplify-atomic-swap-struct-slot
   mezzano.compiler::simplify-dcas-struct-slot mezzano.compiler::simplify-struct-vector-slot
   mezzano.compiler::simplify-setf-struct-vector-slot mezzano.compiler::simplify-cas-struct-vector-slot))
(clos-extract-definitions "SYS:SOURCE;RUNTIME;INSTANCE.LISP" "REMOTE:/tmp/clos-cost/location-hotload.lisp" :mezzano.runtime '(mezzano.runtime::location-offset-t))
(clos-extract-definitions "SYS:SOURCE;SYSTEM;ARRAY.LISP" "REMOTE:/tmp/clos-cost/array-hotload.lisp" :mezzano.internals '(mezzano.internals::aref-1))
(let ((*package* (find-package :mezzano.compiler)))
  (with-open-file (in "SYS:SOURCE;COMPILER;TRANSFORMS.LISP")
    (loop for form = (read in nil :eof) until (eq form :eof)
          when (and (consp form)
                    (eq (car form) 'mezzano.compiler::define-transform)
                    (eq (second form) 'mezzano.runtime::right-shift)
                    (equal (third form) '((integer fixnum) (count (and fixnum (integer 0))))))
            do (eval form))))
(clos-extract-definitions "SYS:SOURCE;SYSTEM;GC.LISP" "REMOTE:/tmp/clos-cost/weak-hotload.lisp" :mezzano.internals '(mezzano.internals::examine-weak-pointer-key) t)
(clos-extract-definitions "SYS:SOURCE;SYSTEM;CLOS;FAST-CLASS-HASH-TABLE.LISP" "REMOTE:/tmp/clos-cost/cache-hotload.lisp" :mezzano.clos t)
(clos-extract-definitions "SYS:SOURCE;SYSTEM;CLOS;SINGLE-DISPATCH-EMF-TABLE.LISP" "REMOTE:/tmp/clos-cost/dispatch-hotload.lisp" :mezzano.clos t)
(mezzano.internals::defglobal mezzano.clos::*standard-generic-function-emf-table-location*)
(setf mezzano.clos::*standard-generic-function-emf-table-location*
      (mezzano.clos::slot-location-using-layout
       'mezzano.clos::classes-to-emf-table mezzano.clos::*the-layout-standard-generic-function*))
(clos-extract-definitions "SYS:SOURCE;SYSTEM;CLOS;CLOSETTE.LISP" "REMOTE:/tmp/clos-cost/clos-hotload.lisp" :mezzano.clos
 '(mezzano.clos::standard-instance-access mezzano.clos::with-current-instance-storage
   mezzano.clos::classes-to-emf-table mezzano.clos::safe-class-precedence-list
   class-of mezzano.clos::accessor-cache-cell mezzano.clos::write-cached-slot mezzano.clos::read-cached-slot
   mezzano.clos::make-slot-value-reader mezzano.clos::make-slot-value-writer
   mezzano.clos::slot-value-cache-cell
   mezzano.clos::read-accessor-entry mezzano.clos::write-accessor-entry
   mezzano.clos::compute-reader-discriminator mezzano.clos::compute-writer-discriminator
   mezzano.clos::compute-1-effective-discriminator mezzano.clos::slow-single-dispatch-method-lookup*
   mezzano.clos::slow-single-dispatch-method-lookup mezzano.clos::fetch-up-to-date-instance-slots-and-layout))
(let ((*package* (find-package :mezzano.clos)))
  (with-open-file (in "SYS:SOURCE;SYSTEM;CLOS;CLOSETTE.LISP")
    (loop for form = (read in nil :eof) until (eq form :eof)
          when (and (consp form) (eq (car form) 'define-compiler-macro)
                    (member (second form) '(slot-value (setf slot-value)) :test #'equal))
            do (eval form))))
;; Existing compiled callers keep their old load-time constants until recompiled.
;; Refresh the linked-list helpers used heavily by the compilation benchmark.
(clos-extract-definitions "SYS:SOURCE;COMPILER;BACKEND;BACKEND.LISP" "REMOTE:/tmp/clos-cost/backend-links-hotload.lisp" :mezzano.compiler.backend
 '(mezzano.compiler.backend::first-instruction mezzano.compiler.backend::last-instruction
   mezzano.compiler.backend::next-instruction mezzano.compiler.backend::prev-instruction
   mezzano.compiler.backend::insert-before mezzano.compiler.backend::insert-after
   mezzano.compiler.backend::append-instruction mezzano.compiler.backend::remove-instruction))
(let ((seen (make-hash-table :test 'eq)) (gfs nil))
 (do-all-symbols (symbol)
  (dolist (name (list symbol (list 'setf symbol)))
   (when (fboundp name)
    (let ((fn (fdefinition name)))
     (when (and (mezzano.clos::standard-generic-function-instance-p fn) (not (gethash fn seen)))
      (setf (gethash fn seen) t) (push fn gfs))))))
 (dolist (table (list mezzano.clos::*fast-slot-value-readers*
                     mezzano.clos::*fast-slot-value-writers*))
   (maphash (lambda (name gf)
              (declare (ignore name))
              (unless (gethash gf seen)
                (setf (gethash gf seen) t)
                (push gf gfs))) table))
 (dolist (gf gfs)
  (mezzano.clos::reset-gf-emf-table gf)
  (mezzano.clos:set-funcallable-instance-function gf (mezzano.clos::std-compute-discriminating-function gf)))
 (format t "Refreshed ~D generic functions.~%" (length gfs)))

)
