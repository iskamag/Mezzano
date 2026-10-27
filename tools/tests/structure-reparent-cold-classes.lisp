;;;; Included among the cold sources of the private regression image only.
(in-package :mezzano.internals)

(defstruct reparent-cold-old (shared 11))
(defstruct reparent-cold-new (shared 11))
(defstruct (reparent-cold-middle (:include reparent-cold-old)) marker)
(defstruct (reparent-cold-leaf (:include reparent-cold-middle)) leaf-value)
(defstruct reparent-cold-different fresh)
(defstruct (reparent-cold-single (:include reparent-cold-old)) marker)

;; Keep compiled references to the original class layout after redefinition.
(defun reparent-cold-single-original-p (object)
  (typep object 'reparent-cold-single))
(defun reparent-cold-single-original-marker (object)
  (reparent-cold-single-marker object))
