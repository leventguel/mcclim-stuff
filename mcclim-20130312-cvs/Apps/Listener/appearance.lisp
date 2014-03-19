

(in-package :clim-listener)

;;; Apropos

(defparameter *apropos-symbol-unbound-family* :fix)
(defparameter *apropos-symbol-unbound-face*   :bold)
(defparameter *apropos-symbol-bound-family*   :fix)
(defparameter *apropos-symbol-bound-face*     :bold)

;;; Show Class Slots

(defparameter *slot-name-ink*     +slate-blue+)
(defparameter *slot-type-ink*     +green+)
(defparameter *slot-initargs-ink* +red+)
(defparameter *slot-initform-ink* +goldenrod3+)
(defparameter *slot-readers-ink*  +wheat3+)
(defparameter *slot-writers-ink*  +wheat2+)
(defparameter *slot-documentation-ink* +turquoise4+)

;;; Graphing (classes and packages)

(defparameter *graph-edge-ink* (make-rgb-color 0.72 0.72 0.72))
(defparameter *graph-text-style* (make-text-style :fix :roman :very-large))



