;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-DEMO; Base: 10; Lowercase: Yes -*-

;;;  (c) copyright 2000 by 
;;;           Iban Hatchondo (hatchond@emi.u-bordeaux.fr)
;;;           Julien Boninfante (boninfan@emi.u-bordeaux.fr)

;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Library General Public License for more details.
;;;
;;; You should have received a copy of the GNU Library General Public
;;; License along with this library; if not, write to the 
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, 
;;; Boston, MA  02111-1307  USA.

(in-package :clim-demo)

;; Gadget Test/Demo

;; To run the gadget test:   (clim-demo:gadget-test)

;; McCLIM contains an alternate look and feel entitled "pixie" which is
;; not the default. It can by used by creating your application using an
;; alternate frame manager, clim-internals::pixie/clx-look.

;; To run the gadget test using the pixie frame manager:
;; (gadget-test 'clim-internals::pixie/clx-look)
;; This may require you to load the clim-looks system.

(defun gadget-test (&optional frame-manager-name)
  (run-frame-top-level
   (if frame-manager-name 
       (make-application-frame 'gadget-test
                               :frame-manager (make-instance frame-manager-name
                                                             :port (find-port)))
       (make-application-frame 'gadget-test))))

(export 'gadget-test)

(defun run-pixie-test (name)
  (when name
    (run-frame-top-level
     (make-application-frame name
                             :frame-manager (make-instance 'clim-internals::pixie/clx-look
                                                           :port (find-port))))))

(defmethod gadget-test-frame-top-level ((frame application-frame)
				       &key (command-parser 'command-line-command-parser)
				       (command-unparser 'command-line-command-unparser)
				       (partial-command-parser
					'command-line-read-remaining-arguments-for-partial-command)
				       (prompt "Command: "))
  (declare (ignore command-parser command-unparser partial-command-parser prompt))
  (catch 'exit
    (clim-extensions:simple-event-loop))
  (frame-exit frame))

(make-command-table 'lisp-menu
                    :errorp nil
                    :menu '(("Heir" :menu lisp-sub-menu)
                            ("Lisp" :command test)
                            ("Lisp" :command test)))

(make-command-table 'lisp-sub-menu
                    :errorp nil
                    :menu '(("Does"  :command test)
                            ("This"  :command test)
                            ("Work?" :command test)))

(make-command-table 'edit-menu
                    :errorp nil
                    :menu '(("Edit" :command test)
                            ("Edit" :command test)
                            ("Edit" :command test)))

(make-command-table 'view-menu
                    :errorp nil
                    :menu '(("View" :command test)
                            ("View" :command test)
                            ("View" :command test)))

(make-command-table 'search-menu
                    :errorp nil
                    :menu '(("Search" :command test)
                            ("Search" :command test)
                            ("Search" :command test)))

(define-command test ()
  (format *error-output* "That was just a test~%")
  (finish-output *error-output*))

(macrolet ((make-pane-constructor (class-name)
             `(defmacro ,class-name (&rest options)
                `(make-pane ',',class-name ,@options))))
  (make-pane-constructor text-field)
  (make-pane-constructor text-edit)
  (make-pane-constructor slider)
  (make-pane-constructor push-button)
  (make-pane-constructor toggle-button))

(define-application-frame gadget-test ()
    ()
    (:menu-bar
     (("Lisp"   :menu lisp-menu)
      ("Edit"   :menu edit-menu)
      ("View"   :menu view-menu)
      ("Search" :menu search-menu)))
    (:panes
    (raised     (raising (:border-width 3 :background +Gray83+)
                  (make-pane 'check-box :choices '("First" "Second" "Third"))))
     (tf1        :push-button
                 :text-style (make-text-style :fix :roman 24)
                 :label "Text Field")
     (tf2        :push-button
                 :text-style (make-text-style :serif :roman 24)
                 :label "Text Field")
     (tf3        :push-button
                 :text-style (make-text-style :serif :italic 24)
                 :label "Text Field")
     (tf4        :push-button
                 :text-style (make-text-style :sans-serif '(:bold :italic) 24)
                 :label "Text Field")
    (text-edit  :text-editor
                :value "Text Editor")
     (slider-h   :slider
                 :min-value 0
                 :max-value 100
                 :value 0
                 :show-value-p t
                 :orientation :horizontal
                 :current-color +black+)
     (slider-v   :slider
                 :min-value 0
                 :max-value 100
                 :orientation :vertical
                 :current-color +black+
                 :value 0)
     (slider-v1  :slider
                 :min-value 0
                 :max-value 100
                 :orientation :vertical
                 :current-color +black+
                 :value 0)
     (slider-v2  :slider
                 :min-value 0
                 :max-value 100
                 :orientation :vertical
                 :current-color +black+
                 :value 0)
     (slider-v3  :slider
                 :min-value 0
                 :max-value 100
                 :orientation :vertical
                 :current-color +black+
                 :value 0)
     (radar      (make-pane 'radar-pane :name 'radar))
     (push-btn   (lowering (:border-width 3 :background +Gray83+)
                   (horizontally ()
                     (push-button
                       :name  "Radiate"
                       :label "Radiate"
                       :activate-callback
                         (lambda (pane &rest args)
                             nil))
                     (push-button
                       :label "No, Push Me")
                     (push-button
                       :label "Me!"))))
     (table (lowering (:border-width 3 :background +Gray83+)
              (tabling (:height 50)
                (list (push-button :label "A") (push-button :label "B"))
                (list (push-button :label "C") (push-button :label "D"))
                (list (push-button :label "E") (push-button :label "F")))))
     (toggle-btn :toggle-button
                 :label "Toggle"
                 :value t
                 :normal +red+
                 :highlighted +red+
                 :pushed-and-highlighted +red+)
     (scroll    (raising (:border-width 1 :background +Gray83+)
                   (scrolling (:background +Gray83+ :width 100 :height 100)
                     (horizontally ()
                       (vertically ()
                         (push-button :label "This is a button")
                         (push-button :label "That is a button")
                         (push-button :label "This is a button too"))
                       (with-radio-box (:orientation :vertical)
                         (clim:radio-box-current-selection "First")
                         "Second" "Third"
                         "Red" "Blue" "Orange"
                         "Elephant" "Dog" "Cat")
                       (with-radio-box (:orientation :vertical :type :some-of)
                         (clim:radio-box-current-selection "Fourth") "Fifth" "Sixth")
                       (with-radio-box (:orientation :vertical)
                         (clim:radio-box-current-selection "Seventh") "Eighth" "Ninth")
                       (with-radio-box (:orientation :vertical :type :some-of)
                         (clim:radio-box-current-selection "Tenth") "Eleventh" "Twelth")))))
     (radio-box  (with-radio-box (:orientation :horizontal)
                   (clim:radio-box-current-selection "One") "Two" "Three"))
     (check-box  (with-radio-box (:type :some-of :orientation :horizontal)
                   (clim:radio-box-current-selection "First") "Second" "Third")))
    (:layouts
     (default
       (raising (:border-width 5 :background +Gray83+)
         (horizontally ()
	   (vertically ()
	     (horizontally ()
	       (horizontally ()
		 (vertically ()
		   slider-v
		   slider-v2)
		 slider-v3)
	       (vertically ()
		 tf1 tf2 tf3 tf4
		 slider-h))
	     ;; FIXME: the radar doesn't seem to do anything except take
	     ;; up vast amounts of space.
	     #+(or) radar
	     text-edit)
	   (vertically ()
	     push-btn
	     table
	     toggle-btn
	     scroll
	     radio-box
	     check-box)))))
    (:top-level (gadget-test-frame-top-level . nil)))

(defmethod run-frame-top-level :around ((frame gadget-test) &key &allow-other-keys)
  ;; FIXME: Timer events appear to have rotted.
  ;; Also, the following won't work because the frame has not really been realized yet,
  ;; so you can't get at its panes. Yet it has worked, and recently. Odd.
  ;; (clim-internals::schedule-timer-event (find-pane-named frame 'radar) 'radiate 0.1)
  (call-next-method))

(defclass radar-pane (basic-gadget) (
  (points
    :initform '((0.01 0.01 0.10 0.10)
                (0.10 0.02 0.70 0.40)
                (0.20 0.03 0.60 0.30)
                (0.20 0.04 0.20 0.50)
                (0.20 0.05 0.60 0.20)
                (0.20 0.06 0.30 0.40)
                (0.20 0.07 0.60 0.90)
                (0.20 0.08 0.80 0.30)
                (0.20 0.09 0.60 0.20)))))

(defmethod handle-event ((pane radar-pane) (event timer-event))
  (with-slots (points) pane
    (with-bounding-rectangle* (x1 y1 x2 y2) (sheet-region pane)
      (let ((xf (- x2 x1))
            (yf (- y2 y1)))
        (dolist (point points)
          (destructuring-bind (radius grow x y) point
            (let ((old-radius radius))
              (setf radius
                    (if (< radius 0.3)
                        (+ radius grow)
                        (progn
                          (setf (second point) (random 0.10))
                          (setf (third point)  (random 1.0))
                          (setf (fourth point) (random 1.0))
                          0.01)))
              (setf (first point) radius)
              ; v- fix with a transform?
              (let ((x (+ x1 (* x xf)))
                    (y (+ y1 (* y yf)))
                    (rx (* radius xf))
                    (ry (* radius yf))
                    (orx (* old-radius xf))
                    (ory (* old-radius yf)))
                (when (> radius 0.01)
                  (draw-ellipse* pane x y
                                      0 ry
                                      rx 0
                                      :ink +black+ :filled nil))
                (draw-ellipse* pane x y
                                    0 ory
                                    orx 0
                                    :ink +white+ :filled nil))))))))
  (clim-internals::schedule-timer-event pane 'radiate 0.1))

#-sbcl
(defun common-lisp-user::lg ()
  ; convenience, because I'm lazy
  (with-open-file (file "Examples/grammar.lisp" :external-format :euc-kr)
    (load file))
  (run-pixie-test 'grammar))
