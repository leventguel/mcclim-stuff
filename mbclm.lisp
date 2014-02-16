(setq *print-pretty* t 
      *print-right-margin* 110 
      *read-default-float-format* 'double-float
      *readtable* (copy-readtable nil)
      *break-on-signals* nil)

(if (not (member :rune-is-character *features*))
  (push :rune-is-character *features*))

(declaim (optimize (safety 3) (debug 3) (space 0) (speed 0) (compilation-speed 0) (inhibit-warnings 0)))
(declaim (sb-ext:muffle-conditions sb-ext:compiler-note))
(declaim (sb-ext:muffle-conditions sb-ext:code-deletion-note))

(defun compiler-policy () (funcall (lambda () (sb-ext:describe-compiler-policy))))

(defun unlock-my-packages ()
  (eval-when (:compile-toplevel :load-toplevel :execute)
    #+sbcl
    (dolist (pkg '(:sb-ext :sb-debug :sb-alien :common-lisp :common-lisp-user))
      (sb-ext:unlock-package pkg))))

(defvar *last-package* nil)
(defvar *cached-prompt* nil)
(defvar *prompt* nil)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (sb-ext:without-package-locks
    
    (defun package-prompt (stream)
      (unless (eq *last-package* *package*)
	(setf *cached-prompt*
	  (concatenate 'string (or (first (package-nicknames *package*))
				 (package-name *package*))
	    "> "))
	(setf *last-package* *package*))
      (terpri)
      (princ *cached-prompt* stream))))

(setf sb-int:*repl-prompt-fun* #'package-prompt) 

(require :asdf)
(require :sb-bsd-sockets)


;; decided to use the asdf2 source-registry no more asdf:*central-registry* for first!
;;#+asdf
;;(progn
;;  (push "/home/wbooze/quicklisp/dists/quicklisp/software/cl-unicode-0.1.4/" asdf:*central-registry*)
;;  (push "/home/wbooze/quicklisp/dists/quicklisp/software/cl-ppcre-2.0.6/" asdf:*central-registry*)
;;  (push "/home/wbooze/quicklisp/dists/quicklisp/software/trivial-gray-streams-20131211-git/" asdf:*central-registry*)
;;  (push "/home/wbooze/quicklisp/dists/quicklisp/software/flexi-streams-1.0.12/" asdf:*central-registry*)
;;  (push "/home/wbooze/quicklisp/dists/quicklisp/software/usocket-0.6.1/" asdf:*central-registry*)
;;  (push "/home/wbooze/quicklisp/dists/quicklisp/software/usocket-udp-2.6/" asdf:*central-registry*)
;;  (push "/home/wbooze/quicklisp/dists/quicklisp/software/zpb-ttf-1.0.3/" asdf:*central-registry*)
;;  (push "/home/wbooze/quicklisp/dists/quicklisp/software/cl-vectors-20130312-git/" asdf:*central-registry*)
;;  (push "/home/wbooze/quicklisp/dists/quicklisp/software/clx-20131003-git/" asdf:*central-registry*)
;;  (push "/home/wbooze/quicklisp/dists/quicklisp/software/mcclim-20130813-cvs/" asdf:*central-registry*)
;;  (push "/home/wbooze/quicklisp/dists/quicklisp/software/mcclim-20130813-cvs/Experimental/freetype/" asdf:*central-registry*)

;;  (push "/home/wbooze/quicklisp/dists/quicklisp/software/mcclim-20130813-cvs/Apps/Functional-Geometry/" asdf:*central-registry*)
;;  (push "/home/wbooze/mcclim-0.9.6/" asdf:*central-registry*)
;;  (push "/home/wbooze/quicklisp/dists/quicklisp/software/flexichain_1.5.1/" asdf:*central-registry*)
;;  (push "/home/wbooze/quicklisp/dists/quicklisp/software/spatial-trees-20131211-git/" asdf:*central-registry*)
;;  (push "/home/wbooze/quicklisp/dists/quicklisp/software/climacs-20101006-cvs/" asdf:*central-registry*)
;;  (push "/home/wbooze/quicklisp/dists/quicklisp/software/slime-20131211-cvs/" asdf:*central-registry*)
;;  (push "/home/wbooze/quicklisp/dists/quicklisp/software/split-sequence-1.1/" asdf:*central-registry*)
;;  (push "/home/wbooze/quicklisp/dists/quicklisp/software/cl-irc-0.9.1/" asdf:*central-registry*)
;;  (push "/home/wbooze/quicklisp/dists/quicklisp/software/bordeaux-threads-0.8.3/" asdf:*central-registry*)
;;  (push "/home/wbooze/quicklisp/dists/quicklisp/software/alexandria-20130128-git/" asdf:*central-registry*)
;;  (push "/home/wbooze/quicklisp/dists/quicklisp/software/cl-fad-0.7.2/" asdf:*central-registry*)
;;  (push "/home/wbooze/quicklisp/dists/quicklisp/software/beirc-20101107-cvs/" asdf:*central-registry*)
;;  (push "/usr/share/common-lisp/systems/" asdf:*central-registry*))

#-quicklisp
(defun init-quick ()
(let ((*read-eval* t))
  (let ((quicklisp-init (merge-pathnames "/home/wbooze/quicklisp/setup.lisp"
			  (user-homedir-pathname))))
    (if (probe-file quicklisp-init)
      (load quicklisp-init)
      (load "/home/wbooze/quicklisp.lisp")))))

(init-quick)

;;(setq sb-ext:*evaluator-mode* :interpret)

(defun quick ()
  #+ quicklisp
  (progn 
      ;;; Check for --no-linedit command-line option.
    (if (member "--no-linedit" sb-ext:*posix-argv* :test 'equal)
      (setf sb-ext:*posix-argv* 
	(remove "--no-linedit" sb-ext:*posix-argv* :test 'equal))
      #+aclrepl
      (progn
	(ql:quickload :sb-aclrepl)
	(when (interactive-stream-p *terminal-io*)
	  (progn
	    (ignore-errors (require 'sb-aclrepl))
	    (when (find-package 'sb-aclrepl)
	      (push :aclrepl cl:*features*))
	    (setq sb-aclrepl:*max-history* 100)
	    (setf (sb-aclrepl:alias "asdc") 
	      #'(lambda (sys) (asdf:operate 'asdf:compile-op sys)))
	    (sb-aclrepl:alias "l" (sys) (asdf:operate 'asdf:load-op sys))
	    (sb-aclrepl:alias "t" (sys) (asdf:operate 'asdf:test-op sys))
	    ;; The 1 below means that two characaters ("up") are required
	    (sb-aclrepl:alias ("up" 1 "Use package") (package) (use-package package))
	    ;; The 0 below means only the first letter ("r") is required, such as ":r base64"
	    (sb-aclrepl:alias ("require" 0 "Require module") (sys) (require sys))
	    (setq cl:*features* (delete :aclrepl cl:*features*))))))))  

;; you can enable it if you want!
;;(quick)

(require :sb-posix)
(defun change-directory (pathname)
  "Ensure that the current directory seen by RUN-PROGRAM has changed, and update *default-pathname-defaults*"
  #+CMU (unix:unix-chdir (namestring pathname))
  #+scl (unix:unix-chdir (ext:unix-namestring pathname))
  #+clisp (ext:cd pathname)
  #+sbcl (sb-posix:chdir (namestring pathname))
 (setf *default-pathname-defaults* pathname))

(defun list-directory (pathname)
  ;; Sooner or later, I'm putting all the sb-posix junk back in.
  ;; I *really* don't like truenames.
  (directory pathname))

;;#-asdf
;;(defmethod asdf:perform :around ((o asdf:load-op)
;;				  (c asdf:cl-source-file))
;;  (handler-case (call-next-method o c)
;;    ;; If a fasl was stale, try to recompile and load (once).
;;    (sb-ext:invalid-fasl ()
;;      (asdf:perform (make-instance 'asdf:compile-op) c)
;;      (call-next-method))))

;;(mapcar (lambda (x) (asdf:oos 'asdf:load-op x))
;;  '(
;;     :cl-unicode
;;     :bordeaux-threads
;;     :bordeaux-fft
;;     :lisp-critic
;;     :clx
;;     :esa
;;     :mcclim
;;     :clouseau
;;     :clim-clx
;;     :esa-mcclim
;;     :drei-tests 
;;     :mcclim-freetype 
;;     :clim-examples
;;     :clim-listener
;;     :climacs
;;     :antik
;;     :mcclim-png-bitmaps 
;;     :mcclim-gif-bitmaps 
;;     :mcclim-jpeg-bitmaps 
;;     :mcclim-tiff-bitmaps))

#+quicklisp
(init-quick)

#+quicklisp
(quick)

#+quicklisp
(ql:quickload :cl-unicode)
#+asdf
(asdf:oos 'asdf:load-op :cl-unicode)

#+quicklisp
(ql:quickload :cl-ppcre)
#+asdf
(asdf:oos 'asdf:load-op :cl-ppcre)

#+quicklisp
(ql:quickload :trivial-gray-streams)
#+asdf
(asdf:oos 'asdf:load-op :trivial-gray-streams)

#+quicklisp
(ql:quickload :flexi-streams)
#+asdf
(asdf:oos 'asdf:load-op :flexi-streams)

#+quicklisp
(ql:quickload :flexichain)
#+asdf
(asdf:oos 'asdf:load-op :flexichain)

#+quicklisp
(ql:quickload :spatial-trees)
#+asdf
(asdf:oos 'asdf:load-op :spatial-trees)

#+asdf
(asdf:oos 'asdf:load-op :trivial-sockets)

#+quicklisp
(ql:quickload :usocket)
#+asdf
(asdf:oos 'asdf:load-op :usocket)

#+quicklisp
(ql:quickload :usocket-udp)
#+asdf
(asdf:oos 'asdf:load-op :usocket-udp)

#+quicklisp
(ql:quickload :cl-vectors)
#+asdf
(asdf:oos 'asdf:load-op :cl-vectors)

#+quicklisp
(ql:quickload :zpb-ttf)
#+asdf
(asdf:oos 'asdf:load-op :zpb-ttf)

#+quicklisp
(ql:quickload :clx)
#+asdf
(asdf:oos 'asdf:load-op :clx)

#+quicklisp
(ql:quickload :mcclim)
#+asdf
(asdf:oos 'asdf:load-op :mcclim)

(load "/home/wbooze/quicklisp/dists/quicklisp/software/mcclim-20130813-cvs/Lisp-Dep/fix-sbcl.lisp")
(load "/home/wbooze/quicklisp/dists/quicklisp/software/mcclim-20130813-cvs/Lisp-Dep/mp-sbcl.lisp")

#+quicklisp
(ql:quickload :clouseau)
#+asdf
(asdf:oos 'asdf:load-op :clouseau)

(load "/home/wbooze/quicklisp/dists/quicklisp/software/mcclim-20130813-cvs/Apps/Debugger/clim-debugger.lisp")

#+quicklisp
(ql:quickload :clim-clx)
#+asdf
(asdf:oos 'asdf:load-op :clim-clx)

;;(defmethod asdf:perform :after ((o asdf:load-op) (s (eql (asdf:find-system :clim-clx)))) 
;;  (asdf:oos 'asdf:load-op :mcclim-truetype))

#+quicklisp
(ql:quickload :clim-listener)
#+asdf
(asdf:oos 'asdf:load-op :clim-listener)

#+quicklisp
(ql:quickload :climacs)
#+asdf
(asdf:oos 'asdf:load-op :climacs)

#+quicklisp
(ql:quickload :mcclim-truetype)
#+asdf
(asdf:oos 'asdf:load-op :mcclim-truetype)

#+quicklisp
(ql:quickload :split-sequence)
#+asdf
(asdf:oos 'asdf:load-op :split-sequence)

#+quicklisp
(ql:quickload :cl-irc)
#+asdf
(asdf:oos 'asdf:load-op :cl-irc)

#+quicklisp
(ql:quickload :bordeaux-threads)
#+asdf
(asdf:oos 'asdf:load-op :bordeaux-threads)

#+quicklisp
(ql:quickload :alexandria)
#+asdf
(asdf:oos 'asdf:load-op :alexandria)

#+quicklisp
(ql:quickload :cl-fad)
#+asdf
(asdf:oos 'asdf:load-op :cl-fad)

#+quicklisp
(ql:quickload :beirc)
#+asdf
(asdf:oos 'asdf:load-op :beirc)

;;#+quicklisp
;;(ql:quickload :quicklisp-slime-helper)
;;(asdf:oos 'asdf:load-op :quicklisp-slime-helper)

;;(map nil #'ql:quickload
;;    '( ;;:cl-unicode
;;       :bordeaux-threads
;;       :bordeaux-fft
;;       :lisp-critic
;;       :clx
;;       :esa
;;       :mcclim
;;       :clouseau
;;       :clim-clx
;;       :esa-mcclim
;;       :drei-tests 
;;       :mcclim-freetype 
;;       :clim-examples
;;       :clim-listener))
;;       :climacs))
;;       :antik
;;       :mcclim-png-bitmaps 
;;       :mcclim-gif-bitmaps 
;;       :mcclim-jpeg-bitmaps 
;;       :mcclim-tiff-bitmaps))

(in-package :clim-listener)
#+quicklisp
(ql:quickload :functional-geometry)
(asdf:oos 'asdf:load-op :functional-geometry)


;;  (defmethod asdf:perform :around ((o asdf:load-op) (s (eql (asdf:find-system :mcclim-freetype)))) 
;;    (asdf:oos 'asdf:load-op :mcclim-truetype)))

;;  (ql:quickload :tab-layout)


(in-package :clim-user)

(defun current-view (&optional (pane-name *standard-output*))
  (funcall
    (lambda ()
	(stream-default-view pane-name))))

(defun current-frame-name (&optional (frame *application-frame*))
  (funcall
    (lambda ()
      (or
	(type-of frame)
	(slot-value frame 'climi::name)))))

(defun current-frame (&optional (frame *application-frame*))
  (funcall
    (lambda ()
      (slot-value (frame-top-level-sheet frame) 'climi::frame))))

(defun current-frame-class (&optional (frame *application-frame*))
  (funcall
    (lambda ()
      (class-of frame))))

(defun current-frame-class-description ()
  (funcall
    (lambda ()
      (describe (current-frame-class)))))

(defun current-frame-instance (&optional (frame *application-frame*))
  (funcall
    (lambda ()
      frame)))

(defun current-frame-instance-description ()
  (funcall
    (lambda ()
      (describe (current-frame-instance)))))

(defun current-frame-panes (&optional (frame *application-frame*))
  (funcall
    (lambda ()
      (slot-value frame 'climi::named-panes))))

(defun current-frame-layouts (&optional (frame *application-frame*))
  (funcall
    (lambda ()
	(slot-value frame 'climi::layouts))))

(defun current-frame-layout (&optional (frame *application-frame*))
  (funcall
    (lambda ()
	(slot-value frame 'climi::current-layout))))

(defun current-frame-layout-panes (&optional (frame *application-frame*))
  (funcall
    (lambda ()
	(slot-value frame 'climi::panes-for-layout))))

(defparameter *default-font-family-name* "-*-*-medium-r-*-*-*-180-*-*-c-*-iso8859-1")
;;  (setq *default-font-family-name* (climi::make-text-style "misc-fixed" "medium-r" 18))

(defun update-map-for-face-with-name (map family name)
  (let ((current-face (getf map family)))
    (unless current-face
	(error "family ~A not found!" family))
    (substitute `(,name ,@(cdr current-face)) current-face map)))

(defun set-fix ()
  (let ((*default-font-family-name* "-*-*-medium-r-*-*-*-180-*-*-c-*-iso8859-1"))
    (setf clim-clx::*clx-text-family+face-map*
      (clim-user::update-map-for-face-with-name
  	clim-clx::*clx-text-family+face-map* :fix clim-user::*default-font-family-name*))))

(defmethod asdf:perform :around ((o asdf:load-op) (c (eql (asdf:find-system :clim-clx))))
  (eval `(defmethod initialize-instance :around ((port ,(intern "CLX-PORT" :clim-clx)) &rest args)
	   (setf (symbol-value (intern "*CLX-TEXT-FAMILY+FACE-MAP*" :clim-clx))
	     (update-map-for-face-with-name
	       (symbol-value (intern "*CLX-TEXT-FAMILY+FACE-MAP*" :clim-clx))
	       :fix clim-user::*default-font-family-name* :large))
	   (sleep 0.01)
	   (apply #'call-next-method port args))))

  
(in-package :cl-user)

(defun clme ()
  (setq climacs-gui::*default-external-format* 'utf-8)
  (setq climacs-gui::*climacs-text-style* (clim:make-text-style :fix :bold :very-large))
  (climacs:climacs-rv :new-process t))

(defun clmi ()
  #+clim
  (progn
    (setf *debugger-hook* #'clim-debugger:debugger)
    (setf *invoke-debugger-hook* #'clim-debugger:debugger)
    (let* ((*read-default-float-format* 'double-float))
      ;; pixie doesn't work, segfaults at tiffcp in libtiff
      ;;(setf clim:*default-frame-manager* (make-instance 'climi::pixie/clx-look :port (clim:find-port)))
      
      ;;(setq drei::*highlight-strokes* nil)
      (setq drei::*use-tabs-for-indentation* t)
      (setq drei::*show-mark* t)

      (sb-sys:without-interrupts				      
	(sb-sys:with-local-interrupts	
	  (unwind-protect
	    (values
	      (sleep 0.01)
	      (clim-listener:run-listener :new-process t)
	      (sleep 0.01))))))))

#+clim
(let ((climi::*default-text-style* (climi::make-text-style :fix :roman :large)))
(defun clm ()
  (values 
    (clmi)
    (clme))))

#+clim
(let ((climi::*default-text-style* (climi::make-text-style :sans-serif :roman :large)))
(defun mbrc ()
  #+clim
  (progn
    (setf *debugger-hook* #'clim-debugger:debugger)
    (setf *invoke-debugger-hook* #'clim-debugger:debugger)
    (let* ((*read-default-float-format* 'double-float))
      ;; pixie doesn't work, segfaults at tiffcp in libtiff
      ;;(setf clim:*default-frame-manager* (make-instance 'climi::pixie/clx-look :port (clim:find-port)))
      
      ;;(setq drei::*highlight-strokes* nil)
      (setq drei::*use-tabs-for-indentation* t)
      (setq drei::*show-mark* t)

      (sb-sys:without-interrupts				      
	(sb-sys:with-local-interrupts	
	  (unwind-protect
	    (values
	      (sleep 0.01)
	      (beirc:beirc :new-process t)
	      (sleep 0.01)))))))))

;;(load "/home/wbooze/clm-4/all.lisp")

;;(eval-when (:compile-toplevel :load-toplevel :execute)
;;  (defun pds ()
;;    (ignore-errors
;;      (progn
;;	(load "/home/wbooze/prg/lisp/lisp/ppmx.lisp")
;;	(load "/home/wbooze/prg/lisp/lisp/dtrace.lisp")
;;	(load "/home/wbooze/prg/lisp/lisp/sdraw.lisp"))))
  
;;  (defun lold ()
;;    (ignore-errors
;;      (progn
;;	(load "/home/wbooze/prg/lisp/lisp/onlisp.lisp")
;;	(load "/home/wbooze/prg/lisp/lisp/onlisp-original.lisp")
;;	(load "/home/wbooze/prg/lisp/lisp/acl2.lisp")
;;	(load "/home/wbooze/prg/lisp/lisp/acl2-original.lisp")
;;	(load "/home/wbooze/prg/lisp/lisp/lol-working.lisp")
;;	(load "/home/wbooze/prg/lisp/lisp/lol-book.lisp")
;;	(load "/home/wbooze/prg/lisp/lisp/generators.lisp")))))

;;(export 'pds)
;;(export 'lold) 
;;(pds) 
;;(lold)

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun thread-list ()
  (funcall
    (let ()
      (lambda ()
	(sb-thread:list-all-threads)))))

(defun current-threads ()
    (let ()
      (lambda () (thread-list))))

(setf (symbol-value 'thread-list) (funcall (symbol-function 'thread-list)))
(setf (symbol-value 'current-threads) (current-threads))

(defun kill-first-of ()
  (sb-thread:terminate-thread (first (sb-thread:list-all-threads))))

(defun kill-last-of ()
  (sb-thread:terminate-thread (first (last (sb-thread:list-all-threads)))))

(defun kill-nth-of (n)
  (sb-thread:terminate-thread (nth n (sb-thread:list-all-threads)))))

(export 'thread-list)
(export 'kill-first-of)
(export 'kill-last-of) 
(export 'kill-nth-of)
