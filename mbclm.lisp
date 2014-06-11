(in-package :cl-user)

(setq 
 *print-pretty* t
 *print-escape* nil
 *print-circle* nil
 *print-right-margin* 110 
 *read-default-float-format* 'double-float
 *readtable* (copy-readtable nil))
 ;;*break-on-signals* nil)

(if (not (member :rune-is-character *features*))
  (push :rune-is-character *features*))

(defun nil-as-list () 
  (set-pprint-dispatch
    '(eql nil)
    (lambda (srm el)
      (cond ((null (cdr el))
              (format srm "()"))
	(t
	  (pprint-fill srm el t))))
    2))

(defun remove-nil-as-list ()
  (let*
    ((*print-pretty* nil)
      (dispatch-table (slot-value *print-pprint-dispatch* 'sb-pretty::entries)))
    (dolist (x dispatch-table)
      (cond
	((equal '(eql ()) (slot-value x 'sb-pretty::type))
	  (setf (slot-value *print-pprint-dispatch* 'sb-pretty::entries)
	    (remove x dispatch-table)))))))

(defun pprint-dispatch-cons-entries ()
  (let*
    ((*print-pretty* nil)
      (dispatch-table (slot-value *print-pprint-dispatch* 'sb-pretty::cons-entries)))
    (loop for key being the hash-keys of dispatch-table
      using (hash-value value)
      collect (list key value))))

(defun pprint-dispatch-entries (&optional p)
  (let*
    ((*print-pretty* nil)
      (dispatch-table (slot-value *print-pprint-dispatch* 'sb-pretty::entries)))
    (if p
      (dolist (x dispatch-table)
	(print x))
      dispatch-table)))

(defun pprint-dispatch-find (term) ;; (pprint-dispatch-find '(eql ())) after (nil-as-list) for example
  (let*
    ((*print-pretty* nil)
      (dispatch-table (slot-value *print-pprint-dispatch* 'sb-pretty::entries)))
    (dolist (x dispatch-table)
      (cond
	((equal term (slot-value x 'sb-pretty::type))
	  (return x))))))

(defun pprint-dispatch-remove-quote ()
  (let ((dispatch-table (slot-value *print-pprint-dispatch* (quote sb-pretty::cons-entries))))
    (loop for key being the hash-keys of dispatch-table using (hash-value hash-value)
      collect (if (equal (quote (cons (eql quote))) (slot-value hash-value (quote type)))
		(remhash key dispatch-table)))))


(defun remove-from-pprint-dispatch (term)
  (let ((dispatch-table (slot-value *print-pprint-dispatch* (quote sb-pretty::cons-entries))))
    (loop for key being the hash-keys of dispatch-table using (hash-value hash-value)
      collect (if (equal term (slot-value hash-value (quote type)))
		(remhash key dispatch-table)))))

(defun dohash (table)
  (let (result)
    (maphash (lambda (k v) (push (list k v) result)) table)
    (nreverse result)))

(defmacro do-hash ((key-var val-var hash-expr &optional result-form) &body body)
  (let ((hash-var (gensym "HASH-")))
    `(loop with ,hash-var = ,hash-expr
       for ,key-var being the hash-keys of ,hash-var
       for ,val-var being the hash-values of ,hash-var
       do (progn ,@body)
       finally (return ,result-form))))

;;; usage like...
;;;(do-hash (k v table (dohash table))
;;;  (terpri)
;;;  #+:clim
;;;  (with-drawing-options (t :text-size 16 :text-face :bold)
;;    (format t "key: ~s, value: ~s" k v)))


;; don't forget if you want your 'a stuff to expand to (quote a) properly set *print-pretty* to nil (disable)
;; else lookups are made via the pprint dispatch tables and the pprinter has its own ways of printing stuff!
;; especially don't forget to disable it when using/defining macro character functions!
;; else expansions might not be the same as expected which will confuse just more....
;; http://www.lispworks.com/documentation/HyperSpec/Body/22_ab.htm

;;;; and tho *print-pretty* is a special var disabling/enabling it from the listener won't affect it's value
;;;; in the sbcl repl! (threads ?)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;,;;;;;;;;;;;;;;;;;

(setq *clocc-root* "/home/wbooze/clocc/")
(load "clocc/src/ytools/ytload/ytload")
(setq ytools::config-directory* "/home/wbooze/")
(setq ytools::ytload-directory* "clocc/src/ytools/ytload/")

(defun paip ()
  (ignore-errors
    (progn
      (load "/home/wbooze/prg/lisp/paip/norvig-old/auxmacs.lisp")
      (load "/home/wbooze/prg/lisp/paip/norvig-old/auxfns.lisp"))))

(export 'cl-user::paip)

(defun paip-new ()
  (ignore-errors
    (progn 
      (load "/home/wbooze/prg/lisp/paip-pjb/norvig-paip-pjb.lisp"))))

(export 'cl-user::paip-new)

(defun make-loops (var-list-pairs last-loop-body)
  (if (endp var-list-pairs)
    last-loop-body
    (destructuring-bind ((var list) . therest) var-list-pairs
      `(loop for ,var in ,list do ,(make-loops therest last-loop-body)))))

(defmacro make-comprehension (expr vars-in-lists guards)
  (let ((accum (gensym)))
    `(let ((,accum (list)))
       (progn 
	 ,(make-loops vars-in-lists
	    `(if (and ,@guards)
	       (push ,expr ,accum)))
	 (nreverse ,accum)))))

(defun bubble-sort (array cmp-fun) 
  "Bubble sort implementation in common lisp. Using the extended loop facility."
  (let ((result (copy-seq array)))
    (loop for i from (1- (length result)) downto 0 do
      (loop for j from 0 to i
	when (funcall cmp-fun (aref result i) (aref result j))
	do (rotatef (aref result i) (aref result j)) ))
    result))

(defun insertion-sort (cmp-fun array-to-sort) 
  "Implementation of a destructive insertion sort on ARRAY-TO-SORT.
The CMP-FUN is used to parametrize the order conditions. 
This sort is generic, that means it can sort all kinds of objects for which
one can run the CMP-FUN"

  (flet ((insert-into-sorted (index)
           ;; example of a local function all outer variables are 
           ;; accessible from within this local function
           (let ((el (aref array-to-sort index)))
             ;; save the element for later insertion
             (loop for j = (1- index) then (1- j)
	       while (and (>= j 0) (not (funcall cmp-fun (aref array-to-sort j) el)))
	       ;; the not is needed because the following should move all elements
	       ;; not in order to the right
	       do (setf (aref array-to-sort (1+ j)) (aref array-to-sort j))
	       finally  (setf (aref array-to-sort (1+ j)) el)))))
    ;; now we can add el at the proper place
    (loop for i from 0 upto (1- (length array-to-sort))
      do (insert-into-sorted i)))
  array-to-sort)

(defun small (list)
  (or (null list) (null (cdr list))))

(defun right-half (list)
  (last list (ceiling (/ (length list) 2))))

(defun left-half (list)
  (ldiff list (right-half list)))

(defun merge-lists (list1 list2)
  (merge 'list list1 list2 #'<))

(defun merge-sort (list)
  (if (small list) list
    (merge-lists
      (merge-sort (left-half list))
      (merge-sort (right-half list)))))

(defgeneric lt (some other))

(defmethod lt ((some number) (other number)) (< some other))

(defmethod lt ((some string) (other string)) (string< some other))

(defun qsort (l)
  (when l (destructuring-bind (p . xs) l
            (append (qsort (@ x x xs (lt x p))) (list p)
	      (qsort (@ x x xs (not (lt x p))))))))

(defun qsort-alt (list) 
  (when list 
    (destructuring-bind (p . xs) list 
      (loop for x in xs if (lt x p) 
	collect x into lesser 
	else collect x into greater 
	finally (return (append (quicksort-alt lesser) (list p) (quicksort-alt greater)))))))

(defun gen-tuples-m (lst)
  (reduce (lambda (b rest)
            (loop for xs in rest
	      append (loop for i from 1 to b
		       collecting (cons i xs))))
    lst
    :from-end t
    :initial-value '(())))

(defun string-to-number (str &optional (base *read-base*) &rest rest)
  (read-from-string str base))

(defun number-to-string (num &optional (base *read-base*) &rest rest)
  (write-to-string num :base base))

(defun read-char-by-name (stream)
  "blabla"
  (let* ((input stream))
    (cond
      ((equal "^[0-9a-fA-F]+s" input)
	(string-to-number input 16))
      ((equal "^#" input)
	(read input))
      (t (read stream)))))

(defun insert (&rest args)
  args)

(defun pds ()
  (ignore-errors
    (progn
      (load "/home/wbooze/prg/lisp/lisp/ppmx.lisp")
      (load "/home/wbooze/prg/lisp/lisp/dtrace.lisp")
      (load "/home/wbooze/prg/lisp/lisp/sdraw.lisp"))))

  (defun lold ()
    (ignore-errors
      (progn
	(load "/home/wbooze/prg/lisp/lisp/package.lisp")
	(load "/home/wbooze/prg/lisp/lisp/onlisp-util.lisp")
	(load "/home/wbooze/prg/lisp/lisp/onlisp-app.lisp")
	(load "/home/wbooze/prg/lisp/lisp/lol-working.lisp")
	(load "/home/wbooze/prg/lisp/lisp/lol-book.lisp")
	(load "/home/wbooze/prg/lisp/lisp/generators.lisp"))))

(defun acl2 ()
  (load "/home/wbooze/prg/lisp/lisp/acl2.lisp"))

(export 'cl-user::pds)
(export 'cl-user::lold)
(export 'cl-user::acl2)

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

;;(setq sb-ext:*evaluator-mode* :interpret)


(require :sb-aclrepl)

(defun quick ()
   ;;; Check for --no-linedit command-line option.
  #+quicklisp
  (if (member "--no-linedit" sb-ext:*posix-argv* :test 'equal)
    (setf sb-ext:*posix-argv* (remove "--no-linedit" sb-ext:*posix-argv* :test 'equal))
    (progn
      (if (interactive-stream-p *standard-output*)
	(require :sb-aclrepl)
	(require :sb-aclrepl))
      (if (find-package :sb-aclrepl)
	(progn
	  (push :aclrepl cl:*features*)
	  (setq sb-aclrepl:*max-history* 100)
	  (setf (sb-aclrepl:alias "asdc") #'(lambda (sys) (asdf:operate 'asdf:compile-op sys)))
	  (sb-aclrepl:alias "l" (sys) (asdf:operate 'asdf:load-op sys))
	  (sb-aclrepl:alias "t" (sys) (asdf:operate 'asdf:test-op sys))
	  ;; The 1 below means that two characaters ("up") are required
	  (sb-aclrepl:alias ("up" 1 "Use package") (package) (use-package package))
	  ;; The 0 below means only the first letter ("r") is required, such as ":r base64"
	  (sb-aclrepl:alias ("require" 0 "Require module") (sys) (require sys)))
	(setq cl:*features* (delete :aclrepl cl:*features*))))))


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

#-:quicklisp
(init-quick)


#+:quicklisp
(quick)

(ql:quickload :asdf) ;; upgrade it on the way (to eventually higher versions)
(require :sb-posix)
(require :sb-bsd-sockets)

#+:quicklisp
(defun sa (args)
  (ql:system-apropos (string-downcase args)))

#+:quicklisp
(defun ql (args)
  (ql:quickload (string-downcase args)))

#+:quicklisp
(ql:quickload :named-readtables)
(asdf:oos 'asdf:load-op :named-readtables)

#+:quicklisp
(ql:quickload :closer-mop)
(asdf:oos 'asdf:load-op :closer-mop)

#+:quicklisp
(ql:quickload :cl-unicode)
(asdf:oos 'asdf:load-op :cl-unicode)

#+:quicklisp
(ql:quickload :cl-ppcre)
(asdf:oos 'asdf:load-op :cl-ppcre)

#+:quicklisp
(ql:quickload :trivial-gray-streams)
(asdf:oos 'asdf:load-op :trivial-gray-streams)

#+:quicklisp
(ql:quickload :flexi-streams)
(asdf:oos 'asdf:load-op :flexi-streams)

#+:quicklisp
(ql:quickload :flexichain)
(asdf:oos 'asdf:load-op :flexichain)

#+:quicklisp
(ql:quickload :spatial-trees)
(asdf:oos 'asdf:load-op :spatial-trees)

;;#+asdf
;;(asdf:oos 'asdf:load-op :trivial-sockets)

#+:quicklisp
(ql:quickload :usocket)
(asdf:oos 'asdf:load-op :usocket)

#+:quicklisp
(ql:quickload :usocket-udp)
(asdf:oos 'asdf:load-op :usocket-udp)

#+:quicklisp
(ql:quickload :cl-vectors)
(asdf:oos 'asdf:load-op :cl-vectors)

#+:quicklisp
(ql:quickload :cl-freetype2)
(asdf:oos 'asdf:load-op :cl-freetype2)

#+:quicklisp
(ql:quickload :zpb-ttf)
(asdf:oos 'asdf:load-op :zpb-ttf)

#+:quicklisp
(ql:quickload :clx)
(asdf:oos 'asdf:load-op :clx)

#+:quicklisp
(ql:quickload :mcclim)
(asdf:oos 'asdf:load-op :mcclim)

(load "/home/wbooze/quicklisp/dists/quicklisp/software/mcclim-20130813-cvs/Lisp-Dep/fix-sbcl.lisp")
(load "/home/wbooze/quicklisp/dists/quicklisp/software/mcclim-20130813-cvs/Lisp-Dep/mp-sbcl.lisp")

(in-package :cl-user)

#+:quicklisp
(ql:quickload :clouseau)
(asdf:oos 'asdf:load-op :clouseau)

(load "/home/wbooze/quicklisp/dists/quicklisp/software/mcclim-20130813-cvs/Apps/Debugger/clim-debugger.lisp")

(in-package :cl-user)

#+:quicklisp
(ql:quickload :clim-clx)
(asdf:oos 'asdf:load-op :clim-clx)

;;(defmethod asdf:perform :after ((o asdf:load-op) (s (eql (asdf:find-system :clim-clx)))) 
;;  (asdf:oos 'asdf:load-op :mcclim-truetype))

#+:quicklisp
(ql:quickload :clim-listener)
(asdf:oos 'asdf:load-op :clim-listener)

#+:quicklisp
(ql:quickload :climacs)
(asdf:oos 'asdf:load-op :climacs)

#+:quicklisp
(ql:quickload :mcclim-truetype)
(asdf:oos 'asdf:load-op :mcclim-truetype)

#+:quicklisp
(ql:quickload :split-sequence)
(asdf:oos 'asdf:load-op :split-sequence)

#+:quicklisp
(ql:quickload :cl-irc)
(asdf:oos 'asdf:load-op :cl-irc)

#+:quicklisp
(ql:quickload :bordeaux-threads)
(asdf:oos 'asdf:load-op :bordeaux-threads)

#+:quicklisp
(ql:quickload :alexandria)
(asdf:oos 'asdf:load-op :alexandria)

#+:quicklisp
(ql:quickload :cl-fad)
(asdf:oos 'asdf:load-op :cl-fad)

#+:quicklisp
(ql:quickload :beirc)
(asdf:oos 'asdf:load-op :beirc)

#+:quicklisp
(ql:quickload :cxml)
(asdf:oos 'asdf:load-op :cxml)

#+:quicklisp
(ql:quickload :cl-gd)
(asdf:oos 'asdf:load-op :cl-gd)

#+:quicklisp
(ql:quickload :imago)
(asdf:oos 'asdf:load-op :imago)

#+:quicklisp
(ql:quickload :clem)
(asdf:oos 'asdf:load-op :clem)

#+:quicklisp
(ql:quickload :ch-image)
(asdf:oos 'asdf:load-op :ch-image)

#+:quicklisp
(ql:quickload :cl-graph)
(asdf:oos 'asdf:load-op :cl-graph)

#+:quicklisp
(ql:quickload :ph-maths)
(asdf:oos 'asdf:load-op :ph-maths)

#+:quicklisp
(ql:quickload :generic-math)
(asdf:oos 'asdf:load-op :generic-math)

#+:quicklisp
(ql:quickload :antik)
(asdf:oos 'asdf:load-op :antik)

#+:quicklisp
(ql:quickload :gsll)
(asdf:oos 'asdf:load-op :gsll)

#+:quicklisp
(ql:quickload :math-high)
(asdf:oos 'asdf:load-op :math-high)

#+:quicklisp
(ql:quickload :math-functions)
(asdf:oos 'asdf:load-op :math-functions)

#+:quicklisp
(ql:quickload :cl-css)
(asdf:oos 'asdf:load-op :cl-css)

#+:quicklisp
(ql:quickload :cl-json)
(asdf:oos 'asdf:load-op :cl-json)

#+:quicklisp
(ql:quickload :st-json)
(asdf:oos 'asdf:load-op :st-json)

#+:quicklisp
(ql:quickload :yason)
(asdf:oos 'asdf:load-op :yason)

#+:quicklisp
(ql:quickload :closure)
(asdf:oos 'asdf:load-op :closure)

#+:quicklisp
(ql:quickload :quicklisp-slime-helper)
(asdf:oos 'asdf:load-op :quicklisp-slime-helper)

(defun load-gsl ()
  #+quicklisp
  (map nil #'ql:quickload 
    '(:osicat 
       :metabang-bind 
       :static-vectors 
       :puri
       :cl-base64
       :cl+ssl
       :chunga
       :drakma
       :asdf-system-connections
       :antik
       :gsll
       :colorize))
  (map nil #'(lambda (x) (asdf:oos 'asdf:load-op x))
    '(:osicat 
       :metabang-bind 
       :static-vectors 
       :puri
       :cl-base64
       :cl+ssl
       :chunga
       :drakma
       :asdf-system-connections
       :antik
       :gsll
       :colorize)))

(defun load-sdl ()
  (progn
    #+:quicklisp
    (ql:quickload :trivial-garbage)
    (asdf:oos 'asdf:load-op :trivial-garbage)
    
    #+:quicklisp
    (ql:quickload :trivial-features)
    (asdf:oos 'asdf:load-op :trivial-features)
    
    #+:quicklisp
    (ql:quickload :babel)
    (asdf:oos 'asdf:load-op :babel)
    
    #+:quicklisp
    (ql:quickload :cffi)
    (asdf:oos 'asdf:load-op :cffi)
    
    #+:quicklisp
    (ql:quickload :lispbuilder-sdl)
    (asdf:oos 'asdf:load-op :lispbuilder-sdl)))

(defun load-png-stuff ()
  (progn
    
    #+:quicklisp
    (ql:quickload :iterate)
    (asdf:oos 'asdf:load-op :iterate)

    #+:quicklisp
    (ql:quickload :chipz)
    (asdf:oos 'asdf:load-op :chipz)

    #+:quicklisp
    (ql:quickload :png-read)
    (asdf:oos 'asdf:load-op :png-read)

    #+:quicklisp
    (ql:quickload :mcclim-png-bitmaps)
    (asdf:oos 'asdf:load-op :mcclim-png-bitmaps)))

(defun load-jpeg-stuff ()
  (progn
    
    #+:quicklisp
    (ql:quickload :cl-jpeg)
    (asdf:oos 'asdf:load-op :cl-jpeg)

    #+:quicklisp
    (ql:quickload :mcclim-jpeg-bitmaps)
    (asdf:oos 'asdf:load-op :mcclim-jpeg-bitmaps)))

(defun load-gif-stuff ()
  (progn

    #+:quicklisp
    (ql:quickload :skippy)
    (asdf:oos 'asdf:load-op :skippy)

    #+:quicklisp
    (ql:quickload :mcclim-gif-bitmaps)
    (asdf:oos 'asdf:load-op :mcclim-gif-bitmaps)))

(defun load-tiff-stuff ()
  (progn

    #+:quicklisp
    (ql:quickload :ieee-floats)
    (asdf:oos 'asdf:load-op :ieee-floats)

    #+:quicklisp
    (ql:quickload :com.gigamonkeys.binary-data)
    (asdf:oos 'asdf:load-op :com.gigamonkeys.binary-data)

    #+:quicklisp
    (ql:quickload :retrospectiff)
    (asdf:oos 'asdf:load-op :retrospectiff)

    #+:quicklisp
    (ql:quickload :mcclim-tiff-bitmaps)
    (asdf:oos 'asdf:load-op :mcclim-tiff-bitmaps)))


(defun load-ernestine ()
  (progn
    #+:quicklisp
    (map nil #'ql:quickload
      '(cl-ppcre
	 drakma
	 split-sequence
	 cl-prevalence
	 s-sysdeps
	 s-xml
	 ernestine))
    (map nil #'(lambda (x) (asdf:oos 'asdf:load-op x))
      '(cl-ppcre
	 drakma
	 split-sequence
	 cl-prevalence
	 s-sysdeps
	 s-xml
	 ernestine))))

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
#+:quicklisp
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

;;(defparameter *default-font-family-name* "-*-unifont-*-*-*-*-*-180-*-*-*-*-iso10646-1")
(defparameter *default-font-family-name* "-*-dejavu sans mono-bold-r-normal-*-*-180-*-*-*-*-iso10646-1")
;;  (setq *default-font-family-name* (climi::make-text-style "misc-fixed" "medium-r" 18))

(defun update-map-for-face-with-name (map family name)
  (let ((current-face (getf map family)))
    (unless current-face
      (error "family ~A not found!" family))
    (substitute `(,name ,@(cdr current-face)) current-face map)))

(defun set-fix ()
  (let ((*default-font-family-name* "-*-unifont-*-*-*-*-*-180-*-*-*-*-iso10646-1"))
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

(defun change-directory (pathname)
  "Ensure that the current directory seen by RUN-PROGRAM has changed, and update *default-pathname-defaults*"
  #+CMU (unix:unix-chdir (namestring pathname))
  #+scl (unix:unix-chdir (ext:unix-namestring pathname))
  #+clisp (ext:cd pathname)
  #+sbcl (sb-posix:chdir (pathname (namestring pathname)))
  (setf *default-pathname-defaults* (pathname (namestring pathname))))

(defun list-dir (pathname)
  ;; (list-dir '/home/wbooze) for example
  (loop for f in 
    (directory (make-pathname :directory (string-downcase pathname) :name :wild :type :wild)) 
    collect f))

(defun print-dir (pathname)
  ;; (print-directory '/home/wbooze) for example
  (loop for f in 
    (directory (make-pathname :directory (string-downcase pathname) :name :wild :type :wild)) 
    do (print f)))

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
      ;;(setq drei::*use-tabs-for-indentation* t)
      ;;(setq drei::*show-mark* t)

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
    (progn 
      (clmi)
      (clme))))

#+clim
(let ((climi::*default-text-style* (climi::make-text-style :sans-serif :roman :very-large))
      (climacs-gui::*climacs-text-style* (clim:make-text-style :sans-serif :roman :very-large)))
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
		(let ((*read-eval* nil))
		  (beirc:beirc :new-process t))
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

(defun kill-listener ()
  (let ((thread-list (sb-thread:list-all-threads)))
    (dolist (x thread-list)
      (cond ((equal "Listener" (sb-thread:thread-name x))
	      (sb-thread:terminate-thread x))))))

(defun kill-climacs ()
  (let ((thread-list (sb-thread:list-all-threads)))
    (dolist (x thread-list)
      (cond ((equal (or "Climacs-RV" "Climacs") (sb-thread:thread-name x))
	      (sb-thread:terminate-thread x))))))

(defun kill-beirc ()
  (let ((thread-list (sb-thread:list-all-threads)))
    (dolist (x thread-list)
      (cond ((equal "Beirc GUI process" (sb-thread:thread-name x))
	      (sb-thread:terminate-thread x))))))

(defun kill-closure ()
  (let ((thread-list (sb-thread:list-all-threads)))
    (dolist (x thread-list)
      (cond ((equal "Closure" (sb-thread:thread-name x))
	      (sb-thread:terminate-thread x))))))

(defun rcl ()
  (load-png-stuff)
  (load-gif-stuff)
  (load-jpeg-stuff)
  (load-tiff-stuff)
  (closure:start))

(defun ucs-insert (&optional character (count 1))
  "given a character returns the unicode symbol or reads input and then returns the symbol"

  (if character
    (let ((result (or (string (code-char character)) (string character))))
      (progn
        (dotimes (i count)
	  (format t "~s" result))))

    (progn
      (let* (
	      (character (read-char))
	      (result (or (string character) (string (code-char character)))))
	(dotimes (i count)
	  (format t "~s" result))))))

(defun ascii-table ()
  (let ((i -1))
    (format t "~&ASCII characters 32 thru 127.~&~%")
    (format t "   Dec  Hex  Char         |   Dec  Hex   Char         |   Dec  Hex   Char         |   Dec  Hex   Char~%")
    (loop while (< i 31) do
      (princ (format nil "~4d ~4x    ~12s | ~4d ~4x    ~12s | ~4d ~4x    ~12s | ~4d ~4x    ~12s~%"
	       (setq i (+ 33  i)) i (code-char i)
	       (setq i (+ 32 i)) i (code-char i)
	       (setq i (+ 32 i)) i (code-char i)
	       (setq i (+ 1 i)) i (code-char i)))
      (setq i (- i 95)))) (values))

(defun ascii-table-s ()
  (let ((i -1))
    (format t "~&ASCII characters 32 thru 127.~&~%")
    (format t "   Dec  Hex  Char         |   Dec  Hex   Char         |   Dec  Hex   Char         |   Dec  Hex   Char~%")
    (loop while (< i 31) do
      (princ (format nil "~4d ~4x    ~12s | ~4d ~4x    ~12s | ~4d ~4x    ~12s | ~4d ~4x    ~12s~%"
	       (setq i (+ 33  i)) i (string (code-char i))
	       (setq i (+ 32 i)) i (string (code-char i))
	       (setq i (+ 32 i)) i (string (code-char i))
	       (setq i (+ 1 i)) i (string (code-char i))))
      (setq i (- i 95)))) (values))

(defun extended-table ()
  (let ((i 128))
    (format t "~&extended ASCII characters (unicode) 128 thru 256.~&~%")
    (format t " Dec   Hex   Char  |  Dec   Hex   Char~%")
    (loop while (< i 256) do
      (princ (format nil "~4d ~4x ~50s  |  ~4d ~4x ~50s~%"
	       i i (code-char i)
	       (incf i) i (code-char i)))
      (incf i))) (values))

(defun extended-table-s ()
  (let ((i 128))
    (format t "~&extended ascii characters (unicode) 128 thru 256.~&~%")
    (format t " dec   hex   char  |  dec   hex   char~%")
    (loop while (< i 256)
      do (princ
	   (format nil "~4d ~4x ~50s  |  ~4d ~4x ~50s~%" 
	     i i (string (code-char i)) 
	     (incf i) i (string (code-char i))))
      (incf i))) (values))

(defun ucs-codes-t (start row col) ;; terminal version
  (let ((x start) (somechars nil))
    (do ((i 1 (1+ i)))
      ((> i row))
      (terpri)
      (do ((j 1 (1+ j)))
	((> j col))
	(format t "~s " (string (code-char x)))
	(incf x)))))

(defun ucs-codes-tl (start row col) ;; terminal-list version
  (let ((x start) (somechars nil))
    (do ((i 1 (1+ i)))
      ((> i row))
      (do ((j 1 (1+ j)))
	((> j col))
	(setq somechars (append somechars (list (string (code-char x)))))
	(incf x))) somechars))


(in-package :cl-user)
(defun ma (args) (macroexpand args))
(defun ma-1 (args) (macroexpand-1 args))

(defun load-maxima ()
  (cl-user::change-directory "/home/wbooze/maxima-code/src/")
  (load "maxima-build.lisp")
  (cl-user::maxima-load)
  (cl-user::change-directory "/home/wbooze/"))


(export 'cl-user::pds)
(export 'cl-user::lold)
(export 'cl-user::acl2)
(export 'cl-user::clm)
(export 'cl-user::mbrc)
(export 'cl-user::insert)
(export 'cl-user::ucs-insert)

(export 'cl-user::ascii-table)
(export 'cl-user::extended-table)
(export 'cl-user::ascii-table-s)
(export 'cl-user::extended-table-s)
(export 'cl-user::ucs-codes-t)
(export 'cl-user::ucs-codes-tl)
(export 'cl-user::pds)
(export 'cl-user::ma)
(export 'cl-user::ma-1)
(export 'cl-user::nil-as-list)
(export 'cl-user::remove-nil-as-list)
(export 'cl-user::sa)
(export 'cl-user::ql)
(export 'cl-user::pprint-dispatch-cons-entries)
(export 'cl-user::pprint-dispatch-entries)
(export 'cl-user::pprint-dispatch-find)

(export 'cl-user::change-directory)
(export 'cl-user::list-dir)

(export 'cl-user::clme)
(export 'cl-user::clmi)

(export 'cl-user::thread-list)
(export 'cl-user::kill-first-of)
(export 'cl-user::kill-last-of) 
(export 'cl-user::kill-nth-of)
(export 'cl-user::kill-listener)
(export 'cl-user::kill-climacs)
(export 'cl-user::kill-beirc)
(export 'cl-user::kill-closure)
(export 'cl-user::load-maxima)

(in-package :clim-user)

#+clim
(let ((*print-pretty* nil))
  (defmacro in-listener (&rest body)
    `(clim-user::with-drawing-options (*standard-output* :text-size :very-large) ,@body)))

#+clim
(let ((*print-pretty* nil))
  (defmacro in-clim (&rest body)
      `(if (and 
          (equal *package* (find-package :clim-user))
          (find :clim *features*))
         (clim-user::with-drawing-options (*standard-output* :text-size :huge) (progn ,@body))
         (progn ,@body))))

;; for this to work remove the installed version of maxima and 
;; get the sources and compile it in some directory
;; the way it's told in README.lisp or INSTALL.lisp
;; (load "configure.lisp") (configure) (quit)
;; (load "maxima-build.lisp") (maxima-compile) (quit)
;; and rename your maxima-init.mac to something else like maxima-init-old.mac!
;; i don't know why it interferes with.
(defun load-maxima ()
  (cl-user::change-directory "/home/wbooze/maxima-code/src/")
  (load "maxima-build.lisp")
  (maxima-load)
  (cl-user::change-directory "/home/wbooze/"))

;; package change again!
(in-package :cl-user)

(load "/home/wbooze/clocc/clocc.lisp")
(setf *clocc-root* "/home/wbooze/clocc/")
(compile-file (concatenate 'string *clocc-root* "clocc"))
(load (compile-file (concatenate 'string *clocc-root* "clocc")))

(load "clocc/src/ytools/ytload/ytload")
(setq ytools::config-directory* "/home/wbooze")
(setq ytools::ytload-directory* "clocc/src/ytools/ytload")

(defun load-sys (sys)
 (load (concatenate 'string *clocc-root* "clocc"))
  (load (translate-logical-pathname (concatenate 'string "clocc:src;" sys))))

(load-sys "cllib;base")
(load-sys "cllib;autoload")
(load-sys "cllib;simple")
(load-sys "cllib;clhs")

(setf (logical-pathname-translations "norvig")
  `(("norvig:**;*.*.*" "/home/wbooze/prg/lisp/paip-pjb/norvig/**/*.*")))

(setq *default-pathname-defaults*
  (merge-pathnames
    *default-pathname-defaults*
    (make-pathname :directory '(:relative "prg/lisp/paip-pjb/"))))

(defun subclasses (class) (clim-listener::com-show-class-subclasses class))
(defun superclasses (class) (clim-listener::com-show-class-superclasses class))
(defun gfs (class) (clim-listener::com-show-class-generic-functions class))
(defun slots (class) (clim-listener::com-show-class-slots class))
(defun info (obj) (describe
		    (or
		      (find-class (find obj (apropos-list obj nil t)) nil)
		      (find-class (first (apropos-list obj nil t)) nil))))

(defun gf (gf) (clim-listener::com-show-generic-function
		 (sb-pcl::find-generic-function gf)))

(import 'cl-user::clm)
(import 'cl-user::mbrc)
(import 'cl-user::insert) 
(import 'cl-user::ucs-insert) 

(import 'cl-user::pds) 
(import 'cl-user::lold)
(import 'cl-user::acl2)

(import 'cl::dribble) 

(import 'cl-user::ascii-table) 
(import 'cl-user::extended-table)
(import 'cl-user::ascii-table-s)
(import 'cl-user::extended-table-s)
(import 'cl-user::paip)
(import 'cl-user::paip-new)
(import 'cl-user::ma)
(import 'cl-user::ma-1)
(import 'cl-user::nil-as-list)
(import 'cl-user::remove-nil-as-list)
(import 'cl-user::sa)
(import 'cl-user::ql)
(import 'cl-user::pprint-dispatch-cons-entries)
(import 'cl-user::pprint-dispatch-entries)
(import 'cl-user::pprint-dispatch-find)

(import 'cl-user::change-directory)
(import 'cl-user::list-dir)

(import 'cl-user::clme)
(import 'cl-user::clmi)

(import 'cl-user::thread-list)
(import 'cl-user::kill-first-of)
(import 'cl-user::kill-last-of) 
(import 'cl-user::kill-nth-of)
(import 'cl-user::kill-listener)
(import 'cl-user::kill-climacs)
(import 'cl-user::kill-beirc)
(import 'cl-user::kill-closure)

(lold) ;;via this we get (#{1 5}) for expanding into (1 2 3 4 5) and the pg namespace funcs

(defun |#{-reader| (stream char arg)
  (declare (ignore char arg))
  (let ((pair (read-delimited-list #\} stream t)) (accum ()))
    (push (mapcon #'(lambda (x) (mapcar #'(lambda (y) (list (car x) y)) (cdr x))) pair) accum)
    (list 'quote (first accum))))

(defun |#{-reader| (stream char arg)
  (declare (ignore char arg))
  (let ((pair (read-delimited-list #\} stream t)) (accum ()) (arg (if arg arg 2)))
    (push (paul-graham:group pair arg) accum)
    (lambda () (list 'quote (first accum)))))

(set-dispatch-macro-character #\# #\{ #'|#{-reader|)
(set-macro-character #\} (get-macro-character #\) nil))

;;via the above we get (#2{a b c d}) expanding into pairs i.e. ((a b) (c d)), numerical arg changes grouping!


;;(defun quote-as-apostrophe (&optional (*readtable* (copy-readtable *read-table*)))
;;  (set-macro-character #\' 
;;    #'(lambda (stream char) 
;;	(declare (ignore char)) 
;;	(list '' (read stream t nil t)))))

;;(defun quote-as-literal-quote (&optional (*readtable* (copy-readtable *read-table*)))
;;  (set-macro-character #\' 
;;    #'(lambda (stream char) 
;;	(declare (ignore char)) 
;;	(list 'quote (read stream t nil t)))))

(in-package :cl-user)

(defun walk-tree (fun tree)
  (subst-if t
    (constantly nil)
    tree
    :key fun))

(defun walk-tree-atoms (fun tree)
  (tree-equal tree tree
    :test (lambda (element-1 element-2)
	    (declare (ignore element-2))
	    (funcall fun element-1)
	    t)))

(defun list-of-bits (integer)
  (let ((bits '()))
    (dotimes (index (integer-length integer) bits)
      (push (if (logbitp index integer) 1 0) bits))))

(defun list-of-bits (integer)
  (let ((i integer)
	 (bits '()))
    (dotimes (j (integer-length integer) bits)
      (push (logand i 1) bits)
      (setf i (ash i -1)))))

(defun list-of-bits (integer)
  (let ((mask 1)
	 (bits '()))
    (dotimes (i (integer-length integer) bits)
      (push (if (logtest mask integer) 1 0) bits)
      (setf mask (ash mask 1)))))

(defun list-of-bits (integer)
  (let ((bits '()))
    (dotimes (position (integer-length integer) bits)
      (push (ldb (byte 1 position) integer) bits))))

(defun :bin (value &optional (size 8))
  (format nil "#b~v,'0B" size value))

(defun :bin (value &key (size 64) (byte 8))
  (loop for position from (- size byte) downto -1 by byte

    with result = (ldb (byte byte position) value)
    and left-shift = (ash (ldb (byte byte position) value) 1)  
    and right-shift = (ash (ldb (byte byte position) value) -1)

    do
    (format t "~%~70<~v,'0b~>~&~70<~v,'0b~>~&~70<~v,'0b~>~% " byte result byte left-shift byte right-shift)
    ))

(defun :oct (value &optional (size 3))
  (format nil "#o~v,'0O" size value))

(defun :hex (value &optional (size 3))
  (format nil "#x~v,'0X" size value))

(defun :dec (value &optional (size 3))
  (format nil "#d~v,'0d" size value))

(defun bin->hex (bin)
  (:hex (values (read-from-string (:bin bin) t nil :start 2))))

(defun hex->bin (hex)
  (:bin (values (read-from-string (:hex hex) t nil :start 2))))

(defun oct->bin (oct)
  (:bin (values (read-from-string (:oct oct) t nil :start 2))))

(defun bin->oct (bin)
  (:oct (values (read-from-string (:bin bin) t nil :start 2))))

(defun bin->dec (bin)
  (:dec (values (read-from-string (:bin bin) t nil :start 2))))

(defun dec->bin (dec)
  (:bin (values (read-from-string (:dec dec) t nil :start 2))))

(defun hex->dec (hex)
  (:dec (values (read-from-string (:hex hex) t nil :start 2))))

(defun dec->hex (dec)
  (:hex (values (read-from-string (:dec dec) t nil :start 2))))

(defun oct->dec (oct)
  (:dec (values (read-from-string (:oct oct) t nil :start 2))))

(defun dec->oct (dec)
  (:oct (values (read-from-string (:dec dec) t nil :start 2))))

(defun hex->oct (hex)
  (:oct (values (read-from-string (:hex hex) t nil :start 2))))

(defun oct->hex (oct)
  (:hex (values (read-from-string (:oct oct) t nil :start 2))))

(defun bits (value &optional (size 8))
  (cond 
    ((and (stringp value) (equal (values (read-from-string value nil nil :start 1 :end 2)) 'x))
      (values (read-from-string (format nil "~v,'0B" size (hex->bin value)) t nil :start 2)))
    ((and (stringp value) (equal (values (read-from-string value nil nil :start 1 :end 2)) 'o))
      (values (read-from-string (format nil "~v,'0B" size (oct->bin value)) t nil :start 2)))
    ((and (stringp value) (equal (values (read-from-string value nil nil :start 1 :end 2)) 'd))
      (values (read-from-string (format nil "~v,'0B" size (dec->bin (read-from-string value t nil :start 2))) t nil :start 2)))
    ((and (stringp value) (equal (values (read-from-string value nil nil :start 1 :end 2)) 'b))
      (values (read-from-string (format nil "~v,'0B" size value) t nil :start 2)))
    ((numberp value) (values (read-from-string (format nil "~v,'0B" size value))))
    (t
      (values (read-from-string (format nil "~v,'0B" size value) t nil :start 2)))))

;;(export '(hex->bin bin->hex oct->bin bin->oct bin->dec dec->bin hex->dec dec->hex oct->dec dec->oct hex->oct oct->hex bits list-of-bits walk-tree;; walk-tree-atoms))


(defun next-epsi (epsi) (/ epsi 2))

(defun epsi-sig-single-p (epsi) (> (+ 1.0f0 epsi) 1.0f0))
(defun epsi-sig-double-p (epsi) (> (+ 1.0d0 epsi) 1.0d0))

(defun is-epsi-single-p (epsi) 
  (and (epsi-sig-single-p epsi) 
    (not (epsi-sig-single-p (next-epsi epsi)))))

(defun is-epsi-double-p (epsi) 
  (and (epsi-sig-double-p epsi) 
    (not (epsi-sig-double-p (next-epsi epsi)))))

(defun find-epsi-single (&OPTIONAL (epsi 1.0f0)) 
  (if (is-epsi-single-p epsi)  ; if the next epsi candidate isn't significant
    epsi  ; we have found epsilon
    (find-epsi-single (next-epsi epsi)))) ; otherwise, go smaller

(defun find-epsi-double (&OPTIONAL (epsi 1.0d0)) 
  (if (is-epsi-double-p epsi)  ; if the next epsi candidate isn't significant
    epsi  ; we have found epsilon
    (find-epsi-double (next-epsi epsi)))) ; otherwise, go smaller

(format t "~% machine-epsilon-single: ~a ~% machine-epsilon-double: ~a ~% epsi-sig-single-p? ~a ~% epsi-sig-double-p? ~a ~%"(find-epsi-single) (find-epsi-double) (epsi-sig-single-p (find-epsi-single)) (epsi-sig-double-p (find-epsi-double)))

(print "Happy lisping!" t)
(write-char #\Newline t) ;;is identical to (terpri t)
(print (machine-version) t)
(print (lisp-implementation-type) t)
(print (machine-type) t)
(print (lisp-implementation-version) t)
(terpri t)
(progn (terpri t) (run-program "/usr/bin/date" '() :output t) (values))
