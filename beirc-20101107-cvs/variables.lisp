(in-package :beirc)

(defvar *hyperspec-base-url* "file:///mnt/hd/HyperSpec/")
(defparameter *default-fill-column* 120)
(defvar *timestamp-column-orientation* :right)
(defvar *default-nick* (format nil "wbooze"))
(defvar *default-realname* NIL
  "Either a string or NIL.")
(defvar *default-web-browser* #+darwin "/usr/bin/open"
                              ;; assuming a debian system running X:
	#+linux  "/usr/bin/x-www-browser")
;;(defvar *default-sound-player* 
;;    (or nil
;;	#+linux "/usr/bin/ogg123 -")
;;  "An external program that can be used to produce sounds.  
;;You should set this to be a program that will read from
;;its standard input and produce sounds.  See the example
;;value, which is ogg123, configured to read its input from
;;stdin, instead of from a file.")
(defvar *default-sound-player* nil)

(defvar *sound-for-my-nick* nil
  "If the NOISEMAKER post-message-hook is enabled, and there
is a *default-sound-player* defined, this noise will be 
played when your nick is mentioned.")

(defvar *auto-join-alist* nil)

;;(defvar *auto-join-alist* '(("irc.freeenode.net" . ("#lisp" "#scheme" "#emacs" "#lispgames" "#lispcafe" "#sbcl" "#algorithms" "##math"))))

;;(defvar *auto-join-alist* 
;;  "An alist mapping irc server name to a list of channels to
;;  automatically join on connect. Each element should have this
;;  format:
;; (\"server-name\" . (\"#channel-name\" \"#channel2\" \"#channel3\"))"
;;  '(("irc.freenode.net" . ("##astronomy" "##workingset" "##networking" "#physics" "##asm" "#kernel" "#maxima" "#math" "#per;;l" "#awk" "##sed" "#bash" "#kde-devel" "##java" "##c++" "##c" "#haskell" "#sbcl" "#emacs" "#lispgames" "#lisp" "#latex" "#scheme" "##linux" "##slackware"))))

(defvar *auto-connect-list* '("irc.freenode.net")
  "A list of servers (strings) specifying servers to which 
beirc should automatically connect on startup."
)

(defvar *auto-identify-list* '("irc.freenode.net")
  "A list of servers for which BEIRC should automatically execute 
the identify command on connection.")


(defvar *nickserv-password-alist* '(("irc.freenode.net" . "mlpdfg787"))
  "Default password to send to the NickServ authentication bot")


(defvar *beirc-user-init-file* (merge-pathnames (make-pathname :name ".beirc" :type "lisp")
                                                (user-homedir-pathname)))

(defvar *auto-close-inactive-query-windows-p* nil
  "Indicates whether beirc automatically closes query windows
that were inactive for longer than *max-query-inactive-time*
seconds. If set to NIL, beirc doesn't automaticaly close query
windows. Closing inactive query windows is still available via
/Close Inactive Queries.")

(defvar *max-query-inactive-time* 600
  "Longest time an inactive query window will be kept around by
the command /Close Inactive Queries and the automatic query
window closing mechanism (see
*auto-close-inactive-query-windows-p*).")

(defvar *meme-log-bot-nick* "cmeme"
  "The name of the meme channel log bot")

(defvar *filter-colors* nil
  "If set to non-NIL, filter color, bold, inverse and underline
codes from IRC messages.")

(defvar *auto-focused-alist* nil
  "An alist mapping channels to nicks which will be focused by
default. Each element should have the following format:
(\"#channel-name\" . (\"nick1\" \"nick2\"))")
