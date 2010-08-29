;;; init.el --- init routine of skkinput3 for tc2

;;; $Id: init.el,v 1.3 2003/02/01 11:06:17 kitajima Exp $

;;; skkinput3.0.6のskk10/init.elを変更。

;; この init.el は emacs-20.7 に含まれる emacs-lisp 群及び skk10.62a
;; から抜粋、利用している部分があります。
;; これらの copyright は emacs, emacs-lisp 作者及び skk 作者にあります。

(setq global-map '(keymap
 [set-mark-command		beginning-of-line	backward-char
  mode-specific-command-prefix	delete-char		end-of-line
  forward-char			keyboard-quit		delete-backward-char
  indent-for-tab-command	newline-and-indent	kill-line
  recenter			newline-and-indent	next-line
  open-line			previous-line		quoted-insert
  isearch-backward		isearch-forward		transpose-chars
  universal-argument		scroll-up		kill-region
  Control-X-prefix		yank			scroll-down
  ESC-prefix			toggle-input-method	abort-recursive-edit
  nil				undo			self-insert-command
  self-insert-command		self-insert-command	self-insert-command
  self-insert-command		self-insert-command	self-insert-command
  self-insert-command		self-insert-command	self-insert-command
  self-insert-command		self-insert-command	self-insert-command
  self-insert-command		self-insert-command	self-insert-command
  self-insert-command		self-insert-command	self-insert-command
  self-insert-command		self-insert-command	self-insert-command
  self-insert-command		self-insert-command	self-insert-command
  self-insert-command		self-insert-command	self-insert-command
  self-insert-command		self-insert-command	self-insert-command
  self-insert-command		self-insert-command	self-insert-command
  self-insert-command		self-insert-command	self-insert-command
  self-insert-command		self-insert-command	self-insert-command
  self-insert-command		self-insert-command	self-insert-command
  self-insert-command		self-insert-command	self-insert-command
  self-insert-command		self-insert-command	self-insert-command
  self-insert-command		self-insert-command	self-insert-command
  self-insert-command		self-insert-command	self-insert-command
  self-insert-command		self-insert-command	self-insert-command
  self-insert-command		self-insert-command	self-insert-command
  self-insert-command		self-insert-command	self-insert-command
  self-insert-command		self-insert-command	self-insert-command
  self-insert-command		self-insert-command	self-insert-command
  self-insert-command		self-insert-command	self-insert-command
  self-insert-command		self-insert-command	self-insert-command
  self-insert-command		self-insert-command	self-insert-command
  self-insert-command		self-insert-command	self-insert-command
  self-insert-command		self-insert-command	self-insert-command
  self-insert-command		self-insert-command	self-insert-command
  self-insert-command		self-insert-command	self-insert-command
  self-insert-command		self-insert-command	self-insert-command
  self-insert-command		delete-backward-char]))

(setq im-local-map 
      '(keymap (0 . im-key-filter) (1 . im-key-filter) (2 . im-key-filter)
	       (3 . im-key-filter) (4 . im-key-filter) (5 . im-key-filter)
	       (6 . im-key-filter) (7 . im-key-filter) (8 . im-key-filter)
	       (9 . im-key-filter) (10 . im-key-filter) (11 . im-key-filter) 
	       (12 . im-key-filter) (13 . im-newline) (14 . im-key-filter)
	       (15 . im-key-filter) (16 . im-key-filter) (17 . im-key-filter)
	       (18 . im-key-filter) (19 . im-key-filter) (20 . im-key-filter)
	       (21 . im-key-filter) (22 . im-key-filter) (23 . im-key-filter)
	       (24 . im-key-filter) (25 . im-key-filter) (26 . im-key-filter)
	       (27 . im-key-filter) (28 . im-end-conversion) (29 . im-key-filter)
	       (30 . im-key-filter) (31 . im-key-filter) (127 . im-key-filter)
	       (453050400 . im-end-conversion) (453509216 . im-end-conversion)
	       (t . im-key-notify)))
;(setq im-local-map nil)

(setq minibuffer-local-map 
      '(keymap (13 . exit-minibuffer) (7 . abort-recursive-edit)))

(setq minibuffer-local-completion-map 
      '(keymap (13 . exit-minibuffer) (7 . abort-recursive-edit)))

(setq minibuffer-local-ns-map
      '(keymap (63 . self-insert-and-exit) (9 . exit-minibuffer) (32 . exit-minibuffer) (10 . skk-kakutei) (13 . exit-minibuffer) (7 . abort-recursive-edit)))

(setq mode-line-format '("" "%-"))
;(setq mode-line-format nil)
(setq overwrite-mode nil)
(setq auto-fill-function nil)
(setq unread-command-char -1)
(setq system-type 'unix)
(setq kill-ring nil)
(setq kill-ring-yank-pointer nil)
(setq minor-mode-alist nil)
(setq overriding-local-map nil)
(setq temporary-file-directory "/tmp/")
(setq skk-emacs-type 'mule4)

(defmacro when (cond &rest body)
  (list 'if cond (cons 'progn body)))

(defmacro unless (cond &rest body)
  (cons 'if (cons cond (cons nil body))))

(defvar save-match-data-internal)
(defmacro save-match-data (&rest body)
  (` (let ((save-match-data-internal (match-data)))
       (unwind-protect
	   (progn (,@ body))
	 (set-match-data save-match-data-internal)))))

(defun require (FEATURE &optional FILENAME NOERR)
  (if (not (featurep FEATURE))
      (progn
	(if FILENAME
	    (load FILENAME)
	  (load (symbol-name FEATURE))))
    t))

(defun keyboard-quit ()
  (interactive)
  (signal 'quit nil))

(defvar key-substitution-in-progress nil
 "Used internally by substitute-key-definition.")

(defun substitute-key-definition (olddef newdef keymap &optional oldmap prefix)
  (or prefix (setq prefix ""))
  (let* ((scan (or oldmap keymap))
	 (vec1 (vector nil))
	 (prefix1 (vconcat prefix vec1))
	 (key-substitution-in-progress
	  (cons scan key-substitution-in-progress)))
    ;; Scan OLDMAP, finding each char or event-symbol that
    ;; has any definition, and act on it with hack-key.
    (while (consp scan)
      (if (consp (car scan))
	  (let ((char (car (car scan)))
		(defn (cdr (car scan))))
	    ;; The inside of this let duplicates exactly
	    ;; the inside of the following let that handles array elements.
	    (aset vec1 0 char)
	    (aset prefix1 (length prefix) char)
	    (let (inner-def skipped)
	      ;; Skip past menu-prompt.
	      (while (stringp (car-safe defn))
		(setq skipped (cons (car defn) skipped))
		(setq defn (cdr defn)))
	      ;; Skip past cached key-equivalence data for menu items.
	      (and (consp defn) (consp (car defn))
		   (setq defn (cdr defn)))
	      (setq inner-def defn)
	      ;; Look past a symbol that names a keymap.
	      (while (and (symbolp inner-def)
			  (fboundp inner-def))
		(setq inner-def (symbol-function inner-def)))
	      (if (or (eq defn olddef)
		      ;; Compare with equal if definition is a key sequence.
		      ;; That is useful for operating on function-key-map.
		      (and (or (stringp defn) (vectorp defn))
			   (equal defn olddef)))
		    (define-key keymap prefix1 (nconc (nreverse skipped) newdef))
		(if (and (keymapp defn)
			 ;; Avoid recursively scanning
			 ;; where KEYMAP does not have a submap.
			 (let ((elt (lookup-key keymap prefix1)))
			   (or (null elt)
			       (keymapp elt)))
			 ;; Avoid recursively rescanning keymap being scanned.
			 (not (memq inner-def
				    key-substitution-in-progress)))
		    ;; If this one isn't being scanned already,
		    ;; scan it now.
		    (substitute-key-definition olddef newdef keymap
					       inner-def
					       prefix1)))))
	(if (vectorp (car scan))
	    (let* ((array (car scan))
		   (len (length array))
		   (i 0))
	      (while (< i len)
		(let ((char i) (defn (aref array i)))
		  ;; The inside of this let duplicates exactly
		  ;; the inside of the previous let.
		  (aset vec1 0 char)
		  (aset prefix1 (length prefix) char)
		  (let (inner-def skipped)
		    ;; Skip past menu-prompt.
		    (while (stringp (car-safe defn))
		      (setq skipped (cons (car defn) skipped))
		      (setq defn (cdr defn)))
		    (and (consp defn) (consp (car defn))
			 (setq defn (cdr defn)))
		    (setq inner-def defn)
		    (while (and (symbolp inner-def)
				(fboundp inner-def))
		      (setq inner-def (symbol-function inner-def)))
		    (if (or (eq defn olddef)
			    (and (or (stringp defn) (vectorp defn))
				 (equal defn olddef)))
			(define-key keymap prefix1
			  (nconc (nreverse skipped) newdef))
		      (if (and (keymapp defn)
			       (let ((elt (lookup-key keymap prefix1)))
				 (or (null elt)
				     (keymapp elt)))
			       (not (memq inner-def
					  key-substitution-in-progress)))
			  (substitute-key-definition olddef newdef keymap
						     inner-def
						     prefix1)))))
		(setq i (1+ i))))))
      (setq scan (cdr scan)))))

(defmacro insert-and-inherit(str)
  (list 'insert str))

(defmacro buffer-substring-no-properties(START END)
  (list 'buffer-substring START END))

(defmacro with-current-buffer (buffer &rest body)
  (` (save-current-buffer
       (set-buffer (, buffer))
       (,@ body))))

(defmacro ad-real-fset(SYMBOL DEFINITION)
  (list 'fset SYMBOL DEFINITION))

(defmacro ad-real-documentation(FUNCTION &optional RAW)
  (list 'documentation FUNCTION RAW))

(defun split-string (string &optional separators)
  (let ((rexp (or separators "[ \f\t\n\r\v]+"))
	(start 0)
	notfirst
	(list nil))
    (while (and (string-match rexp string
			      (if (and notfirst
				       (= start (match-beginning 0))
				       (< start (length string)))
				  (1+ start) start))
		(< (match-beginning 0) (length string)))
      (setq notfirst t)
      (or (eq (match-beginning 0) 0)
	  (and (eq (match-beginning 0) (match-end 0))
	       (eq (match-beginning 0) start))
	  (setq list
		(cons (substring string start (match-beginning 0))
		      list)))
      (setq start (match-end 0)))
    (or (eq start (length string))
	(setq list
	      (cons (substring string start)
		    list)))
    (nreverse list)))

(defun cancel-undo-boundary()
  nil)

(defun remove-hook (hook function &optional local)
  (if (or (not (boundp hook))		;unbound symbol, or
	  (not (default-boundp hook))
	  (null (symbol-value hook))	;value is nil, or
	  (null function))		;function is nil, then
      nil				;Do nothing.
    (if (or local
	    ;; Detect the case where make-local-variable was used on a hook
	    ;; and do what we used to do.
	    (and (local-variable-p hook)
		  (consp (symbol-value hook))
		  (not (memq t (symbol-value hook)))))
	(let ((hook-value (symbol-value hook)))
	  (if (consp hook-value)
	      (if (member function hook-value)
		  (setq hook-value (delete function (copy-sequence hook-value))))
	    (if (equal hook-value function)
		(setq hook-value nil)))
	  (set hook hook-value))
      (let ((hook-value (default-value hook)))
	(if (and (consp hook-value) (not (functionp hook-value)))
	    (if (member function hook-value)
		(setq hook-value (delete function (copy-sequence hook-value))))
	  (if (equal hook-value function)
	      (setq hook-value nil)))
	(set-default hook hook-value)))))

(setq custom-declare-variable-list nil)
(setq current-load-list nil)

(defun charset-list ()
  '(ascii latin-iso8859-1 latin-iso8859-2 latin-iso8859-3 
	  iso8859-4 iso8859-5 iso8859-6 iso8859-7
	  iso8859-14 iso8859-15 katakana-jisx0201 japanese-jisx0208-1978
	  japanese-jisx0208 japanese-jisx0212 chinese-gb2312
	  korean-ksc5601 chinese-cns11643-1))

(defun write-region-as-coding-system
  (coding-system start end filename &optional append visit lockname)
  (let ((coding-system-for-write coding-system))
    (write-region start end filename append visit lockname)))

(defun match-string (num &optional string)
  (if (match-beginning num)
      (if string
	  (substring string (match-beginning num) (match-end num))
	(buffer-substring (match-beginning num) (match-end num)))))

(defmacro with-temp-buffer (&rest body)
  (let ((temp-buffer (make-symbol "temp-buffer")))
    (` (let (((, temp-buffer)
	      (get-buffer-create (generate-new-buffer-name " *temp*"))))
	 (unwind-protect
	     (with-current-buffer ,temp-buffer
	       (,@ body))
	   (and (buffer-name (, temp-buffer))
		(kill-buffer (, temp-buffer))))))))

(defun caar(X)	(car (car X)))
(defun cdar(X)	(cdr (car X)))
(defun cadr(X)	(car (cdr X)))
(defun cddr(X)	(cdr (cdr X)))

;;; loading t-code modules.
(load "load-path.el")
(load "tc-skki.el")
(load "tc.el")

;;; defined in skk.el
(defun skk-command-key-sequence (key command)
  ;; KEY から universal arguments を取り除き、COMMAND を実行するキーを返す。
  ;; `execute-extended-command' によってコマンドが実行された場合は、nil を返す。
  (while (not (or (zerop (length key))
                  (eq command (key-binding key))))
    (setq key (vconcat (cdr (append key nil)))))
  (and (not (zerop (length key))) key))

(defun im-auto-send-text ()
  (im-send-text im-client (buffer-substring (point-min) (point-max)))
  (delete-region (point-min) (point-max)))

(defun im-newline (arg)
  (interactive "p")
  (let ((str (buffer-string)))
    (if (> (length str) 0)
	(im-send-text im-client str))
    (im-send-text im-client "\n")
    (erase-buffer)))

(defun im-end-conversion (arg)
  (interactive "p")
  (im-auto-send-text)
  (run-hooks 'tcode-im-end-conversion-hook)
  (im-send-end-conversion im-client))

(defun im-key-notify (arg)
  (interactive "p")
; 文字が入っている時に効果があるようにするべきか否か。
; (if (= (point-min) (point-max)) を追加するかどうか、ですが。
  (im-send-key im-client last-command-event))

(defun im-key-filter (arg)
  (interactive "p")
  (let ((prefix-arg arg)
	(local-map (current-local-map))
	(keys (skk-command-key-sequence (this-command-keys) this-command))
	(buf (current-buffer)))
    (if (= (point-min) (point-max))
	(im-send-key im-client last-command-event)
      (if (null keys)
	  ;; no alternative commands.  may be invoked by M-x.
	  nil
	(unwind-protect
	    (progn
	      (use-local-map nil)
	      (let ((command (j-key-binding keys)))
		(if (eq command this-command)
		    ;; avoid recursive calling of j-emulate-original-map.
		    nil
		  ;; if no bindings are found, call `undefined'.  it's
		  ;; original behaviour.
		  ;; でも、condition-case で拾わなかったら、local-map
		  ;; が復元できなくて困ってしまう。
		  (setq this-command (or command (function undefined)))
		  (condition-case nil
		      (command-execute this-command)
		    (error nil)))))
	  ;; restore skk keymap.
	  (save-excursion
	    (set-buffer buf)
	    (use-local-map local-map)))))))
