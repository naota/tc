;;; tc-is18.el --- incremental search with T-Code on NEmacs

;; $Id: tc-is18.el,v 1.14 2003/05/18 09:03:58 kitajima Exp $

;; following codes are stolen from  skk-isearch.el

;; original-file: Incremental search for SKK (version 1.9 of April 21, 1991)
;; Masahiko Sato modified the original incremental search.
;; Tsugutomo Enami (enami@ptgd.sony.co.jp) contributed modifications.

;; Modification for tc2 by KITAJIMA Akira <kitajima@isc.osakac.ac.jp>
;; Maintainer: KITAJIMA Akira

(autoload 'tcode-following-char "tc-sysdep")
(autoload 'tcode-string-to-char "tc-sysdep")

(defvar search-tcode-char ?\C-\\
  "*Character to switch T-Code mode while in incremental search.")
(defvar search-tcode-char-prompt "T-Code "
  "*Prompt identifying T-Code isearch")

(defvar search-jwrap-char ?\C-v
  "*Character to toggle canonical mode in incremental search.")
(defvar search-jwrap-start-state 'auto
  "*インクリメンタルサーチ開始時の行ラップモードを指定する。
nil, t はそれぞれ行ラップあり、なしを表わす。
nil でも t でもなければ tcode-ready-in-this-buffer を使う(デフォルト)。")

(defvar search-string-char ?\C-k)
(defvar search-string-char-prompt "Enter string... ")

(defvar search-tcode-start-state nil
  "*インクリメンタルサーチ開始時のTコードモードを指定する。
	nil: バッファのTコードモードに同期(デフォルト)。
	t:   バッファのTコードモードと独立。開始時はバッファと同じ。
	0:   バッファと独立に常に非Tコードサーチから開始。
	1:   バッファと独立に常にTコードサーチから開始。")
(make-variable-buffer-local 'search-tcode-start-state)
(setq-default search-tcode-start-state nil)

(defvar j-ignore-exp "[\n\t <>|]*"
  "*If non-nil, it is inserted between consecutive JIS characters in the search string.")

(defvar j-ascii-ignore-exp "[\n\t <>|]*"
  "*If non-nil, it is inserted between an ASCII character and a space charcter.
The space character will be deleted.")

(defvar tcode-isearch-special-function-alist
  '((tcode-bushu-begin-conversion . tcode-isearch-bushu-conversion-command)
    (tcode-bushu-begin-alternate-conversion
     . tcode-isearch-bushu-alternate-conversion-command)
    (tcode-mazegaki-begin-alternate-conversion . tcode-isearch-prefix-mazegaki)
    (tcode-mazegaki-begin-conversion . tcode-isearch-postfix-mazegaki)
    (tcode-toggle-alnum-mode))
  "*isearch中での特殊なコマンドの入力に対する代替コマンドの alist。")

(or (fboundp 'tcode-on-p)
    (defun tcode-on-p ()))		; dummy function

(defun j-add-ignore-exp (str)
  "Expand STR by inserting j-ignore-str between JIS characters."
  (save-excursion
    (set-buffer (get-buffer-create " *search-work-buff*"))
    (erase-buffer)
    (insert str)
    (goto-char (point-min))
    (or (eobp)
	(forward-char 1)
	(while (not (eobp))
	  (if (< (preceding-char) 128)
	      (if (and (= (following-char) ? ) j-ascii-ignore-exp)
		  ;; if j-ascii-ignore-exp is non-nil (and in such a case
		  ;; it must be a string for a regular expression) and
		  ;; if the preceding char is an ascii char and the following
		  ;; char is a space then replace it by j-ascii-ignore-exp.
		  (progn
		    (insert j-ascii-ignore-exp)
		    (delete-char 1))
		;; otherwise go forward
		(forward-char 1))
	    (if (>= (following-char) 128)
		;; if the point is between two JIS characters, insert
		;; j-ignore-exp.
		(progn
		  (insert j-ignore-exp)
		  (forward-char 1))
	      (forward-char 1)))))
    ;; it is necessary to enclose the string by a pair of parentheses
    ;; to cope with a bug in Nemacs' regexp search
    (concat "\\(" (buffer-substring (point-min) (point-max)) "\\)")
    ))


;;
;; following codes are stolen from  isearch.el
;;

;;; 88.5.29  modified for Nemacs Ver.2.1 by K.Handa
;;; 90.9.12  modified for T-Code by K.Maeda
;;; 91.4.23  debug and new feature by K.Maeda

;; Incremental search
;; Copyright (C) 1985, 1986 Free Software Foundation, Inc.

(defun isearch (forward &optional regexp)
  (let ((search-string "")
	(search-message "")
	(cmds nil)
	(success t)
	(wrapped nil)
	(barrier (point))
	adjusted
	(invalid-regexp nil)
	(slow-terminal-mode (and (<= (baud-rate) search-slow-speed)
				 (> (window-height)
				    (* 4 search-slow-window-lines))))
	(other-end nil)			;Start of last match if fwd, end if backwd.
	(small-window nil)		;if t, using a small window
	(found-point nil)		;to restore point from a small window
	;; This is the window-start value found by the search.
	(found-start nil)
	;; patch by K.Maeda 91.4.23
	(tcode-state (if (numberp search-tcode-start-state)
			 (if (zerop search-tcode-start-state) nil t)
		       (and (boundp 'tcode-on-p)
			    tcode-on-p)))
	(j-wrap (and search-jwrap-start-state
		     (if (eq t search-jwrap-start-state)
			 t
		       (and (boundp 'tcode-ready-in-this-buffer)
			    tcode-ready-in-this-buffer))))
	;; end of patch
	(opoint (point))
	(inhibit-quit t))		;Prevent ^G from quitting immediately.
    (isearch-push-state)
    (save-window-excursion
      (catch 'search-done
	(while t
	  (or (>= unread-command-char 0)
	      (progn
		(or (input-pending-p)
		    (isearch-message))
		(if (and slow-terminal-mode
			 (not (or small-window (pos-visible-in-window-p))))
		    (progn
		      (setq small-window t)
		      (setq found-point (point))
		      (move-to-window-line 0)
		      (let ((window-min-height 1))
			(split-window nil (if (< search-slow-window-lines 0)
					      (1+ (- search-slow-window-lines))
					    (- (window-height)
					       (1+ search-slow-window-lines)))))
		      (if (< search-slow-window-lines 0)
			  (progn (vertical-motion (- 1 search-slow-window-lines))
				 (set-window-start (next-window) (point))
				 (set-window-hscroll (next-window)
						     (window-hscroll))
				 (set-window-hscroll (selected-window) 0))
			(other-window 1))
		      (goto-char found-point)))))
	  (let ((char (if quit-flag
			  ?\C-g
			(read-char)))
		ignore)
	    (setq quit-flag nil adjusted nil)
	    ;; Meta character means exit search.
	    (cond ((and (>= char 128)
			search-exit-option)
		   (setq unread-command-char char)
		   (throw 'search-done t))
		  ((eq char search-exit-char)
		   ;; Esc means exit search normally.
		   ;; Except, if first thing typed, it means do nonincremental
		   (if (= 0 (length search-string))
		       (nonincremental-search forward regexp))
		   (throw 'search-done t))
		  ((= char ?\C-g)
		   ;; ^G means the user tried to quit.
		   (ding)
		   (discard-input)
		   (if success
		       ;; If search is successful, move back to starting point
		       ;; and really do quit.
		       (progn (goto-char opoint)
			      (signal 'quit nil))
		     ;; If search is failing, rub out until it is once more
		     ;;  successful.
		     (while (not success) (isearch-pop))))
		  ((or (eq char search-repeat-char)
		       (eq char search-reverse-char))
		   (if (eq forward (eq char search-repeat-char))
		       ;; C-s in forward or C-r in reverse.
		       (if (equal search-string "")
			   ;; If search string is empty, use last one.
			   (setq search-string
				 (if regexp
				     search-last-regexp search-last-string)
				 search-message
				 (mapconcat 'text-char-description
					   ;;; original line is
					   ;;; "
					   ;;; search-string ""))
					   ;;; "
					   ;;; and new lines are
					    (kanji-word-list search-string)
					    nil))
			                   ;;; end of patch
			   ;; If already have what to search for, repeat it.
			   (or success
			       (progn (goto-char (if forward (point-min) (point-max)))
				      (setq wrapped t))))
		       ;; C-s in reverse or C-r in forward, change direction.
		       (setq forward (not forward)))
		   (setq barrier (point)) ; For subsequent \| if regexp.
		   (setq success t)
		   (or (equal search-string "")
		       (isearch-search))
		   (isearch-push-state))
		  ((= char search-delete-char)
		   ;; Rubout means discard last input item and move point
		   ;; back.  If buffer is empty, just beep.
		   (if (null (cdr cmds))
		       (ding)
		     (isearch-pop)))
		  ;; patch by K.Maeda 90.9.12
		  ;; 91.4.23 hack tcode-state
		  ((eq char search-tcode-char)
		   (require 'tc)
		   (if search-tcode-start-state
		       (setq tcode-state (not tcode-state))
		     (or tcode-ready-in-this-buffer
			 (setq j-wrap t))
		     (toggle-input-method)))
		  ((eq char search-jwrap-char)
		   (setq j-wrap (not j-wrap))
		   (and (not success) j-wrap
			(progn
			  (setq success t)
			  (goto-char (if forward other-end
				       (min opoint barrier other-end)))
			  (isearch-search))))
		  ;; end of patch
		  (t
		   (cond ((or (eq char search-yank-word-char)
			      (eq char search-yank-line-char))
			  ;; ^W means gobble next word from buffer.
			  ;; ^Y means gobble rest of line from buffer.
			  (let ((word (save-excursion
					(and (not forward) other-end
					     (goto-char other-end))
					(buffer-substring
					 (point)
					 (save-excursion
					   (if (eq char search-yank-line-char)
					       (end-of-line)
					     (if (> (tcode-following-char)
						    256)
						 (forward-char 1)
					       (forward-word 1)))
					   (point))))))
			    (setq search-string (concat search-string word)
				  search-message
				  (concat search-message
					  (mapconcat 'text-char-description
					   ;;; original line is
					   ;;; "
					   ;;;	      word "")))))
					   ;;; "
					   ;;; and new lines are
						     (kanji-word-list word)
						     nil)))))
			                   ;;; end of patch
 			 ;;; patch by K.Handa 88.5.27
			 ((eq char search-string-char)
			  (let ((str (save-excursion
				       (read-string
					(concat search-string-char-prompt
						current-isearch-message)))))
			    (setq search-string (concat search-string str)
				  search-message
				  (concat search-message str))))
			 ;;; end of patch
			 ;; Any other control char =>
			 ;;  unread it and exit the search normally.
			 ((and search-exit-option
			       (/= char search-quote-char)
			       (or (= char ?\177)
				   (and (< char ? ) (/= char ?\t) (/= char ?\r))))
			  (setq unread-command-char char)
			  (throw 'search-done t))
			 (t
			  ;; Any other character => add it to the
			  ;;  search string and search.
			  (cond ((= char search-quote-char)
				 (setq char (read-quoted-char
					     (isearch-message t))))
				((= char ?\r)
				 ;; unix braindeath
				 (setq char ?\n)))
			  ;;; patch by K.Maeda 90.9.12
			  ;;; original lines "
			  ;;;(setq search-string (concat search-string
			  ;;;			      (char-to-string char))
			  ;;;	search-message (concat search-message
			  ;;;			       (text-char-description char)))))
			  ;;; "
			  ;;; 91.4.23, hack tcode-state
			  ;;; 91.5.30, hack -1..-3 codes for tcode-keymap-table.
			  (let ((search-added-string (char-to-string char))
				(search-desc-string (text-char-description char)))
			    (if (if search-tcode-start-state tcode-state (tcode-on-p))
				(let ((k1 (tcode-char-to-key char))
				      (c1 char) k2 c2)
				  (cond
				   ((= k1 -1))
				   ((= k1 -2)
				    (setq search-added-string
					  (downcase search-added-string))
				    (setq search-desc-string
					  (downcase search-desc-string)))
				   ((= k1 -3))
				   ((< k1 -3)
				    (setq search-added-string (char-to-string (- k2)))
				    (setq search-desc-string
					  (text-char-description (- k2))))
				   (t
				    (let* ((decoded (tcode-decode-chars c1))
					   (action (car decoded))
					   (prev (tcode-isearch-bushu)))
      (cond ((null action)
	     (ding)
	     (setq search-added-string ""
		   search-desc-string ""
		   ignore t))
	    ((integerp action)
	     (tcode-isearch-process-string 
	      (char-to-string (car (tcode-apply-filters (list action))))
	      prev))
	    ((stringp action)
	     (setq action 
		   (mapconcat 'char-to-string
			      (tcode-apply-filters 
			       (kanji-word-list action))
			      nil))
	     (tcode-isearch-process-string action prev))
	    ((and (not (tcode-function-p action))
		  (consp action))
	       (tcode-isearch-process-string
		(mapconcat 'char-to-string
			   (tcode-apply-filters
			    (tcode-apply-filters
			     (mapcar 'tcode-string-to-char
				     (delq nil action))))
			   nil)
		prev))
	    ((tcode-function-p action)
	     (let ((func (assq action
			       tcode-isearch-special-function-alist)))
	       (if (null func)
		   (let ((str (mapconcat 'char-to-string (cdr decoded) nil)))
		     (setq search-added-string str
			   search-desc-string str))
		 (setq search-added-string ""
		       search-desc-string ""
		       ignore t)
		 (funcall (or (cdr func)
			      action)))))
	    (t
	     (setq search-added-string ""
		   search-desc-string ""
		   ignore t))))))))
			    (setq search-string (concat search-string search-added-string)
				  search-message (concat search-message search-desc-string)))))
			  ;;; end of patch
		   (if (and (not success)
			    ;; unsuccessful regexp search may become
			    ;;  successful by addition of characters which
			    ;;  make search-string valid
			    (not regexp))
		       nil
		     ;; If a regexp search may have been made more
		     ;; liberal, retreat the search start.
		     ;; Go back to place last successful search started
		     ;; or to the last ^S/^R (barrier), whichever is nearer.
		     (and regexp success cmds
			  (cond ((memq char '(?* ??))
				 (setq adjusted t)
				 (let ((cs (nth (if forward
						    5 ; other-end
						  2) ; saved (point)
						(car (cdr cmds)))))
				   ;; (car cmds) is after last search;
				   ;; (car (cdr cmds)) is from before it.
				   (setq cs (or cs barrier))
				   (goto-char
				    (if forward
					(max cs barrier)
				      (min cs barrier)))))
				((eq char ?\|)
				 (setq adjusted t)
				 (goto-char barrier))))
		     ;; In reverse regexp search, adding a character at
		     ;; the end may cause zero or many more chars to be
		     ;; matched, in the string following point.
		     ;; Allow all those possibiities without moving point as
		     ;; long as the match does not extend past search origin.
		     (if (and regexp (not forward) (not adjusted)
			      (condition-case ()
				  (looking-at search-string)
				(error nil))
			      (<= (match-end 0) (min opoint barrier)))
			 (setq success t invalid-regexp nil
			       other-end (match-end 0))
		       ;; Not regexp, not reverse, or no match at point.
		       (if (and other-end (not adjusted))
			   (goto-char (if forward other-end
					(min opoint barrier (1+ other-end)))))
		       (isearch-search)))
		   (or ignore
		       (isearch-push-state)))))))
      (setq found-start (window-start (selected-window)))
      (setq found-point (point)))
    (if (> (length search-string) 0)
	(if regexp
	    (setq search-last-regexp search-string)
	  (setq search-last-string search-string)))
    ;; If there was movement, mark the starting position.
    ;; Maybe should test difference between and set mark iff > threshold.
    ;; 91.4.23 debugged by info on gnu.emacs.bugs (point) -> found-point; kaoru.
    (if (/= found-point opoint)
	(push-mark opoint)
      (message ""))
    (if small-window
	(goto-char found-point)
      ;; Exiting the save-window-excursion clobbers this; restore it.
      (set-window-start (selected-window) found-start t))))

(defun tcode-isearch-bushu ()
  "isearch-message中の部首合成の文字を調べる"
  (cond
   ((string-match "▲$" search-message)
    t)
   ((string-match "▲.$" search-message)
    (substring search-message (string-match ".$" search-message)))
   (t
    nil)))

(defun tcode-isearch-start-bushu ()
  "Tコードインクリメンタルサーチ中の prefix 部首変換を始める。"
  (tcode-bushu-init 2)
  (setq search-added-string ""
	search-desc-string "▲"))

(defun tcode-isearch-process-string (str prev)
  "文字 STR を検索文字列に加えて検索する。
PREV と合成できるときはその合成した文字で検索する。"
  (if (not (stringp prev))
      (setq search-added-string str
	    search-desc-string str)
    (let ((s (tcode-bushu-compose-two-chars (tcode-string-to-char prev)
					    (tcode-string-to-char str))))
      (if s
	  (progn
	    (let ((msg (car (cdr (car cmds)))))
	      (while (and msg (string= msg (car (cdr (car cmds)))))
		(isearch-pop)))
	    (let ((msg (car (car cmds))))
	      (while (and msg (string= msg (car (cdr (car cmds)))))
		(isearch-pop)))
	    (setq search-added-string (char-to-string s)
		  search-desc-string search-added-string))
	(ding)
	(setq search-added-string ""
	      search-desc-string ""
	      ignore t)))))


(defun tcode-isearch-bushu-alternate-conversion-command ()
  "isearch中で通常とは逆の型の部首合成変換を始める。"
  (interactive)
  (if tcode-use-postfix-bushu-as-default
      (tcode-isearch-start-bushu)
    (tcode-isearch-postfix-bushu)))

(defun tcode-isearch-bushu-conversion-command ()
  "isearch中で部首合成変換を始める。"
  (interactive)
  (if (not tcode-use-postfix-bushu-as-default)
      (tcode-isearch-start-bushu)
    (tcode-isearch-postfix-bushu)))

(defun tcode-isearch-postfix-bushu ()
  "Tコードインクリメンタルサーチ中の  postfix 部首変換。"
  (let ((p1 (string-match "..$" search-message))
	(p2 (string-match ".$"  search-message)))
    (if (null p1)
	(progn
	  (beep)
	  (setq search-added-string ""
		search-desc-string ""
		ignore t))
      (let ((k1 (substring search-message p1 p2))
	    (k2 (substring search-message p2)))
	(tcode-bushu-init 2)
	(if (setq k1 (tcode-bushu-compose-two-chars (tcode-string-to-char k1)
						    (tcode-string-to-char k2)))
	    (progn
	      (let ((msg (car (car cmds))))
		(while (and msg (string= msg (car (car cmds))))
		  (isearch-pop)))
	      (let ((msg (car (car cmds))))
		(while (and msg (string= msg (car (car cmds))))
		  (isearch-pop)))
	      (setq search-added-string (char-to-string k1)
		    search-desc-string search-added-string
		    ignore nil))
	  (beep)
	  (setq search-added-string ""
		search-desc-string ""
		ignore t))))))

(defun isearch-message (&optional c-q-hack ellipsis)
  ;; If about to search, and previous search regexp was invalid,
  ;; check that it still is.  If it is valid now,
  ;; let the message we display while searching say that it is valid.
  (and invalid-regexp ellipsis
       (condition-case ()
	   (progn (re-search-forward search-string (point) t)
		  (setq invalid-regexp nil))
	 (error nil)))
  ;; If currently failing, display no ellipsis.
  (or success (setq ellipsis nil))
  (let ((m (concat (if success "" "failing ")
		   (if wrapped "wrapped ")
		   (if regexp "regexp " "")
		   ;;; patch by K.Maeda 90.9.12
		   ;;; 91.4.23 hack tcode-state
		   (if (if search-tcode-start-state tcode-state (tcode-on-p))
		       search-tcode-char-prompt)
		   (if j-wrap "W-" "")
		   ;;; end of patch
		   "I-search"
		   (if forward ": " " backward: ")
		   search-message
		   (if c-q-hack "^Q" "")
		   (if invalid-regexp
		       (concat " [" invalid-regexp "]")
		     ""))))
    (aset m 0 (upcase (aref m 0)))
    ;;; patch by K.Handa 88.5.28
    (setq current-isearch-message m)
    ;;; end of patch
    (let ((cursor-in-echo-area ellipsis))
      (if c-q-hack m (message "%s" m)))))

(defun isearch-search ()
  (isearch-message nil t)
  (condition-case lossage
      (let ((inhibit-quit nil))
	(if regexp (setq invalid-regexp nil))
	(setq success
	      ;; patch for j-ignore-exp ---from skk-isearch.el
	      ;; 91.4.23 K.Maeda
	      ;;--original lines:
	      ;;(funcall
	      ;; (if regexp
	      ;;   (if forward 're-search-forward 're-search-backward)
	      ;; (if forward 'search-forward 'search-backward))
	      ;; search-string nil t)
	      (if j-wrap
		  (funcall
		   (if forward 're-search-forward 're-search-backward)
		   (if regexp
		       search-string
		     (j-add-ignore-exp (regexp-quote search-string)))
		   nil t)
		(funcall
		 (if regexp
		     (if forward 're-search-forward 're-search-backward)
		   (if forward 'search-forward 'search-backward))
		 search-string nil t)))
	      ;;; end of patch
	(if success
	    (setq other-end
		  (if forward (match-beginning 0) (match-end 0)))))
    (quit (setq unread-command-char ?\C-g)
	  (setq success nil))
    (invalid-regexp (setq invalid-regexp (car (cdr lossage)))
		    (if (string-match "\\`Premature \\|\\`Unmatched \\|\\`Invalid "
				      invalid-regexp)
			(setq invalid-regexp "incomplete input"))))
  (if success
      nil
    ;; Ding if failed this time after succeeding last time.
    (and (nth 3 (car cmds))
	 (ding))
    (goto-char (nth 2 (car cmds)))))


(defun tcode-isearch-read-string ()
  "インクリメンタルサーチ中に文字列を読み込む。"
  (let* ((string (read-string (concat "Isearch read: " current-isearch-message))))
    (unless (string= string "")
      (tcode-isearch-process-string string nil))))


(defun tcode-isearch-postfix-mazegaki ()
  "インクリメンタルサーチ中に後置型の交ぜ書き変換を行う。"
  (let ((orig-isearch-cmds cmds))
    (unwind-protect
	(let ((current-string current-isearch-message))
	  ;; clear isearch states
	  (while (cdr cmds)
	    (isearch-pop-state))
	  (let* ((string (read-string "Isearch read: "
				      current-string nil nil t)))
	    (unless (string= string "")
	      (tcode-isearch-process-string string nil))))
      (setq isearch-cmds orig-isearch-cmds)
      (isearch-top-state))))

(provide 'tc-is18)

;;; tc-is18.el ends here
