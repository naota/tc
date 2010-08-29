;;; tc-util.el --- utilities for T-Code

;; Copyright (C) 1996-2001 KITAJIMA Akira

;; Author: KITAJIMA Akira <kitajima@isc.osakac.ac.jp>
;; Created: 7 May 1996
;; Version: $Id: tc-util.el,v 1.22 2003/03/03 04:04:50 kitajima Exp $
;; Keywords: wp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA.

;;; Code:

(require 'tc)
(require 'tc-sysdep)

(defgroup tcode-utility nil
  "T�����ɤ������ǽ"
  :group 'tcode)

(defun tcode-inactivate-and-self-insert (n)
  "Inactivate tcode-mode and self-insert."
  (interactive "*p")
  (if (tcode-on-p)
      (toggle-input-method))
  (self-insert-command n))

;;
;; ��ĥ���ޥ�� (moved from tc.el)
;;

;;;###autoload
(defun tcode-insert-register (reg arg)
  "`insert-register' ��Ʊ�������������ݥ���Ȥȥޡ����ΰ��֤��ա�"
  (interactive "cInsert register: \nP")
  (insert-register reg (not arg)))

;;;###autoload
(defun tcode-transpose-strokes (arg)
  "�ݥ���Ȱ��֤�ʸ���Υ��ȥ��������줫���롣"
  (interactive "*P")
  (if (not (tcode-on-p))
      (transpose-chars arg)
    (if (eolp) (tcode-forward-char -1))
    (let* ((ch (buffer-substring (point)
				 (save-excursion (tcode-forward-char 1)
						 (point))))
	   (strokes (tcode-encode (tcode-string-to-char ch))))
      (when (and (= (length strokes) 2)
		 (setq ch (tcode-action-to-printable
			   (cdr (tcode-decode (reverse strokes))))))
	(tcode-delete-char 1)
	(insert ch)))))

;;;; ����������

;;;###autoload
(defun set-tcode-mode-key (key func &optional type)
  "obsolete; use `tcode-set-key'."
  (interactive (list (progn
		       (message "T�����ɥ⡼�ɤ������Ԥ�������? ")
		       (setq key (read-char)))
		     (read-command (format "��%c�פ˳�����Ƥ륳�ޥ�ɤ�? "
					   key))
		     prefix-arg))
  (tcode-set-key key func type))

;;;; ��������ο�

(when (or (tcode-mule-2-p)
	  (tcode-mule-3-p)
	  (tcode-mule-4-p)
	  (tcode-xemacs-p))
  (or (fboundp 'set-cursor-color)
      ;; for XEmacs
      (defun set-cursor-color (color)
	(set-frame-property (selected-frame) 'cursor-color color)))

  (defcustom tcode-mode-off-cursor-color
    (and window-system
	 (or (cdr (assq 'cursor-color (frame-parameters (selected-frame))))
	     (face-background-name (get-face 'text-cursor)))) ; XEmacs
    "* T�����ɥ⡼�ɤǤʤ��Ȥ��Υ�������ο���"
    :type 'string :group 'tcode-utility)
  (defcustom tcode-mode-on-cursor-color "GreenYellow"
    "* T�����ɥ⡼�ɤΤȤ��Υ�������ο���"
    :type 'string :group 'tcode-utility)
  (defcustom tcode-katakana-mode-cursor-color "Green"
    "* T�����ɥ⡼�ɤǥ������ʥ⡼�ɤΤȤ��Υ�������ο���"
    :type 'string :group 'tcode-utility)

  (defun tcode-change-cursor-color ()
    (set-cursor-color
     (if (tcode-on-p)
	 (if tcode-katakana-mode
	     tcode-katakana-mode-cursor-color
	   tcode-mode-on-cursor-color)
       tcode-mode-off-cursor-color)))

  (defun tcode-enable-cursor-to-change-color (&optional arg)
    "�������ʤ���С���������ο����⡼�ɤˤ���Ѥ��褦�ˤ��롣
nil �Ǥʤ�����������С���������ο����⡼�ɤˤ���Ѥ��ʤ��褦�ˤ��롣"
    (interactive "P")
    (if (null arg)
	(progn
	  (add-hook 'tcode-toggle-hook 'tcode-change-cursor-color)
	  (add-hook 'post-command-hook 'tcode-change-cursor-color)
	  (add-hook 'minibuffer-setup-hook 'tcode-change-cursor-color))
      (remove-hook 'tcode-toggle-hook 'tcode-change-cursor-color)
      (remove-hook 'post-command-hook 'tcode-change-cursor-color)
      (remove-hook 'minibuffer-setup-hook 'tcode-change-cursor-color))))

;;;; �򤼽񤭼��񤫤���ܤ�����

(defvar tcode-mazegaki-delete-log-buffer "*Mazegaki Delete Log*"
  "* ������ΰ�����ɽ������Хåե�̾")

(autoload 'tcode-mazegaki-switch-to-dictionary "tc-mazegaki" nil t)

(defun tcode-mazegaki-write-to-delete-log (str)
  (save-excursion
    (set-buffer tcode-mazegaki-delete-log-buffer)
    (goto-char (point-max))
    (insert str)))

(defun tcode-mazegaki-make-entry-list (kanji)
  "������ޤ���ܤΰ�����������롣
�����������������Τ� `tcode-mazegaki-delete-kanji-from-dictionary' ��
����������ܤ�����"
  (and (string= kanji "")
       (error "Quit"))
  (let ((nod 0)
	(yomi-pattern (concat "[^ ]*" kanji))
	str)
    (save-excursion
      (get-buffer-create tcode-mazegaki-delete-log-buffer)
      (tcode-mazegaki-switch-to-dictionary)
      (goto-char (point-min))
      (message "������(%s)..." kanji)
      (while (search-forward kanji nil t)
	(beginning-of-line)
	(if (looking-at yomi-pattern)
	    (next-line 1)
	  (setq str (buffer-substring (point) (progn (next-line 1) (point))))
	  (tcode-mazegaki-write-to-delete-log str)
	  (setq nod (1+ nod))))
      (if (> nod 0)
	  (tcode-mazegaki-write-to-delete-log
	   (format "\n\t%s  %s\n" kanji (format "%d ����" nod)))
	(message "�����ʤ�"))
      (> nod 0))))

;;;###autoload
(defun tcode-mazegaki-delete-kanji-from-dictionary (kanji)
  "������ޤ���ܤ������롣
����������������Τ��ɤߤˤ��δ������ޤޤ�Ƥ��ʤ����ܤ�����"
  (interactive
   (let ((minibuffer-setup-hook
	  (unless (or (tcode-nemacs-p)
		      (tcode-mule-1-p))
	    (cons 'tcode-activate minibuffer-setup-hook))))
     (list (read-from-minibuffer "���������� "))))
  (and (or (string= kanji "")
	   (string= kanji "/"))
       (error "Quit"))
  (and (tcode-mazegaki-make-entry-list kanji)
       (if (save-excursion
	     (pop-to-buffer tcode-mazegaki-delete-log-buffer)
	     (goto-char (point-max))
	     (y-or-n-p "������ޤ���"))
	   (let ((nod 0)
		 (yomi-pattern (concat "[^ ]*" kanji))
		 (pattern (concat "/[^/\n]*" kanji "[^/\n]*/")))
	     (save-excursion
	       (tcode-mazegaki-switch-to-dictionary)
	       (goto-char (point-min))
	       (message "�����(%s)..." kanji)
	       (while (search-forward kanji nil t)
		 (beginning-of-line)
		 (if (looking-at yomi-pattern)
		     (next-line 1)
		   (narrow-to-region (point)
				     (save-excursion (next-line 1) (point)))
		   (while (re-search-forward pattern nil t)
		     (replace-match "/")
		     (backward-char)
		     (setq nod (1+ nod)))
		   (widen)
		   (if (and (= (preceding-char) ? )
			    (= (char-after (1+ (point))) ?\n))
		       (progn
			 (beginning-of-line)
			 (kill-line 1))
		     (end-of-line)
		     (forward-char))))
	       (message "�����(%s)...��λ (%s)" kanji
			(format "%d����" nod))
	       (tcode-mazegaki-write-to-delete-log
		(format "%d����\n\f\n" nod))))
	 (tcode-mazegaki-write-to-delete-log "������\n\f\n"))))

;;;; �򤼽񤭼������κ�Ĺ���ɤߤ��ĸ��Ĥ���

;;;###autoload
(defun tcode-mazegaki-get-yomi-max ()
  "������ι��ܤ�����ɤߤ���Ĺ�Τ��(����Ӥ���Ĺ��)���ĸ��Ĥ��롣
����Ĺ�����֤���"
  (interactive)
  (let ((max 0)	n
	line (l 0)
	maxstr str)
    (tcode-mazegaki-switch-to-dictionary)
    (goto-char (point-min))
    (while (not (eobp))
      (setq n (if (= (char-width (tcode-following-char)) 1)
		  1			; alphabet
		(length
		 (string-to-list
		  (setq str (buffer-substring
			     (point)
			     (prog2
				 (looking-at "^\\([^/]+\\) /")
				 (match-end 1))))))))
      (and (> n max)
	   (setq max n
		 line l
		 maxstr str))
      (forward-line 1)
      (setq l (1+ l)))
    (and (interactive-p)
	 (message "%dʸ�� (%s) %d����" max maxstr line))
    max))

;;;; ����ȥ��륭����ȼ��ʤ� T�����ɥ⡼�ɤ��ڤ��ؤ�

(defvar tcode-electric-switching-command-list
  '(self-insert-command
    egg-self-insert-command
    tcode-mazegaki-finish
    delete-backward-char
    backward-delete-char
    backward-delete-char-untabify
    tcode-insert-ya-outset
    tcode-transpose-strokes-or-chars)
  "* ľ��� `tcode-electric-space' �ǥ⡼�ɤ��ڤ��ؤ�륳�ޥ�ɤΥꥹ�ȡ�
T�����ɥ⡼�ɤΤȤ��ˡ����Υꥹ�ȤΥ��ޥ�ɤ��ƤФ줿���
`tcode-electric-space' ��¹Ԥ���� T�����ɥ⡼�ɤ��ڤ��ؤ��롣")

(defvar tcode-electric-switching-chars '(?,)
  "* `tcode-electric-space' ��ľ��˥⡼�ɤ��ڤ��ؤ��ʸ���Υꥹ�ȡ�
������������줿ʸ�����̾�Υ���ե��٥å����ϥ⡼�ɤ�
`tcode-electric-space' ��ľ������Ϥ���� T�����ɥ⡼�ɤ��ڤ��ؤ�롣")

(defvar tcode-space-chars-list '(?  ?\n ?\t)
  "* ����Ȥ��ư���ʸ���Υꥹ��")

(defvar tcode-electric-deleting-and-switching-chars '(?\t)
  "* `tcode-electric-space' ��ľ��˥⡼�ɤ��ڤ��ؤ��ʸ���Υꥹ�ȡ�
��������`tcode-electric-switching-chars' �Ȱۤʤꡢ
ľ���� `tcode-electric-space' ����������ʸ����ä���")

(defvar tcode-electric-space-without-inserting nil
  "* nil �ʤ��ڤ��ؤ���Ȥ���ʸ��(����)�����Ϥ��롣")

(defvar tcode-no-following-space-chars "({[�ơȡʡ̡ΡСҡԡ֡ءڡ�������"
  "����ʸ������Τɤ�ʸ���θ�ˤ�ľ��˶�����������ʤ���")

;;; �򤼽��Ѵ����Ѱդ���Ƥ��ʤ����ϡ�self-insert �����Ԥ�ʤ���
(or (boundp 'tcode-mazegaki-self-insert-or-convert)
    (defun tcode-mazegaki-self-insert-or-convert (arg)
      (interactive "*p")
      (self-insert-command arg)))

;;;###autoload
(defun tcode-electric-space (arg)
  "��������Ϥ��뤳�Ȥˤ�� T�����ɥ⡼�ɤ��ڤ��ؤ��롣
`tcode-electric-switching-command-list' �ˤ��륳�ޥ�ɤ��ƤФ줿ľ���
���Υ��ޥ�ɤ��ƤФ��ȡ�T�����ɥ⡼�ɤ��ڤ��ؤ��롣
�����Ǥʤ��Ȥ��ϡ�ñ�˶�����������롣"
  (interactive "p")
  (cond (buffer-read-only
	 (toggle-input-method))
	((tcode-on-p)
	 (or (tcode-mazegaki-self-insert-or-convert arg)
	     (if (memq last-command tcode-electric-switching-command-list)
		 ;; ����򤽤Τޤޤˤ����ڤ��ؤ�
		 (progn
		   (delete-backward-char 1)
		   (toggle-input-method)
		   (or tcode-electric-space-without-inserting
		       (and (not (bobp))
			    (let ((prev-char (char-to-string
					      (tcode-preceding-char))))
			      (string-match (regexp-quote prev-char)
					    tcode-no-following-space-chars)))
		       (tcode-redo-command last-command-char)))
	       (condition-case nil
		   (let* ((echo-keystrokes 0)
			  (ch (read-char)))
		     (if (memq ch tcode-electric-deleting-and-switching-chars)
			 ;; ľ���ζ����ä����ڤ��ؤ�
			 (progn
			   (delete-backward-char 1)
			   (toggle-input-method))
		       ;; �ڤ��ؤ��ʤ�
		       (tcode-redo-command ch)))
		 (t
		  (setq unread-command-events
			(nconc unread-command-events
			       (list last-input-event))))))))
	(t
	 ;; OFF ���� ON �ؤ��ڤ��ؤ�
	 (self-insert-command arg)
	 (condition-case nil
	     (let* ((echo-keystrokes 0)
		    (ch (read-char)))
	       (cond ((memq ch tcode-electric-switching-chars)
		      (and tcode-electric-space-without-inserting
			   (delete-backward-char 1))
		      (toggle-input-method))
		     ((memq ch tcode-electric-deleting-and-switching-chars)
		      (delete-backward-char 1)
		      (toggle-input-method))
		     (t
		      (tcode-redo-command ch))))
	   (t
	    (setq unread-command-events (nconc unread-command-events
					       (list last-input-event))))))))

;;;###autoload
(defun tcode-electric-comma (arg)
  "����ʤɤθ�ǡ�,�פ����Ϥ��뤳�Ȥˤ�ꡢT�����ɥ⡼�ɤ��ڤ��ؤ��롣
�ڤ��ؤ��Τϡ��� T�����ɥ⡼�ɤǡ����� `tcode-space-chars-list' ���
�����줫��ʸ����ľ��ǡ�,�פ����Ϥ����Ȥ��Τߡ�"
  (interactive "p")
  (if (and (not (tcode-on-p))
	   (or (bolp)
	       (memq (tcode-preceding-char) tcode-space-chars-list)))
      (toggle-input-method)
    (self-insert-command arg)))

;;;; �⤦��Ĥγ�������

(defvar tcode-ya-outset-map-list
  '(["��" "��" "��" "��"  "��"     "��"  "��" "��" "��" "��"

     "��" "��" "��" "��"  "��"     "��"  "��" "��" "��" "��"
     "��" "��" "��" "��"  "��"     "��"  "��" "��" "��" "��"
     "��" "��" "��" "��"  "��"     "��"  "��" "��" "��" "��"]

    ["��" "��" "��" "��"  "��"     "��"  "��" "��" "��" "��"

     "��" "��" "��" "��"  "��"     "��"  "��" "��" "��" "��"
     "��" "��" "��" "��"  "��"     "��"  "��" "��" "��" "��"
     "��" "��" "��" "��"  "��"     "��"  "��" "��" "��" "��"]

    ["��" "��" "��" "��"  "��"     "��"  "��" "��" "��" "��"

     "��" "��" "��" "��"  "��"     "��"  "��" "��" "��" "��"
     "��" "��" "��" "��"  "��"     "��"  "��" "��" "��" "��"
     "��" "��" "��" "��"  "��"     "��"  "��" "��" "��" "��"])
  "* �����ΥޥåפΥꥹ��")

;;;###autoload
(defun tcode-insert-ya-outset (level)
  "��ʸ���ɤ߹��ߡ� `tcode-ya-outset-map-list' ��ɽ�˴�Ť�ʸ�����������롣
LEVEL ���ܤ�ɽ���оݤȤʤ롣"
  (interactive "*p")
  (tcode-cancel-undo-boundary)
  (let* ((map-num (length tcode-ya-outset-map-list))
	 (map-index (1- (let ((i level))
			  (while (> i map-num)
			    (setq i (- i map-num)))
			  i)))
	 (outset-map (nth map-index tcode-ya-outset-map-list))
	 (show-table (sit-for 1)))
    (and show-table
	 (tcode-display-help-buffer
	  (tcode-draw-table outset-map (1+ map-index) map-num) t))
    (unwind-protect
	(let* ((ch (read-char))
	       (addr (tcode-char-to-key ch))
	       (elm (and (>= addr 0)
			 (< addr (length outset-map))
			 (aref outset-map addr))))
	  (cond (elm
		 (let (current-prefix-arg)
		   (tcode-insert elm)))
		((= ch last-command-char)
		 (tcode-insert-ya-outset (1+ level)))
		((= ch ? )
		 (self-insert-command level))
		(t
		 (self-insert-command level)
		 (setq prefix-arg level)
		 (tcode-redo-command ch))))
      (and show-table
	   (tcode-auto-remove-help t)))))

;;;; ʸ���ޤ��ϥ��ȥ����������ؤ�

(defvar tcode-transpose-strokes-enable-commands
  '(tcode-self-insert-command
    egg-self-insert-command
    self-insert-command
    tcode-transpose-strokes-or-chars)
  "*����ư��ǥ��ȥ����������ؤ��뤳�Ȥ��Ǥ��륳�ޥ�ɤΥꥹ�ȡ�
�����ѿ��ǻ��ꤵ�줿���ޥ�ɤ�¹Ԥ���ľ���
`tcode-transpose-strokes-or-chars' ��¹Ԥ���ȡ�
���ȥ����������ؤ��롣")

;;;###autoload
(defun tcode-transpose-strokes-or-chars (&optional arg)
  "T�����ɥ⡼�ɤΤȤ��ˤϡ��ݥ���ȤΥ��ȥ����������ؤ��롣"
  (interactive "*P")
  (if (and (not (bobp))
	   (memq last-command tcode-transpose-strokes-enable-commands)
	   (= (char-width (tcode-preceding-char)) 2))
      (progn
	;; ���ȥ����������ؤ���
	(or (eolp)
	    (tcode-backward-char 1))
	(tcode-transpose-strokes arg))
    ;; ʸ���������ؤ���
    (if (memq last-command tcode-transpose-strokes-enable-commands)
	(progn
	  (backward-char 1))
      (setq this-command 'transpose-chars)
      (and (eolp)
	   (backward-char 1)))
    (transpose-chars arg)))

;;;; �������Ф���򤼽񤭼��񤫤���ɤߤ�ɽ��

;;;###autoload
(defun tcode-mazegaki-show-yomi-region (begin end &optional prefix)
  "�꡼�����ǻ��ꤵ�줿ʸ������ɤߤ�򤼽񤭼��񤫤�õ����ɽ�����롣
PREFIX �� nil �Ǥʤ���Х꡼��������ʸ����ǻϤޤ�ʸ�����õ����"
  (interactive "r\nP")
  (let* ((kanji (buffer-substring begin end))
	 (pattern (concat "/" kanji (if prefix "" "/")))
	 list)
    (save-excursion
      (tcode-mazegaki-switch-to-dictionary)
      (goto-char (point-min))
      (while (search-forward pattern nil t)
	(beginning-of-line)
	(looking-at "^\\([^/]+\\) /")
	(setq list (nconc list
			  (list (buffer-substring (match-beginning 1)
						  (match-end 1)))))
	(forward-line 1))
      (if list
	  (message (mapconcat 'identity list ", "))
	(error "��%s�פ��ɤߤϸ��Ĥ���ޤ���Ǥ�����" kanji)))))

;;;; �Ҥ餬�ʤ��饫�����ʤؤ��Ѵ�

(unless (fboundp 'japanese-katakana-region)
  (if (fboundp 'katakana-region)
      (defun japanese-katakana-region (start end)
	(katakana-region start end))
    (defun japanese-katakana-region (start end)
      "�꡼�������ΤҤ餬�ʤ򥫥����ʤˤ��롣"
      (interactive "r")
      (let* ((str (buffer-substring start end))
	     (katakana (mapconcat (lambda (char) 
				    (char-to-string (japanese-katakana char)))
				  (string-to-list str)
				  nil)))
	(unless (string= str katakana)
	  (delete-region start end)
	  (insert katakana))))))

;;;###autoload
(defun tcode-katakana-previous-char (n)
  "���ݥ���Ȥ�� n ʸ�����ޤǤΤҤ餬�ʤ򥫥����ʤˤ��롣"
  (interactive "*p")
  (let ((prev-char (tcode-preceding-char)))
    (japanese-katakana-region (save-excursion (tcode-backward-char n) (point))
		     (point))
    (and tcode-auto-help
	 (/= prev-char (tcode-preceding-char))
	 (= n 1)
	 (tcode-display-direct-stroke
	  (char-to-string (tcode-preceding-char)))
	 (tcode-auto-remove-help-char))))

;;;; ������JIS �����ɤˤ���������

;;;###autoload
(defun tcode-insert-kanji-by-kuten-code (code)
  "���������� CODE �δ������������롣"
  (interactive "*s����������(10�ʿ�4��)? ")
  (let* ((declist (mapcar (lambda (n)
			    (if (and (>= n ?0)
				     (<= n ?9))
				(- n ?0)
			      0))
			  (string-to-list code)))
	 (kuten (cons (+ (* (car declist) 10)
			 (car (setq declist (cdr declist))))
		      (+ (* (car (setq declist (cdr declist))) 10)
			 (car (cdr declist))))))
    (and (or (> (cdr kuten) 94)
	     (= (cdr kuten) 0)
	     (memq (car kuten) '(0 14 15)))
	 (error "������(%s)���ְ�äƤ��ޤ���" code))
    (tcode-insert-kanji-by-jis-code (format "%x%x"
					    (+ (car kuten) 32)
					    (+ (cdr kuten) 32)))))

;;;###autoload
(defun tcode-insert-kanji-by-jis-code (code)
  "JIS������ CODE �δ������������롣"
  (interactive "*sJIS ������(16�ʿ�)? ")
  (let ((hexlist (mapcar (lambda (n)
			   (cond ((and (>= n ?0)
				       (<= n ?9))
				  (- n ?0))
				 ((and (>= (setq n (downcase n)) ?a)
				       (<= n ?f))
				  (+ (- n ?a) 10))
				 (t
				  0)))
			 (string-to-list code)))
	bytelist)
    (while hexlist
      (setq bytelist (nconc bytelist
			    (list (+ (* (car hexlist) 16)
				     (car (cdr hexlist)))))
	    hexlist (nthcdr 2 hexlist)))
    (let ((kanji (make-char tcode-jisx0208
			    (+ (car bytelist) 128)
			    (+ (car (cdr bytelist)) 128))))
      (tcode-insert kanji)
      (and tcode-auto-help
	   (tcode-display-direct-stroke (char-to-string kanji))
	   (tcode-auto-remove-help-char)))))

;;;; �Хåե������Ƥ˱�������������ư�ڤ��ؤ�

(defvar tcode-kutouten-regexp-alist
  (list '("[����]" . 1)
	(if (tcode-nemacs-p)
	    '("\\z[,.]" . 2)
	  '("\\cj[,.]" . 2)))
  "* ��������Ƚ�ꤹ�뤿�������ɽ���� alist��
�ꥹ�Ȥγ����Ǥϡ�
���ζ��������Ȥ��Ƥ��뤳�Ȥ�Ƚ�ꤹ�뤿�������ɽ���ȡ�
����˥ޥå������������Ф�� `tcode-switch-table-list' ���Ȥ�
�ֹ�(�����ܤ��Ȥ�)��")

(defvar tcode-auto-identify-kutouten-mode-list '(text-mode)
  "* �������μ�ưȽ���Ԥ���⡼�ɤΥꥹ�ȡ�")

(defun tcode-identify-kutouten-type ()
  "�Хåե������Ƥ�����Ѥ���Ƥ����������Ƚ�̤��롣
`tcode-kutouten-regexp-alist' ������ɽ������õ�����ޥå������
�����ֹ���֤����ɤ�ˤ⤢�ƤϤޤ�ʤ����� 0 ���֤���"
  (catch 'found
    (let* ((list tcode-kutouten-regexp-alist)
	   regexp)
      (while list
	(setq regexp (car (car list)))
	(save-excursion
	  (goto-char (point-min))
	  (and (re-search-forward regexp nil t)
	       (throw 'found (cdr (car list)))))
	(setq list (cdr list)))
      0)))

;;;###autoload
(defun tcode-auto-switch-kutouten (&optional force)
  "�Хåե������Ƥ��鼫ưŪ�˶��������ڤ��ؤ��롣
FORCE �� nil �ξ��ϡ�
`tcode-auto-identify-kutouten-mode-list' ��Υ⡼�ɤǡ�
���Ĥ��ΥХåե��� read-only �Ǥʤ����ˤΤ�ư��롣
������������ϴؿ� `tcode-identify-kutouten-type' �ǹԤ���"
  (interactive "P")
  (and (or force
	   (and (memq major-mode tcode-auto-identify-kutouten-mode-list)
		(not buffer-read-only)))
       (tcode-switch-variable (tcode-identify-kutouten-type))))

;;;; ľ����ʸ������˥������ʤ��Ѵ�

(unless (fboundp 'japanese-hiragana-region)
  (if (fboundp 'hiragana-region)
      (defun japanese-hiragana-region (start end)
	(hiragana-region start end))
    (defun japanese-hiragana-region (start end)
      "�꡼�������Υ������ʤ�Ҥ餬�ʤˤ��롣"
      (interactive "r")
      (let* ((str (buffer-substring start end))
	     (hiragana (mapconcat (lambda (char) 
				    (char-to-string (japanese-hiragana char)))
				  (string-to-list str)
				  nil)))
	(unless (string= str hiragana)
	  (delete-region start end)
	  (insert hiragana))))))

(defun tcode-katakana-preceding-chars (arg)
  "ľ����ʸ������˥������ʤ��Ѵ����롣
���ޥ�ɤΥ����򲿲󤫲����ȡ����β������ľ���ΤҤ餬�ʤ��������ʤˤʤ롣
Backspace �ǺǸ�˥������ʤˤʤä�ʸ����Ҥ餬�ʤ��᤹��
RET �ǽ�λ��
����¾�Υ����Ϥ��Υ�����ư���Ԥ���"
  (interactive "*p")
  (let ((point (point)))
    (cond ((> arg 0)
	   (tcode-forward-char (- arg))
	   (japanese-katakana-region (point) point))
	  ((< arg 0)
	   (tcode-forward-char arg)
	   (japanese-hiragana-region (point) (progn (tcode-forward-char 1)
						    (point)))
	   (setq arg (1- (- arg)))))
    (goto-char point)
    (unwind-protect
	(let* ((echo-keystrokes 0)
	       (ch (read-char)))
	  (cond ((= ch last-command-char)
		 (tcode-katakana-preceding-chars (1+ arg)))
		((= ch ?\C-?)
		 (tcode-katakana-preceding-chars (- arg)))
		((= ch ?\C-m))
		(t
		 (tcode-redo-command ch))))
      (goto-char point))))

;;;; KKC ���Ѥ����Ѵ�
;;; contributed by Masayuki Ataka / ���� ��Ƿ

(defvar tcode-kkc-toroku t
  "* ���ޥ�� `tcode-kkc-region' �ǡ��Ѵ����ʸ������Ͽ���뤫�ɤ�����ɽ����
t ���Ѵ�����ʸ����򤼽񤭼������Ͽ���롣
nil �Ǥ���Ͽ���ʤ���1 �����򤹤�ȡ���Ͽʸ�����ɤߤ����Ǥ��롣")

(defun tcode-kkc-region (beg end)
  "�꡼�����ǰϤޤ줿ʿ��̾�����������Ѵ����롣

������ leim �ѥå�������ȤäƤ���Τǡ�
leim �ѥå����������äƤ��ʤ���лȤ����ȤϤǤ��ʤ���
���Ѥ��Ƥ��뼭��� leim �ѥå�������°�μ���

�ѿ� `tcode-kkc-toroku' ��Ȥäơ�
�Ѵ�����ʸ���θ򤼽񤭼���ؤ���Ͽ��
���뤫���ʤ��������椹�뤳�Ȥ��Ǥ��롣"
  (interactive "r")
  (let ((default current-input-method)
	(yomi (buffer-substring beg end)))
    (unwind-protect
	(progn
	  (activate-input-method "japanese")
	  (kkc-region beg end)
	  (tcode-kkc-mazegaki-toroku beg yomi))
      (activate-input-method default))))

(defun tcode-kkc-mazegaki-toroku (beg yomi)
  (cond
   ;; �Ѵ�����ʸ����򤼽񤭼������Ͽ
   ((equal tcode-kkc-toroku t)
    (tcode-mazegaki-make-entry yomi (buffer-substring beg (point))))
   ;; �ɤߤ������Ƥ��顢�������Ͽ
   ((equal tcode-kkc-toroku 1)
    (let ((minibuffer-setup-hook (list 'tcode-activate)))
      (tcode-mazegaki-make-entry
       (read-string
	(format "\"%s\"���ɤ�: "
		(buffer-substring beg (point))) yomi)
       (buffer-substring beg (point)))))
   ;; �򤼽񤭼���ؤ���Ͽ�Ϥ��ʤ���
   (t )))

;;;; zap-to-char �γ�ĥ

;;;###autoload
(defun tcode-zap-to-char (arg char)
  "`zap-to-char'�γ�ĥ�ǡ�T�����ɥ⡼�ɤΤȤ���T�����ɤ����Ϥ��롣"
  (interactive (list (prefix-numeric-value current-prefix-arg)
		     (let ((key (and (message (if (tcode-on-p)
						  "Zap to char [TC]: "
						"Zap to char: "))
				     (read-char))))
		       (if (tcode-on-p)
			   (let ((keys (tcode-input-method key)))
			     (if (= (length keys) 1)
				 (car keys)))
			 key))))
  (if char
      (zap-to-char arg char)))

(provide 'tc-util)

;;; tc-util.el ends here
