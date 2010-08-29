;;; tc-complete.el --- completion with T-Code

;; Copyright (C) 2001 KITAJIMA Akira.

;; Author: KITAJIMA Akira <kitajima@isc.osakac.ac.jp>
;; Maintainer: KITAJIMA Akira
;; Keyword: completion
;; Created: Jul 31, 2001

;; $Id: tc-complete.el,v 1.17 2003/03/03 06:46:45 kitajima Exp $

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

(defcustom tcode-complete-max-candidate-count 3
  "*�䴰�κݤκ���������"
  :type 'integer :group 'tcode)

(defcustom tcode-complete-min-context-length 3
  "*�䴰�κݤ�ʸ̮�κǾ�Ĺ��"
  :type 'integer :group 'tcode)

(defcustom tcode-complete-max-context-length 8
  "*�䴰�κݤ�ʸ̮�κ���Ĺ��"
  :type 'integer :group 'tcode)

(defcustom tcode-complete-delay 0.5
  "*���䤬ɽ�������ޤǤ��Ԥ����֡�"
  :type 'float :group 'tcode)

(defcustom tcode-complete-dictionary-name "complete.dic"
  "*�䴰����Υե�����̾��"
  :type 'string :group 'tcode)
(defconst tcode-complete-buffer-name " *tcode: complete dictionary*")
;; �䴰����ΥХåե�̾

(defcustom tcode-complete-mazegaki-prefix-length 3
  "*�򤼽��Ѵ����񤫤���䴰�ξ�����Ƭ���Ȥߤʤ�ʸ������")

;;; �������Ͽ
(unless (assq tcode-complete-buffer-name tcode-dictionaries)
  (setq tcode-dictionaries (cons (cons tcode-complete-buffer-name
				       tcode-complete-dictionary-name)
				 tcode-dictionaries)))

(defvar tcode-complete-candidate-list nil)
;; �䴰������ݻ������ѿ�
(make-variable-buffer-local 'tcode-complete-candidate-list)

(defvar tcode-message-overlay nil)
(make-variable-buffer-local 'tcode-message-overlay)

(defvar tcode-message-overlay-prefix ">")
(defvar tcode-message-overlay-suffix "<")

;;;
;;; �����Х쥤���Ѥ�����å�����ɽ��
;;;
(defun tcode-overlay-message (str)
  "overlay���Ѥ��ơ����ߤιԤ˥�å�����(STR)��ɽ�����롣"
  (save-excursion
    (insert tcode-message-overlay-prefix))
  (let ((point (point))
	(nol (apply '+ (mapcar (lambda (c)
				 (if (= c ?\n)
				     1
				   0))
			       (string-to-list str)))))
    (setq tcode-message-overlay
	  (if (overlayp tcode-message-overlay)
	      (move-overlay tcode-message-overlay (point) (1+ point))
	    (make-overlay point (1+ point))))
    (overlay-put tcode-message-overlay
		 'after-string 
		 (concat str tcode-message-overlay-suffix))
    ;; ɽ������ȱ������Ϻ�ɽ��
    (if (>= (+ (count-lines (window-start) (point)) nol 1)
	    (1- (window-height)))
	(recenter (1- (- nol))))))

(defun tcode-delete-overlay-message ()
  "`tcode-overlay-message'��ɽ�����줿��å�������ä���"
  (interactive)
  (when (overlayp tcode-message-overlay)
    (save-excursion
      (goto-char (overlay-start tcode-message-overlay))
      (if (looking-at (regexp-quote tcode-message-overlay-prefix))
	  (delete-region (match-beginning 0) (match-end 0))))
    (delete-overlay tcode-message-overlay)
    (redraw-frame (selected-frame))))

;;;
;;; �������
;;;

;;;###autoload
(defun tcode-complete-reload-dictionary ()
  "�䴰�������ɤ߹��ߤ��롣"
  (interactive)
  (tcode-set-work-buffer tcode-complete-buffer-name
			 tcode-complete-dictionary-name
			 t))

(defun tcode-complete-lookup (prefix)
  "�䴰�Ѽ��񤫤�PREFIX����ĸ����õ����"
  (save-excursion
    (tcode-set-work-buffer tcode-complete-buffer-name
			   tcode-complete-dictionary-name)
    (goto-char (point-min))
    (let ((prefix-regexp (concat "^" (regexp-quote prefix)))
	  candidate-list)
      (catch 'overflow
	(while (search-forward-regexp prefix-regexp nil t)
	  (beginning-of-line)
	  (let ((candidate (if (looking-at "^.+ \\(.+\\)$")
			   (buffer-substring (match-beginning 1)
					     (match-end 1))
			 (buffer-substring (point)
					   (progn (end-of-line) (point))))))
	    (unless (string= candidate prefix)
	      (setq candidate-list (cons candidate candidate-list))
	      (if (> (length candidate-list) 
		     tcode-complete-max-candidate-count)
		  (throw 'overflow nil))))
	  (forward-line 1))
	(reverse candidate-list)))))

(defun tcode-complete-switch-to-dictionary ()
  "�Хåե����䴰�Ѽ�����ڤ��ؤ��롣"
  (interactive)
  (switch-to-buffer
   (tcode-set-work-buffer tcode-complete-buffer-name
			  tcode-complete-dictionary-name)))

(defun tcode-complete-add-to-dictionary (beg end)
  "�꡼�����ǻ��ꤷ������䴰�Ѽ������Ͽ���롣"
  (interactive "r")
  (let ((str (buffer-substring beg end)))
    (save-excursion
      (tcode-set-work-buffer tcode-complete-buffer-name
			     tcode-complete-dictionary-name)
      (goto-char (point-min))
      (insert str "\n"))))

(defun tcode-complete-copy-entry-from-mazegaki-dictionary (prefix candidate)
  (save-excursion
    ;; �䴰�������Ͽ�Ѥߤ��ɤ���Ĵ�٤롣
    (tcode-set-work-buffer tcode-complete-buffer-name
			   tcode-complete-dictionary-name)
    (goto-char (point-min))
    (let* ((search-string (concat "\\(^\\| \\)" (regexp-quote candidate) "$"))
	   (found (catch 'found
		    (while (search-forward-regexp search-string nil t)
		      (save-excursion
			(beginning-of-line)
			(if (or (looking-at search-string)
				(looking-at (regexp-quote prefix)))
			    (throw 'found t)))))))
      (unless found
	;; �䴰����ˤϤʤ��ä��Τ��ɲä��롣
	;; �ɤߤ�򤼽��Ѵ����񤫤�Ĵ�٤롣
	(tcode-mazegaki-switch-to-dictionary)
	(tcode-mazegaki-search-yomi (regexp-quote prefix))
	(when (and (search-forward 
		    (concat "/" (regexp-quote candidate) "/") nil t)
		   (save-excursion
		     (beginning-of-line)
		     (looking-at (regexp-quote prefix))))
	  ;; ��Ͽ����٤�����򸫤Ĥ�����
	  (beginning-of-line)
	  (looking-at (concat "\\(" prefix ".*\\) /"))
	  (let ((yomi (match-string 1)))
	    ;; �ɤ�yomi����candidate����Ͽ���롣
	    (tcode-set-work-buffer tcode-complete-buffer-name
				   tcode-complete-dictionary-name)
	    (goto-char (point-min))
	    (insert (format "%s %s\n" yomi candidate))))))))

;;;
;;; �䴰����
;;;

;;;###autoload
(defun tcode-complete-insert (n)
  "���ߤ�ʸ̮�������Ǥ������ϸ�����������롣
N�����ꤵ�줿���ϡ�N���ܤθ���ˤʤ롣"
  (interactive "*p")
  (when tcode-complete-candidate-list
    (delete-region (car (car tcode-complete-candidate-list)) (point))
    (let ((prefix (cdr (car tcode-complete-candidate-list)))
	  (candidate (nth (if (>= n 0)
			  (1- n)
			(+ (length (cdr tcode-complete-candidate-list)) n))
		      (cdr tcode-complete-candidate-list))))
      (tcode-complete-copy-entry-from-mazegaki-dictionary prefix candidate)
      (tcode-insert candidate))
    (tcode-complete-display)))

(global-set-key (kbd "M-RET") 'tcode-complete-insert)

;;;
;;; �䴰�������
;;;

(defun tcode-complete-scan-backward ()
  "���ߤΥݥ���Ȥ���ʸ̮�����롣
ʸ̮�ϥꥹ�ȹ�¤�Ǥ��ꡢ�ꥹ�Ȥ����Ǥ�(POINT . \"ʸ����\")�Ǥ��롣
�����ǡ���ʸ����פϡ�POINT����Ϥޤ븽�ߤΥݥ���ȤޤǤ�ʸ����Ǥ��롣
ʸ�����Ĺ����`tcode-complete-max-context-length'�ޤǤǤ��롣"
  (let ((raw-context (tcode-scan-backward tcode-complete-max-context-length))
	context)
    (while raw-context
      (setq context (cons (cons (car (car raw-context))
				(mapconcat 'cdr raw-context nil))
			  context)
	    raw-context (cdr raw-context)))
    (reverse context)))

(defun tcode-complete-search-candidate (context)
  "���񤫤�ʸ̮�˹礦�����õ����"
  (catch 'found
    (while context
      (let ((candidate-list (append (tcode-complete-lookup (cdr (car context)))
				(and (= tcode-complete-mazegaki-prefix-length
					(length context))
				     (tcode-mazegaki-lookup-with-prefix
				      (string-to-list (cdr (car context)))))))
	    result)
	(while candidate-list
	  (let ((candidate (car candidate-list)))
	    (setq result (nconc result (list candidate))
		  candidate-list (delete candidate candidate-list))))
	(if result
	    (throw 'found (cons (car context) result))))
      (setq context (cdr context)))))

(defun tcode-complete-make-candidate-list-string (prefix candidate-list)
  "�䴰����Υꥹ�Ȥ�ɽ��ʸ������롣"
  (format "%s%s"
	  (let ((candidate (car candidate-list)))
	    (if (string= prefix
			 (substring candidate 0 (length prefix)))
		(substring candidate (length prefix))
	      (concat "(" candidate ")")))
	  (let ((candidate-list (mapcar (lambda (candidate)
					  (substring candidate 
						     (length prefix)))
					(cdr candidate-list))))
	    (if candidate-list
		(concat " ["
			(let ((count 1))
			  (mapconcat (lambda (candidate)
				       (format "%d)%s"
					       (setq count (1+ count))
					       candidate))
				     (cdr candidate-list)
				     " "))
			"]"))
	    "")))

(defun tcode-complete-display ()
  "���ߤ�ʸ̮�������Ǥ������ϸ����ɽ�����롣"
  (interactive)
  (let* ((candidates (tcode-complete-search-candidate
		      (tcode-complete-scan-backward)))
	 (real-candidate-list (cdr candidates))
	 (prefix (cdr (car candidates)))
	 (noc (length real-candidate-list)))
    (if (or (> noc tcode-complete-max-candidate-count)
	    (< (length (string-to-list prefix))
	       tcode-complete-min-context-length))
	(setq tcode-complete-candidate-list nil)
      (setq tcode-complete-candidate-list candidates)
      (when (and candidates
		 (not (window-minibuffer-p (selected-window))))
	(unwind-protect
	    (progn
	      (tcode-overlay-message
	       (tcode-complete-make-candidate-list-string
		prefix real-candidate-list))
	      (tcode-verbose-message (tcode-substitute-command-keys
				      "��\\[tcode-complete-insert]�פ��䴰"))
	      (sit-for 5))
	  (tcode-delete-overlay-message))))))

(defun tcode-complete-display-function ()
  (if (and (tcode-on-p)
	   (memq last-command tcode-input-command-list)
	   (sit-for tcode-complete-delay))
      (tcode-complete-display)))

(add-hook 'post-command-hook 'tcode-complete-display-function)

(provide 'tc-complete)

;;; tc-complete.el ends here
