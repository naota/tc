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
  "*補完の際の最大候補数。"
  :type 'integer :group 'tcode)

(defcustom tcode-complete-min-context-length 3
  "*補完の際の文脈の最小長。"
  :type 'integer :group 'tcode)

(defcustom tcode-complete-max-context-length 8
  "*補完の際の文脈の最大長。"
  :type 'integer :group 'tcode)

(defcustom tcode-complete-delay 0.5
  "*候補が表示されるまでの待ち時間。"
  :type 'float :group 'tcode)

(defcustom tcode-complete-dictionary-name "complete.dic"
  "*補完辞書のファイル名。"
  :type 'string :group 'tcode)
(defconst tcode-complete-buffer-name " *tcode: complete dictionary*")
;; 補完辞書のバッファ名

(defcustom tcode-complete-mazegaki-prefix-length 3
  "*交ぜ書き変換辞書からの補完の場合に接頭辞とみなす文字数。")

;;; 辞書の登録
(unless (assq tcode-complete-buffer-name tcode-dictionaries)
  (setq tcode-dictionaries (cons (cons tcode-complete-buffer-name
				       tcode-complete-dictionary-name)
				 tcode-dictionaries)))

(defvar tcode-complete-candidate-list nil)
;; 補完候補を保持する変数
(make-variable-buffer-local 'tcode-complete-candidate-list)

(defvar tcode-message-overlay nil)
(make-variable-buffer-local 'tcode-message-overlay)

(defvar tcode-message-overlay-prefix ">")
(defvar tcode-message-overlay-suffix "<")

;;;
;;; オーバレイを用いたメッセージ表示
;;;
(defun tcode-overlay-message (str)
  "overlayを用いて、現在の行にメッセージ(STR)を表示する。"
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
    ;; 表示すると隠れる場合は再表示
    (if (>= (+ (count-lines (window-start) (point)) nol 1)
	    (1- (window-height)))
	(recenter (1- (- nol))))))

(defun tcode-delete-overlay-message ()
  "`tcode-overlay-message'で表示されたメッセージを消す。"
  (interactive)
  (when (overlayp tcode-message-overlay)
    (save-excursion
      (goto-char (overlay-start tcode-message-overlay))
      (if (looking-at (regexp-quote tcode-message-overlay-prefix))
	  (delete-region (match-beginning 0) (match-end 0))))
    (delete-overlay tcode-message-overlay)
    (redraw-frame (selected-frame))))

;;;
;;; 辞書管理
;;;

;;;###autoload
(defun tcode-complete-reload-dictionary ()
  "補完辞書を再読み込みする。"
  (interactive)
  (tcode-set-work-buffer tcode-complete-buffer-name
			 tcode-complete-dictionary-name
			 t))

(defun tcode-complete-lookup (prefix)
  "補完用辞書からPREFIXを持つ候補を探す。"
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
  "バッファを補完用辞書に切り替える。"
  (interactive)
  (switch-to-buffer
   (tcode-set-work-buffer tcode-complete-buffer-name
			  tcode-complete-dictionary-name)))

(defun tcode-complete-add-to-dictionary (beg end)
  "リージョンで指定した語を補完用辞書に登録する。"
  (interactive "r")
  (let ((str (buffer-substring beg end)))
    (save-excursion
      (tcode-set-work-buffer tcode-complete-buffer-name
			     tcode-complete-dictionary-name)
      (goto-char (point-min))
      (insert str "\n"))))

(defun tcode-complete-copy-entry-from-mazegaki-dictionary (prefix candidate)
  (save-excursion
    ;; 補完辞書に登録済みかどうか調べる。
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
	;; 補完辞書にはなかったので追加する。
	;; 読みを交ぜ書き変換辞書から調べる。
	(tcode-mazegaki-switch-to-dictionary)
	(tcode-mazegaki-search-yomi (regexp-quote prefix))
	(when (and (search-forward 
		    (concat "/" (regexp-quote candidate) "/") nil t)
		   (save-excursion
		     (beginning-of-line)
		     (looking-at (regexp-quote prefix))))
	  ;; 登録するべき候補を見つけた。
	  (beginning-of-line)
	  (looking-at (concat "\\(" prefix ".*\\) /"))
	  (let ((yomi (match-string 1)))
	    ;; 読みyomi候補candidateで登録する。
	    (tcode-set-work-buffer tcode-complete-buffer-name
				   tcode-complete-dictionary-name)
	    (goto-char (point-min))
	    (insert (format "%s %s\n" yomi candidate))))))))

;;;
;;; 補完入力
;;;

;;;###autoload
(defun tcode-complete-insert (n)
  "現在の文脈から推定できる入力候補を挿入する。
Nが指定された場合は、N番目の候補になる。"
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
;;; 補完候補抽出
;;;

(defun tcode-complete-scan-backward ()
  "現在のポイントから文脈を得る。
文脈はリスト構造であり、リストの要素は(POINT . \"文字列\")である。
ここで、「文字列」は、POINTから始まる現在のポイントまでの文字列である。
文字列の長さは`tcode-complete-max-context-length'までである。"
  (let ((raw-context (tcode-scan-backward tcode-complete-max-context-length))
	context)
    (while raw-context
      (setq context (cons (cons (car (car raw-context))
				(mapconcat 'cdr raw-context nil))
			  context)
	    raw-context (cdr raw-context)))
    (reverse context)))

(defun tcode-complete-search-candidate (context)
  "辞書から文脈に合う候補を探す。"
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
  "補完候補のリストを表す文字列を作る。"
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
  "現在の文脈から推定できる入力候補を表示する。"
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
				      "「\\[tcode-complete-insert]」で補完"))
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
