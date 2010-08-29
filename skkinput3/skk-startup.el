;;; skk-startup.el --- startup routine of skkinput3 for tc2

;;; $Id: skk-startup.el,v 1.4 2003/03/21 09:20:24 kitajima Exp $

;;; skkinput3.0.4のskk-startup.elを変更。

(use-local-map im-local-map)
(setq lc-jp 14)

(defun tcode-mode (&optional arg)
  "T-Code mode.
Type \\[tcode-mode-help] for more detail."
  (interactive "P")
  (tcode-activate (or arg
		      (if tcode-mode -1 1)))
  (tcode-mode-line-redisplay))

(defun im-tab ()
  (interactive)
  (let ((str (buffer-string)))
    (if (> (length str) 0)
	(im-send-text im-client str)
      (im-send-key im-client last-command-event))
    (erase-buffer)))

(define-key im-local-map "\C-\\" 'tcode-mode)
(define-key minibuffer-local-map "\C-\\" 'tcode-mode)
(define-key im-local-map "\t" 'im-tab)

(defun im-send-text-command ()
  (interactive)
  (im-auto-send-text))

(load "tc-mazegaki")
(load "tc-bushu")
(load "tc-util")
(setq mode-line-format '(current-input-method-title))

(activate-input-method default-input-method)

(tcode-set-key "!" nil)
(tcode-set-key "|" nil)

;(add-hook 'post-command-hook 'im-auto-send-text t t)

;;; wrapper

(defun japanese-katakana (char)
    "文字 CHAR がひらがなならカタカナに変換する。
ひらがなでない場合はそのままの値を返す。"
    (let ((str (char-to-string char)))
      (if (string-match (concat "^[ぁ-ん]$") str)
	  (+ 256 char)
	char)))

(defun tcode-display-help-buffer (buffer &optional display-only append)
;; dummy (disable this function)
  )

(defun tcode-draw-current-table (table)
;; dummy (disable this function)
  )