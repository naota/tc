;;; tc-mkmzdic.el --- make mazegaki.dic using emacs

;; Copyright (C) 2001 YAGI Tatsuya

;; Author: YAGI Tatsuya <yagi@is.titech.ac.jp>
;; Version: $Id: tc-mkmzdic.el,v 2.8 2002/09/26 02:22:50 kitajima Exp $
;; Maintainer: KITAJIMA Akira <kitajima@isc.osakac.ac.jp>

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

(require 'tc-setup)
(require 'tc-sysdep)
(require 'tc-mazegaki)

(unless (fboundp 'split-char)
  (defun split-char (c)
    (list 'japanese-jisx0208
	  (- (/ c 256) 128)
	  (- (mod c 256) 128))))

(unless (fboundp 'match-string)
  (defun match-string (num &optional string)
    (if string
	(substring string (match-beginning num)
		   (match-end num))
      (buffer-substring (match-beginning num)
			(match-end num)))))

(unless (fboundp 'with-temp-buffer)
  (defmacro with-temp-buffer (&rest body)
    (cons 'progn 
	  (nconc (list '(set-buffer
			 (get-buffer-create " *temp buffer*"))
		       '(widen)
		       '(erase-buffer))
		 body))))

(unless (fboundp 'delete)
  (defun delete (elt seq)
    (let (l)
      (while seq
	(or (equal (car seq) elt)
	    (setq l (cons (car seq) l)))
	(setq seq (cdr seq)))
      (nreverse l))))

(defun tcode-char-charset (c)
  (if (< emacs-major-version 20)
      (if (< c 256)
	  'ascii
	'japanese-jisx0208)
    (char-charset c)))


(defvar tcode-mazegaki-certains '("certain")
  "*確実に覚えた字の一覧が入っているファイルのリスト。")
(defvar tcode-mazegaki-uncertains '("uncertain")
  "*覚えていない字の一覧が入っているファイルのリスト。")

(defvar tc-mkmzdic-status-vector nil)

(defun tc-mkmzdic-init-status (init)
  (setq tc-mkmzdic-status-vector (make-vector (* 94 94) init)))

(defun tc-mkmzdic-final-status ()
  (setq tc-mkmzdic-status-vector nil))

(defun tc-mkmzdic-get-status (c)
  (aref tc-mkmzdic-status-vector (tc-mkmzdic-code c)))

(defun tc-mkmzdic-set-status (c val)
  (aset tc-mkmzdic-status-vector (tc-mkmzdic-code c) val))

(defun tc-mkmzdic-code (c)
  (if (stringp c) (setq c (tcode-string-to-char c)))
  (let ((l (split-char c)))
    (+ (* 94 (- (nth 1 l) ?!)) (- (nth 2 l) ?!))))

(defun tc-mkmzdic-set-status-from-file (file val)
  (with-temp-buffer
    (insert-file-contents (tcode-path-for-read file))
    (while (not (eobp))
      (let ((c (tcode-following-char)))
	(if (eq 'japanese-jisx0208 (tcode-char-charset c))
	    (tc-mkmzdic-set-status c val)))
      (forward-char))))

(defun tc-mkmzdic-set-status-from-files (files val)
  (while files
    (tc-mkmzdic-set-status-from-file (car files) val)
    (setq files (cdr files))))

(defvar tc-mkmzdic-obarray nil)

(defun tc-mkmzdic-init-obarray (size)
  (setq tc-mkmzdic-obarray (make-vector size nil)))

(defun tc-mkmzdic-final-obarray ()
  (setq tc-mkmzdic-obarray nil))

(defun tc-mkmzdic-put-yomi (yomi kanji)
  (or (equal yomi kanji)
      (let ((l (symbol-value (intern-soft yomi tc-mkmzdic-obarray))))
	(set (intern yomi tc-mkmzdic-obarray) (cons kanji l)))))

(defun tc-mkmzdic-add-yomi (yomi kanji)
  (let ((l (symbol-value (intern-soft yomi tc-mkmzdic-obarray))))
    (set (intern yomi tc-mkmzdic-obarray)
	 (nconc l (list kanji)))))

(defun tc-mkmzdic-sort-kanji ()
  (mapatoms (lambda (sym)
	      (if sym
		  (let ((l (symbol-value sym)))
		    (set sym (sort l 'string-lessp)))))
	    tc-mkmzdic-obarray))

(defun tc-mkmzdic-keys ()
  (let ((l nil))
    (mapatoms (lambda (sym) (if sym (setq l (cons (symbol-name sym) l))))
	      tc-mkmzdic-obarray)
    (sort l 'string-lessp)))

(defun tc-mkmzdic-insert-yomi (yomi)
  (let ((l (symbol-value (intern-soft yomi tc-mkmzdic-obarray)))
	kanji)
    (insert yomi " /")
    (while l
      (insert (setq kanji (car l)) "/")
      (setq l (delete kanji l)))
    (insert "\n")))

(defun tc-mkmzdic-map-insert-yomi ()
  (message "sorting key")
  (let ((l (tc-mkmzdic-keys)))
    (message "inserting data")
    (while l
      (tc-mkmzdic-insert-yomi (car l))
      (setq l (cdr l)))))

(defun tc-mkmzdic-put-kihon-yomi (file)
  (with-temp-buffer
    (insert-file-contents (tcode-path-for-read file))
    (let ((n (count-lines (point-min) (point-max)))
	  (i 0))
      (while (not (eobp))
	(if (= 0 (logand i 1023)) ;; 1023 = 3ff
	    (message "kihon-yomi: %d/%d" i n))
	(tc-mkmzdic-put-string
	 (buffer-substring (point) (progn (end-of-line) (point))))
	(forward-line)
	(setq i (1+ i))))))

(defun tc-mkmzdic-put-string (str)
  (let ((l nil)
	(beg 0))
    (while (string-match "\\(.\\)<\\([^>]*\\)>" str beg)
      (setq l (cons (substring str beg (match-beginning 0)) l)
	    l (cons (cons (match-string 1 str) (match-string 2 str)) l)
	    beg (match-end 0)))
    (setq l (cons (substring str beg) l)
	  l (nreverse l))
    (tc-mkmzdic-put-list l "" "")))

(defun tc-mkmzdic-put-list (l yomi kanji)
  (cond ((null l)
	 (tc-mkmzdic-put-yomi yomi kanji))
	((stringp (car l))
	 (tc-mkmzdic-put-list (cdr l)
			      (concat yomi (car l))
			      (concat kanji (car l))))
	(t ;; (consp (car l))
	 (let* ((c (car (car l)))
		(cyomi (cdr (car l)))
		(cstat (tc-mkmzdic-get-status c)))
	   (if (not (eq 'certain cstat))
	       (tc-mkmzdic-put-list (cdr l)
				    (concat yomi cyomi)
				    (concat kanji c)))
	   (if (not (eq 'cannot-input cstat))
	       (tc-mkmzdic-put-list (cdr l)
				    (concat yomi c)
				    (concat kanji c)))))))

(defun tc-mkmzdic-put-file (file)
  (with-temp-buffer
    (insert-file-contents (tcode-path-for-read file))
    (flush-lines "^\\(;\\|$\\)")
    (while (not (eobp))
      (looking-at "^\\(.*\\)[ \t]+\\(.*\\)$")
      (tc-mkmzdic-add-yomi (match-string 1) (match-string 2))
      (forward-line))))

(defun tc-mkmzdic (outfile certains uncertains kihon-yomi &rest mazedics)
  (let ((coding-system-for-read 'euc-jp)
	(coding-system-for-write 'euc-jp-unix))
    (if (and (file-exists-p outfile)
	     (not (y-or-n-p (format "file %s already exists.  Overwrite?"
				    outfile))))
	(error "Canceled."))
    (tc-mkmzdic-init-status 'cannot-input)
    (message "reading certain files")
    (tc-mkmzdic-set-status-from-files certains 'certain)
    (message "reading uncertain files")
    (tc-mkmzdic-set-status-from-files uncertains 'uncertain)
    (tc-mkmzdic-init-obarray 300000)
    (message "parsing kihon-yomi")
    (tc-mkmzdic-put-kihon-yomi kihon-yomi)
    (tc-mkmzdic-final-status)
    (message "sorting kanji")
    (tc-mkmzdic-sort-kanji)
    (message "reading additional dics")
    (while mazedics
      (tc-mkmzdic-put-file (car mazedics))
      (setq mazedics (cdr mazedics)))
    (with-temp-buffer
      (tc-mkmzdic-map-insert-yomi)
      (goto-char (point-min))
      (while (search-forward "―/" nil t)
	(replace-match "/" nil t))
      (write-region (point-min) (point-max) outfile))
    (tc-mkmzdic-final-obarray)))

;;;###autoload
(defun tcode-make-mazegaki-dictionary ()
  (interactive)
  (tc-mkmzdic tcode-mazegaki-dictionary-name
	      tcode-mazegaki-certains
	      tcode-mazegaki-uncertains
	      "pd_kihon.yom"
	      "greece.maz" "itaiji.maz" "jukujiku.maz"))

;;; tc-mkmzdic.el ends here
