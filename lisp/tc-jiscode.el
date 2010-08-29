;;; tc-jiscode.el --- input with JIS code table

;; Copyright (C) 1989--2001 Kaoru Maeda, Yasushi Saito and KITAJIMA Akira.

;; Author: Kaoru Maeda <maeda@src.ricoh.co.jp>
;;	Yasushi Saito <yasushi@cs.washington.edu>
;;      KITAJIMA Akira <kitajima@isc.osakac.ac.jp>
;; Maintainer: KITAJIMA Akira
;; Keyword: input method

;; $Id: tc-jiscode.el,v 1.4 2002/03/19 07:21:10 kitajima Exp $

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

(defvar tcode-jiscode-buffer " *tcode: JIS code*")
(defvar tcode-jiscode-window-configuration nil)
(defvar tcode-jiscode-marker nil)

(defvar tcode-jiscode-map nil)
(unless tcode-jiscode-map
  (setq tcode-jiscode-map (make-sparse-keymap))
  (mapcar
   (lambda (elm)
     (let ((cmd (car elm))
	   (key (cdr elm)))
       (if (listp key)
	   (while key
	     (define-key tcode-jiscode-map (car key) cmd)
	     (setq key (cdr key)))
	 (define-key tcode-jiscode-map key cmd))))
   '((digit-argument       . ("0" "9" "8" "7" "6" "5" "4" "3" "2" "1" "-"))
     (previous-line        . ("k" "p"))
     (next-line            . ("j" "n"))
     (backward-char        . ("h" "b"))
     (forward-char         . ("l" "f"))
     (scroll-down          . "\C-?")
     (scroll-up            . " ")
     (tcode-jiscode-quit   . "q")
     (tcode-jiscode-insert . ("\n" "\r"))
     (describe-mode        . "?"))))

(defun tcode-jiscode-quit ()
  "JIS ����ɽ���ϥ⡼�ɤ�λ���롣"
  (interactive)
  (set-window-configuration tcode-jiscode-window-configuration)
  (goto-char tcode-jiscode-marker)
  (setq tcode-jiscode-window-configuration nil
	tcode-jiscode-marker nil))

(defun tcode-jiscode-insert-line (1st 2nd)
  (insert (format "%02x%02x " (- 1st 128) (- 2nd 128)))
  (let ((i 0))
    (while (and (< i 16) (< 2nd 255))
      (insert (char-to-string (make-char tcode-jisx0208 1st 2nd)))
      (setq 2nd (1+ 2nd))
      (setq i (1+ i)))
    (insert " : ")
    (while (and (< i 32)(< 2nd 255))
      (insert (char-to-string (make-char tcode-jisx0208 1st 2nd)))
      (setq 2nd (1+ 2nd))
      (setq i (1+ i))))
  (insert ":\n"))

(defun tcode-jiscode-insert-tables ()
  "JIS������ɽ���äơ����Хåե����������롣"
  (message "Making jiscode tables...")
  (let ((1st 161) (2nd 161))
    (while (< 1st 255)
      (setq 2nd 161)
      (tcode-jiscode-insert-line 1st 2nd)
      (tcode-jiscode-insert-line 1st (+ 2nd 32))
      (tcode-jiscode-insert-line 1st (+ 2nd (* 2 32)))
      (setq 1st (1+ 1st))))
  (message "Making jiscode tables...done.")
  (beginning-of-buffer))

(defun tcode-jiscode-insert ()
  "JIS ����ɽ�θ��߰��֤ˤ���ʸ����ɽ�������ˤ����Хåե����������롣"
  (interactive)
  (let ((str (buffer-substring (point)
			       (save-excursion
				 (tcode-forward-char 1)
				 (point)))))
    (set-buffer (marker-buffer tcode-jiscode-marker))
    (tcode-insert str)
    (setq tcode-jiscode-marker
	  (move-marker tcode-jiscode-marker (point)))))

;;;###autoload
(defun tcode-start-jiscode ()
  "JIS����ɽ��ɽ�����롣
ɽ�����줿�Хåե��Ǥϡ�����Ū�˴���������Ǹ��ΥХåե��������Ǥ��롣
���ѤǤ��륭���ϼ����̤ꡣ

   \\[tcode-jiscode-insert]\t����������֤δ������������Хåե����������롣
   \\[tcode-jiscode-quit]\t�������Хåե�����롣

\\{tcode-jiscode-map}"
  (interactive)
  (setq tcode-jiscode-marker (point-marker))
  (or (get-buffer-window tcode-jiscode-buffer)
      (setq tcode-jiscode-window-configuration
	    (current-window-configuration)))
  (switch-to-buffer-other-window tcode-jiscode-buffer)
  (if (= (point-min) (point-max))
      (progn
	(tcode-jiscode-insert-tables)
	(setq mode-name "JIS-select")
	(use-local-map tcode-jiscode-map)
	(setq major-mode 'tcode-start-jiscode)
	(setq buffer-read-only t)))
  (tcode-verbose-message "? �ǥإ��"))

(provide 'tc-jiscode)

;;; tc-jiscode ends here
