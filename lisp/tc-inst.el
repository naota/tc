;;; tc-inst.el --- installer of personal environment for T-Code

;; Copyright (C) 2002 KITAJIMA Akira.

;; Author: KITAJIMA Akira <kitajima@isc.osakac.ac.jp>
;; Maintainer: KITAJIMA Akira
;; Create: 14 Mar, 2002

;; $Id: tc-inst.el,v 2.7 2002/07/23 07:17:10 kitajima Exp $

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA.

;;; Code:

(require 'tc)

;;;###autoload
(defun tcode-install ()
  "T�������ѤΥǡ��������ꤹ�롣"
  (interactive)
  (if (file-exists-p tcode-init-file-name)
      (error "%s���Խ����Ƥ���������" tcode-init-file-name)
    (let ((dir (read-string "T�������ѤΥǡ������֤��ǥ��쥯�ȥ��? " 
			    "~/tcode/")))
      ;; �ǥ��쥯�ȥ�κ���
      (unless (file-exists-p dir)
	(if (not (fboundp 'make-directory))
	    (error "�ǥ��쥯�ȥ�%s��������Ƥ���������" dir))
	(if (y-or-n-p (format "�ǥ��쥯�ȥ�%s����ޤ���?" dir))
	    (make-directory dir)))
      (unless (file-directory-p dir)
	(error "�ǥ��쥯�ȥ�%s�λ��꤬�ְ�äƤ��ޤ���" dir))
      (if (/= (aref dir (1- (length dir))) ?/)
	  (setq dir (concat dir "/")))
      (setq dir (expand-file-name dir))
      ;; .tc�κ���
      (with-current-buffer (get-buffer-create "*tcode: .tc *")
	(erase-buffer)
	(insert ";;; -*-emacs-lisp-*- This file is automatically created\n")
	(insert (format "(setq tcode-data-directory \"%s\")\n" dir))
	(insert (format "(setq tcode-default-input-method \"%s\")\n"
			(or tcode-current-package
			    tcode-default-input-method)))
	(let ((layout (call-interactively 'tcode-set-key-layout)))
	  (unless (assoc (car layout) tcode-key-layout-list)
	    (insert "(setq tcode-key-layout-list '"
		    (prin1-to-string (cons layout tcode-key-layout-list))
		    ")\n"))
	  (insert "(add-hook 'tcode-ready-hook "
		  "'(lambda () (tcode-set-key-layout \"" 
		  (car layout)
		  "\")))\n"))
	(write-file tcode-init-file-name))
      (setq tcode-data-directory dir)
      (message "�ܤ�������ˡ��Info��tc�ι��ܤ򸫤Ƥ���������"))))

;;; tc-inst.el ends here
