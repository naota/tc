;;; tc-help.el --- help routines for T-Code

;; Copyright (C) 1997-2001 Kaoru Maeda, Yasushi Saito and Akira Kitajima.

;; Author: Kaoru Maeda <maeda@src.ricoh.co.jp>
;;	Yasushi Saito <yasushi@cs.washington.edu>
;;	Akira Kitajima <kitajima@isc.osakac.ac.jp>
;; Maintainer: Akira Kitajima
;; Created: 3 Apr 1997
;; Version: $Id: tc-help.el,v 1.36 2003/03/21 03:44:48 kitajima Exp $

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

(require 'tc-bushu)

;;;; �ѿ�

(defvar tcode-strict-help t
  "* non-nil �ΤȤ��������Ѵ��ˤ�����Ϥ��Ǥ��뤫�ɤ�����ܤ���Ĵ�٤롣
nil �ξ��Ǥϸ��Ĥ���ʤ��褦�ʾ��Ǥ⡢non-nil �ˤ���и��Ĥ����礬
���롣�����������ʤ���֤������뤳�Ȥ⤢�롣")

(defvar tcode-help-first-stroke "��"
  "* �إ��ɽ����1�Ǹ����֤�ɽ��ʸ��")
(defvar tcode-help-second-stroke "��"
  "* �إ��ɽ����2�Ǹ����֤�ɽ��ʸ��")
(defvar tcode-help-third-stroke "��"
  "* �إ��ɽ����3�Ǹ����֤�ɽ��ʸ��")
(defvar tcode-help-forth-stroke "��"
  "* �إ��ɽ����4�Ǹ����֤�ɽ��ʸ��")
(defvar tcode-help-double-stroke "��"
  "* �إ��ɽ��ʣ���󤳤ΰ��֤򲡤����Ȥ�ɽ��ʸ��")
(defvar tcode-help-another-double-stroke "��"
  "* �إ��ɽ��ʣ���󤳤ΰ��֤򲡤����Ȥ�ɽ��ʸ����
`tcode-help-double-stroke' ����˻��Ѥ��Ƥ�����˻��Ѥ��롣")

(defvar tcode-help-draw-top-keys t
  "* ���ȥ���ɽ�ǺǾ��ʤΥ�����ɽ�����뤫")

(defvar tcode-last-help-char-list nil
  "�Ǹ�˥إ�פ�ɽ������ʸ���Υꥹ��")

(defvar tcode-help-with-real-keys nil
  "* nil�Ǥʤ���硢ɽ�����ǤϤʤ����������Ͻ�����ˤ��إ��ɽ����Ԥ���")

(defvar tcode-stroke-to-string-option t
  "*`tcode-stroke-to-string'�����椹���ѿ�")
(defvar tcode-stroke-to-string-opener ""
  "*`tcode-stroke-to-string'�ǻȤ��볫��ʸ����")
(defvar tcode-stroke-to-string-separator ""
  "*`tcode-stroke-to-string'�ǻȤ���ʬ��ʸ����")
(defvar tcode-stroke-to-string-closer ""
  "*`tcode-stroke-to-string'�ǻȤ��뽪λʸ����")

(defcustom tcode-help-stroke-hook nil
  "�إ��ɽ��ɽ���������˼¹Ԥ����եå���"
  :type 'hook :group 'tcode)

;;;
;;; �⡼�ɤΥإ��
;;;

;;;###autoload
(defun tcode-mode-help ()
  "T�����ɥ⡼�ɤΥ���������Ƥʤɤ�ɽ�����롣"
  (interactive)
  (let ((buf (get-buffer-create " *T-Code Mode Help*")))
    (set-buffer buf)
    (erase-buffer)
    (if tcode-mode-help-string
	(insert (substitute-command-keys tcode-mode-help-string))
      (insert (documentation 'tcode-activate))
      (goto-char (point-min))
      (forward-line 1)
      (narrow-to-region (point) (point-max)))
    (tcode-display-help-buffer buf)
    (kill-buffer buf)))

;;;
;;; ��������Ѵ��Τإ��
;;;

(defun tcode-bushu-help-lookup (str)
  (save-excursion
    (tcode-set-work-buffer tcode-bushu-help-buffer-name
			   tcode-bushu-help-dictionary-name
			   nil t)
    (goto-char (point-min))
    (if (re-search-forward (format "^%s\\(.\\)\\(.\\)\\*?\n"
				   (regexp-quote str))
			   nil t)
	(cons (buffer-substring (match-beginning 1)
				(match-end 1))
	      (buffer-substring (match-beginning 2)
				(match-end 2))))))

(defun tcode-bushu-composed-p (char bushu1 bushu2)
  "CHAR��BUSHU1��BUSHU2�ǹ����Ǥ��뤫��
���Τˤϡ�BUSHU1��BUSHU2��ξ���Ȥ�ľ�����ϤǤ��ʤ��Ȥ����ʤ���"
  (and (or (null tcode-strict-help)
	   (and (tcode-encode bushu1)
		(tcode-encode bushu2)))
       (let ((composed (tcode-bushu-compose-two-chars bushu1 bushu2)))
	 (and composed
	      (= composed (tcode-string-to-char char))))))

(defun tcode-decompose-to-two-char (char)
  (let* ((str (char-to-string char))
	 (b1 (tcode-bushu-for-char char))
	 (b2 (list (car b1))))
    (catch 'found
      (while (setq b1 (cdr b1))
	(let ((cl1 (or (tcode-char-list-for-bushu b2)
		       (if (= (length b2) 1)
			   b2)))
	      (cl2 (or (tcode-char-list-for-bushu b1)
		       (if (= (length b1) 1)
			   b1))))
	  (mapc (lambda (c1)
		  (mapc (lambda (c2)
			  (if (= (tcode-bushu-compose-two-chars c1 c2)
				 char)
			      (throw 'found (cons c1 c2))))
			cl2))
		cl1))
	(setq b2 (nconc b2 (list (car b1))))))))

(defun tcode-decompose-char (kanji &optional for-help)
  "KANJI����Ĥ������ʬ�򤹤롣
�֤��ͤ���Ĥλ��� `cons' �������롣
ʬ��Ǥ��ʤ����ˤ�nil���֤���
FOR-HELP��nil�Ǥʤ����ϡ�ľ�����ϤǤ������ʬ�򤹤롣"
  (tcode-bushu-init 2)
  (save-excursion
    (tcode-set-work-buffer tcode-bushu-help-buffer-name
			   tcode-bushu-help-dictionary-name
			   nil t))
  (let* ((char (tcode-string-to-char kanji))
	 (decomposed (tcode-decompose-to-two-char char)))
    (if decomposed
	(if for-help
	    (let* ((char1 (car decomposed))
		   (char2 (cdr decomposed))
		   (str1 (char-to-string char1))
		   (str2 (char-to-string char2)))
	      (cond ((tcode-encode char1)
		     (if (tcode-encode char2)
			 (cons str1 str2)
		       (let ((bushu-list (tcode-bushu-for-char
					  (cdr decomposed))))
			 (catch 'found
			   ;; �����������õ����
			   (mapc
			    (lambda (c)
			      (if (tcode-bushu-composed-p kanji
							  (car decomposed)
							  c)
				  (throw 'found (cons str1
						      (char-to-string c)))))
			    (sort (tcode-bushu-subset bushu-list)
				  'tcode-bushu-less-p))
			   ;; ����������õ����
			   (mapc
			    (lambda (c)
			      (if (tcode-bushu-composed-p kanji
							  (car decomposed)
							  c)
				  (throw 'found (cons str1
						      (char-to-string c)))))
			    (sort (tcode-bushu-superset bushu-list)
				  'tcode-bushu-less-p))
			   ;; �Ƶ�Ū��õ����
			   (let ((dec2 (tcode-decompose-char str2 t)))
			     (if dec2
				 (cons str1 dec2)
			       (cons str1 (cons dec2 nil))))))))
		    ((tcode-encode char2)
		     (let ((bushu-list (tcode-bushu-for-char
					(car decomposed))))
		       (catch 'found
			 ;; �����������õ����
			 (mapc
			  (lambda (c)
			    (if (tcode-bushu-composed-p kanji
							c
							(cdr decomposed))
				(throw 'found (cons (char-to-string c)
						    str2))))
			  (sort (tcode-bushu-subset bushu-list)
				'tcode-bushu-less-p))
			 ;; ����������õ����
			 (mapc
			  (lambda (c)
			    (if (tcode-bushu-composed-p kanji
							c
							(cdr decomposed))
				(throw 'found (cons (char-to-string c)
						    str2))))
			  (sort (tcode-bushu-superset bushu-list)
				'tcode-bushu-less-p))
			 ;; �Ƶ�Ū��õ����
			 (let ((dec1 (tcode-decompose-char str1 t)))
			   (if dec1
			       (cons dec1 str2)
			     (cons (cons str1 nil) str2))))))
		    (t
		     (let* ((bushu1 (tcode-bushu-for-char (car decomposed)))
			    (bushu2 (tcode-bushu-for-char (cdr decomposed)))
			    (bushu-list (append bushu1 bushu2))
			    (cl1 (tcode-uniq
				  (sort
				   (nconc (tcode-bushu-subset bushu1)
					  (tcode-bushu-superset bushu1))
				   'tcode-bushu-less-p)))
			    (cl2 (tcode-uniq
				  (sort
				   (nconc (tcode-bushu-subset bushu2)
					  (tcode-bushu-superset bushu2))
				   'tcode-bushu-less-p))))
		       (catch 'found
			 (mapc
			  (lambda (c1)
			    (mapc
			     (lambda (c2)
			       (if (tcode-bushu-composed-p kanji c1 c2)
				   (throw 'found
					  (cons (char-to-string c1)
						(char-to-string c2)))))
			     cl2))
			  cl1)
			 (let ((d1 (tcode-decompose-char str1 t))
			       (d2 (tcode-decompose-char str2 t)))
			   (cons (or d1 (cons str1 nil))
				 (or d2 (cons str2 nil)))))))))
	  (cons (char-to-string (car decomposed))
		(char-to-string (cdr decomposed))))
      ;; ��Ĥ�ʬ��Ǥ��ʤ����
      (if for-help
	  ;; �������������õ����
	  (let ((bushu-list (tcode-bushu-for-char char)))
	    (catch 'found
	      (mapc
	       (lambda (c)
		 (when (tcode-encode c)
		   (let ((diff (tcode-subtract-set (tcode-bushu-for-char c)
						   bushu-list)))
		     (mapc (lambda (d)
			     (if (tcode-bushu-composed-p kanji c d)
				 (throw 'found
					(cons (char-to-string c)
					      (char-to-string d)))))
			   (tcode-bushu-subset diff)))))
	       (sort (tcode-bushu-superset bushu-list)
		     'tcode-bushu-less-p))
	      (cons kanji nil)))))))

(defun tcode-stroke-to-string (stroke)
  "STROKE ��ɽ��û��ʸ������֤���
`tcode-stroke-to-string-opener'�ǻϤޤꡢ
`tcode-stroke-to-string-separator'����ڤ�Ȥ���
`tcode-stroke-to-string-closer'�ǽ���롣

`tcode-stroke-to-string-option'�ˤ�ꥹ�ȥ���ɽ��ʸ�����Ѥ�롣
  nil �λ���2��ο�����ɽ����
  t �λ��� tcode-mode �Ǥʤ��������Ϥ����ʸ����ɽ����
  vector �λ������Ǥ����Ѥ��롣
  �ؿ��λ��Ϥ��δؿ���ƤӽФ���"
  (if (functionp tcode-stroke-to-string-option)
      (funcall tcode-stroke-to-string-option stroke)
    (concat tcode-stroke-to-string-opener
	    (mapconcat
	     (cond ((eq t tcode-stroke-to-string-option)
		    (lambda (addr) (char-to-string (tcode-key-to-char addr))))
		   ((vectorp tcode-stroke-to-string-option)
		    (lambda (addr) (aref tcode-stroke-to-string-option addr)))
		   (t (lambda (addr) (format "%02d" addr))))
	     stroke tcode-stroke-to-string-separator)
	    tcode-stroke-to-string-closer)))

(defun tcode-help-make-bushu-structure-string (str root)
  (cond ((null str)
	 nil)
	((stringp str)
	 (if tcode-help-with-real-keys
	     (let ((strokes (tcode-encode (tcode-string-to-char str))))
	       (if strokes
		   (tcode-stroke-to-string strokes)
		 str))
	   str))
	((listp str)
	 (let ((left (tcode-help-make-bushu-structure-string (car str) nil))
	       (right (tcode-help-make-bushu-structure-string (cdr str) nil)))
	   (if right
	       (format (if root "%s + %s" "(%s + %s)") left right)
	     (concat "[" left "]"))))))

(defun tcode-help-bushu-help-string (ch for-help)
  "ʸ��CH�Ρ���������Ѵ��ˤ���Ǥ�����ɽ��ʸ������֤���"
  (let ((decomposed (or (tcode-bushu-help-lookup ch)
			(tcode-decompose-char ch for-help))))
    (and decomposed
	 (concat (tcode-help-make-bushu-structure-string decomposed t)))))

;;;
;;; �إ��ɽ
;;;

(defun tcode-make-drawing-data (stroke)
  "STROKE ���Ǥ�����޼����뤿��Υǡ������������롣
��������ǡ����ϡ����Υꥹ�ȤΥꥹ�ȡ�
  ����������(0-39)  ��(���ʤ�)  ���֤򼨤�ʸ����(1�ʤ�)"
  (let ((1st (car stroke))
	(2nd (car (cdr stroke)))
	(3rd (car (cdr (cdr stroke))))
	(4th (car (cdr (cdr (cdr stroke)))))
	draw-data)
    (setq draw-data
	  (list (cons 1st
		      (if (memq 1st (cdr stroke))
			  (list tcode-help-double-stroke
				(cond ((and 4th 3rd
					    (= 1st 2nd)
					    (= 1st 3rd)
					    (= 1st 4th))
				       (setq 2nd nil 3rd nil 4th nil)
				       "1����2����3����4")
				      ((and 3rd (= 1st 2nd) (= 1st 3rd))
				       (setq 2nd nil 3rd nil)
				       "1����2����3")
				      ((and 4th 3rd (= 1st 2nd) (= 1st 4th))
				       (setq 2nd nil 4th nil)
				       "1����2����4")
				      ((and 4th 3rd (= 1st 3rd) (= 1st 4th))
				       (setq 3rd nil 4th nil)
				       "1����3����4")
				      ((and 4th 3rd (= 1st 4th))
				       (setq 4th nil)
				       "1����4")
				      ((and 3rd (= 1st 3rd))
				       (setq 3rd nil)
				       "1����3")
				      ((= 1st 2nd) (setq 2nd nil)
				       "1����2")))
			(list tcode-help-first-stroke "1")))))
    (and 2nd
	 (setq draw-data
	       (nconc draw-data
		      (list (cons 2nd
				  (if (memq 2nd (cdr (cdr stroke)))
				      (list
				       (if (and (= (length stroke) 4)
						(or (null 3rd) (null 4th)))
					   tcode-help-another-double-stroke
					 tcode-help-double-stroke)
				       (cond ((and 3rd 4th
						   (= 2nd 3rd) (= 2nd 4th))
					      (setq 3rd nil 4th nil)
					      "2����3����4")
					     ((and 3rd (= 2nd 3rd))
					      (setq 3rd nil)
					      "2����3")
					     ((and 4th (= 2nd 4th))
					      (setq 4th nil)
					      "2����4")))
				    (list tcode-help-second-stroke "2")))))))
    (and 3rd
	 (setq draw-data
	       (nconc draw-data
		      (list (cons 3rd
				  (if (and 4th (= 3rd 4th))
				      (list
				       (if (null 2nd)
					   tcode-help-another-double-stroke
					 tcode-help-double-stroke)
				       (progn
					 (setq 4th nil)
					 "3����4"))
				    (list tcode-help-third-stroke "3")))))))
    (and 4th
	 (setq draw-data
	       (nconc draw-data
		      (list (list 4th tcode-help-forth-stroke "4")))))
    draw-data))

(defun tcode-help-stroke (loc ch)
  "subroutine of `tcode-draw-stroke-for-char'."
  (goto-char (point-min))
  (forward-line (car loc))
  (move-to-column (+ (* 2 (cdr loc)) (if (>= (cdr loc) 6) 0 -2)))
  (tcode-delete-char (if (= (char-width (tcode-following-char)) 2) 
			 1 
		       2))
  (insert ch))

(defun tcode-get-key-location (address)
  (cons (/ address 10) (1+ (% address 10))))

(defun tcode-draw-stroke-for-char (stroke)
  "STROKE ���Ǥ�����ɽ���ޤ�������"
  (let ((draw-data (tcode-make-drawing-data stroke))
	(i 0))
    (insert "\
                      \n\
��������      ��������
��������      ��������
��������      ��������")
    (while draw-data
      (let* ((datum (car draw-data))
	     (addr (car datum))
	     (char (car (cdr datum)))
	     (str (car (cdr (cdr datum)))))
	(tcode-help-stroke (tcode-get-key-location addr) char)
	(goto-char (point-min))
	(forward-line (if (= (mod i 2) 0) 2 3))
	(end-of-line)
	(insert "     " char "����" str "�Ǹ�")
	(setq i (1+ i)
	      draw-data (cdr draw-data))))))

;;;###autoload
(defun tcode-display-key-for-char (ch &optional not-display)
  "CH (1ʸ��)���Ǥ����򥭡������Ͻ��ɽ�����롣"
  (interactive "sHelp:\nP")
  (if (or (null ch)
	  (string= ch ""))
      (progn
	(tcode-verbose-message "�إ�פ�ʸ��������ޤ���")
	(ding))
    (let* ((strokes (tcode-encode (tcode-string-to-char ch)))
	   (msg (if strokes
		    (format "%s = %s" ch (tcode-stroke-to-string strokes))
		  (let ((decomposed-char (tcode-help-bushu-help-string
					  ch
					  (and tcode-strict-help 
					       (not strokes)))))
		    (format (if decomposed-char
				(concat ch " = " decomposed-char)
			      ch))))))
      (setq tcode-last-help-char-list (list (tcode-string-to-char ch)))
      (if not-display
	  msg
	(message msg)))))

;;;###autoload
(defun tcode-display-stroke-sequence (char-list &optional append)
  (if tcode-help-with-real-keys
      (message (mapconcat (lambda (ch)
			    (tcode-display-key-for-char (char-to-string ch) t))
			  char-list 
			  "  "))
    (mapcar (lambda (ch)
	      (tcode-display-stroke-for-char (char-to-string ch)
					     append
					     t)
	      (setq append t))
	    char-list)))

;;;###autoload
(defun tcode-display-direct-stroke (kakutei &optional yomi append)
  "KAKUTEI ����ǡ� YOMI �˴ޤޤ줺������ľ�����ϤǤ��������ɽ�����롣"
  (when (not (window-minibuffer-p (selected-window)))
    (let* ((kakutei-list (string-to-list kakutei))
	   (yomi-list (string-to-list yomi))
	   display-list)
      (while kakutei-list
	(let ((ch (car kakutei-list)))
	  (if (and (not (memq ch yomi-list))
		   (tcode-encode ch))
	      (setq display-list (cons ch display-list)))
	  (setq kakutei-list (delq ch kakutei-list))))
      (setq display-list (nreverse display-list))
      (tcode-display-stroke-sequence display-list append)
      (setq tcode-last-help-char-list display-list))))

;;;###autoload
(defun tcode-display-stroke-for-char (ch &optional append recursive)
  "CH (1ʸ��)���Ǥ�����ɽ�����롣"
  (interactive "sHelp:\nP")
  (run-hooks 'tcode-help-stroke-hook)
  (if tcode-help-with-real-keys
      (tcode-display-key-for-char ch)
    (if (or (null ch)
	    (string= ch ""))
	(progn
	  (tcode-verbose-message "�إ�פ�ʸ��������ޤ���")
	  (ding))
      (let* ((strokes (tcode-encode (tcode-string-to-char ch)))
	     (buf (get-buffer-create " *tcode: stroke*"))
	     decomposed-string)
	(with-current-buffer buf
	  (erase-buffer)
	  (and strokes
	       (<= (length strokes) 4)
	       (tcode-draw-stroke-for-char strokes))
	  (tcode-bushu-init 2)
	  (setq decomposed-string
		(if strokes
		    (concat "{"
			    (mapconcat 'char-to-string
				       (tcode-bushu-for-char
					(tcode-string-to-char ch))
				       ", ")
			    "}")
		  (tcode-help-bushu-help-string ch tcode-strict-help)))
	  (let ((help-string (if decomposed-string
				 (concat ch " = " decomposed-string)
			       (concat ch
				       " = {"
				       (mapconcat 'char-to-string
						  (tcode-bushu-for-char
						   (tcode-string-to-char ch))
						  ", ")
				       "}"))))
	    (if (null strokes)
		(message help-string)
	      (goto-char (point-min))
	      (end-of-line)
	      (insert "     " help-string)
	      (tcode-display-help-buffer " *tcode: stroke*" t append))))
	(if (and decomposed-string
		 (not recursive)
		 (not strokes))
	  (tcode-display-direct-stroke decomposed-string "()[]{}, +" strokes))
	(setq tcode-last-help-char-list (list (tcode-string-to-char ch)))))))

;;;###autoload
(defun tcode-query-stroke (p)
  "����P�ˤ���ʸ�����Ǥ�����ɽ�����롣"
  (interactive "d")
  (let ((ch (buffer-substring (save-excursion
				(goto-char p)
				(tcode-forward-char 1)
				(point))
			      p)))
    (tcode-display-stroke-for-char ch)))

(defun tcode-auto-remove-help-char ()
  "`tcode-last-help-char-list'�˴ޤޤ��ʸ����õ���ƾõ�롣"
  (and (eq tcode-auto-help 'delete-the-char)
       (let ((chars (reverse tcode-last-help-char-list)))
	 (while chars
	   (search-backward (char-to-string (car chars)))
	   (delete-region (match-beginning 0)
			  (match-end 0))
	   (setq chars (cdr chars)))
	 (ding))))

;;;
;;; ���ȥ���ɽ
;;;

;;;###autoload
(defun tcode-show-tables (first second)
  (interactive)
  (let* ((buf (tcode-draw-tables first second)))
    (if tcode-auto-zap-table
	(save-window-excursion
	  (tcode-display-help-buffer buf)
	  (sit-for 0)
	  (tcode-redo-command (read-char)))
      (let ((orig-buf (current-buffer)))
	(set-buffer buf)
	(goto-char (point-min))
	(set-buffer orig-buf))
      (tcode-display-help-buffer buf))))

(defun tcode-make-table-line (k1 k2)
  "subroutine of `tcode-make-LR-block'."
  (let ((i1 k1) (j 0) c)
    (while (< j 5)
      (let ((a (cdr (tcode-decode (list i1 k2)))))
	(setq c (if (null a)
		    "��"
		  (tcode-action-to-printable a)))
	(insert c)
	(if (= (char-width (tcode-string-to-char c)) 1) (insert " ")))
      (setq j (1+ j)
	    i1 (1+ i1)))))

(defun tcode-make-LR-block (k1-start k2-start)
  "subroutine of `tcode-insert-stroke-file'."
  (or tcode-help-draw-top-keys
      (setq k1-start (+ k1-start 10)
	    k2-start (+ k2-start 10)))
  (let ((k1 k1-start)
	(k2 k2-start)
	y x yy)
    (setq yy (if tcode-help-draw-top-keys 0 1))
    (while (< yy 4)
      (setq y (if tcode-help-draw-top-keys 0 1))
      (setq k1 k1-start)
      (while (< y 4)
	(setq k2 k2-start)
	(insert "\n  ")
	(setq x 0)
	(while (< x 5)
	  (tcode-make-table-line k1 k2)
	  (insert "  ")
	  (setq x (1+ x))
	  (setq k2 (1+ k2)))
	(setq y (1+ y))
	(setq k1 (+ 10 k1)))
      (setq k2-start (+ 10 k2-start))
      (insert "\n")
      (setq yy (1+ yy)))))

(defun tcode-insert-stroke-file (&optional force)
  "Make the stroke table and writeout the content into the file.
The file is updated if it is older than tcode table."
  (erase-buffer)
  (if (and (not force)
	   (file-exists-p tcode-stroke-file-name))
      (insert-file-contents tcode-stroke-file-name)
    (insert "\nLL\n")
    (tcode-make-LR-block 0 0)
    (insert "\nLR\n")
    (tcode-make-LR-block 0 5)
    (insert "\nRL\n")
    (tcode-make-LR-block 5 0)
    (insert "\nRR\n")
    (tcode-make-LR-block 5 5)
    (if (file-writable-p tcode-stroke-file-name)
	(write-file tcode-stroke-file-name))))

(defun tcode-draw-tables (first second &optional force)
  "Draw tcode stroke tables.
FIRST corresponds to the first stroke.  If nil, then left, else right.
SECOND to the second stroke.  If nil, then left, else right.
If FORCE is non-nil, make new table."
  (let ((buf (get-buffer-create tcode-stroke-buffer-name))
	(str (concat "^" (if first "R" "L") (if second "R" "L"))))
    (with-current-buffer buf
      (widen)
      (goto-char (point-min))
      (if (and (not force)
	       (re-search-forward str nil t))
	  (progn
	    (forward-line 0)
	    (let ((top (point))
		  (end
		   (progn
		     (forward-line 3)
		     (if (re-search-forward "^[RL]" nil t)
			 (progn(forward-line -1)(point))
		       (point-max)))))
	      (narrow-to-region top end)
	      buf))
	(tcode-insert-stroke-file force)
	(tcode-draw-tables first second)))))

(provide 'tc-help)

;;; tc-help.el ends here
