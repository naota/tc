;;; tc-mazegaki.el --- mazegaki conversion in T-Code

;; Copyright (C) 1996--2003 Kaoru Maeda, Yasushi Saito and KITAJIMA Akira.

;; Author: Kaoru Maeda <maeda@src.ricoh.co.jp>
;;	Yasushi Saito <yasushi@is.s.u-tokyo.ac.jp>
;;	KITAJIMA Akira <kitajima@isc.osakac.ac.jp>
;; Maintainer: KITAJIMA Akira
;; Created: 30 Apr 1996
;; Version: $Id: tc-mazegaki.el,v 1.38 2003/05/18 08:46:08 kitajima Exp $
;; Keywords: wp, japanese, input method

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

(defgroup mazegaki-conversion nil
  "�򤼽��Ѵ�"
  :group 'tcode)

;;; �������ޥ����Ǥ����ѿ�

(defvar tcode-mazegaki-selected-candidate-register ?\[
  "* �򤼽��Ѵ��ǺǸ�˳��ꤷ��ʸ�������¸���Ƥ����쥸������
nil �ξ��ˤ���¸����ʤ���")

(defvar tcode-mazegaki-dictionary-name "mazegaki.dic"
  "�򤼽��Ѵ�����Υե�����̾��")

(defconst tcode-mazegaki-buffer-name " *tcode: mazegaki dictionary*")
;; �򤼽񤭼���ΥХåե�̾

(unless (assq tcode-mazegaki-buffer-name tcode-dictionaries)
  (setq tcode-dictionaries (cons (cons tcode-mazegaki-buffer-name
				       tcode-mazegaki-dictionary-name)
				 tcode-dictionaries)))

(defcustom tcode-mazegaki-yomi-max 10 "* �򤼽��Ѵ����ɤߤκ���ʸ������"
  :group 'mazegaki-conversion)

(defvar tcode-mazegaki-terminate-char-list
  (mapcar (lambda (ch) (tcode-string-to-char ch))
	  '("��" "��" "��" "��" "��" "��" "��" "��" "��"))
  "* �򤼽��Ѵ����ɤߤ˴ޤޤ�ʤ�2�Х���ʸ���Υꥹ�ȡ�")

(defcustom tcode-mazegaki-init-hook nil
  "* �ǽ�� tc-mazegaki.el ����ɤ���Ȥ��˸ƤФ�� hook��"
  :type 'hook :group 'mazegaki-conversion)

(defvar tcode-mazegaki-command-summary-alist
  '(("�̤��" . tcode-mazegaki-relimit-left)
    ("���Ф�" . tcode-mazegaki-relimit-right)
    ("����"   . tcode-mazegaki-finish)
    ("�᤹"   . tcode-mazegaki-restore-yomi-and-quit)
    ("����"   . tcode-mazegaki-table-mode)
    ("��ɽ�ޤ��Ͻ̤�" . tcode-mazegaki-select-candidate-or-relimit)
    ("��Ͽ&����" . tcode-mazegaki-make-entry-and-finish))
  "* `tcode-mazegaki-command-summary' ��ɽ������뵡ǽ�� alist��")

(defvar tcode-mazegaki-enable-inflection t
  "* nil �Ǥʤ��Ȥ������Ѹ����Ѵ����Ǥ��롣")

(defvar tcode-mazegaki-prefix-mark
  (if (or (tcode-nemacs-p)
	  (tcode-mule-1-p)
	  (and (boundp 'window-system)
	       (not window-system)))
      (if (fboundp 'make-glyph)
	  (make-glyph "��")
	"��")
    nil)
  "* �򤼽��Ѵ��λ�����ɽ�����뤷��")

(defvar tcode-mazegaki-face
  (if (or (tcode-nemacs-p)
	  (tcode-mule-1-p)
	  (and (boundp 'window-system)
	       (not window-system)))
      nil
    (prog1
	(make-face 'mazegaki-conversion)
      (set-face-underline-p 'mazegaki-conversion t)))
  "* �򤼽��Ѵ����Ѵ��оݤ�ɽ��ʸ������Ѥ���face��
mule2 �ʾ�ޤ��� XEmacs �ξ��Τ�ͭ����")
(defvar tcode-mazegaki-prefix-overlay nil)

(defvar tcode-mazegaki-stroke-priority-list
; ��������
;  0  1  2  3  4    5  6  7  8  9
; 10 11 12 13 14   15 16 17 18 19
; 20 21 22 23 24   25 26 27 28 29
; 30 31 32 33 34   35 36 37 38 39
  '(22 23 21 24 20
    12 13 11 14 10
    27 26 28 25 29
    17 16 18 15 19)
  "* ������¤٤�Ȥ��ΰ��֡�
���Υꥹ�Ȥˤʤ������ϻ��Ѥ���ʤ���")

(defvar tcode-mazegaki-alternative-select-first-keys
  '(22)
  "* �������Ĥ��椫�������־��Ρ���¦(��1����)�����֥����Υꥹ�ȡ�")
(defvar tcode-mazegaki-alternative-select-second-keys
  '(23)
  "* �������Ĥ��椫�������־��Ρ���¦(��2����)�����֥����Υꥹ�ȡ�")

(defvar tcode-mazegaki-complete-max 10
  "* �䴰�κݤ˰�������ο��κ����͡�
���䤬�����Ͱʾ�ʤ��䴰�ϹԤ�ʤ���")

(defconst tcode-mazegaki-inflection-mark "��"
  "�ɤߤ����Ѥ�����ˡ����Ѹ�����ɽ������ˤĤ���ʸ����")

(defvar tcode-mazegaki-max-suffix-length 4
  "* �ɤߤ���γ��Ѹ����κ���ʸ������")

(defcustom tcode-mazegaki-fixed-priority-count 4
  "* �ؽ�����Ȥ��ˡ��ؽ����оݳ��Ȥʤ����ο���
���ߤ��ɤߤˤĤ��ơ����򤵤줿����ν��֤����ο��ʲ��ʤ�С��ؽ�����ʤ���"
  :group 'mazegaki-conversion)

(defcustom tcode-mazegaki-inline-candidate-count 0
  "* ���䤬ʣ�����äƤ⡢��1����򥤥�饤��ɽ��������"
  :group 'mazegaki-conversion)

(defvar tcode-mazegaki-splitter "|"
  "* ��Ͽ�κݤΡ��ɤߤ���Ӵ����ζ��ڤ��ɽ������ɽ��")

;;; ����¾���ѿ�

(defvar tcode-mazegaki-yomi-list nil
  "�Ѵ����ϻ����ǥ������������ˤ��ä�ʸ���Υꥹ�ȡ�
���������ľ����ʸ���� car���������� cadr���ʲ�Ʊ�͡�
���� `tcode-mazegaki-yomi-max' ʸ���ޤǡ�")

(defvar tcode-mazegaki-current-yomi-length 0
  "�����Ѵ��оݤȤʤäƤ���ʸ�����Ĺ����
\(length tcode-mazegaki-yomi-list\)�ʲ��Ǥ��롣")
(defvar tcode-mazegaki-current-offset 0
  "`tcode-mazegaki-yomi-list' �β����ܤ�ʸ�������ɤߤȤߤʤ�����")
(defvar tcode-mazegaki-current-yomi-point nil)

(defvar tcode-mazegaki-yomi-fixed nil
  "����Ĺ�ɤ��Ѵ���ʤ�� nil�������Ǥʤ��ʤ� t��
�Ĥޤꡢ��fj�פ����֤����ɤߤ����Ϥ��줿���ˤϤ����ѿ����ͤ� t �Ǥ��ꡢ
����ʳ��ξ��� nil �Ǥ��롣")

(defvar tcode-mazegaki-map nil "�򤼽��Ѵ���Υ����ޥåס�")
(defvar tcode-mazegaki-mode nil "�򤼽��Ѵ��椫�ɤ�����ɽ���ѿ���
�ºݤˤϡ��򤼽��Ѵ��оݤ���Ƭ�Υݥ���Ȥ��ݻ����Ƥ��롣")
(make-variable-buffer-local 'tcode-mazegaki-mode)

(defvar tcode-mazegaki-prefix nil)
(make-variable-buffer-local 'tcode-mazegaki-prefix)

(defvar tcode-mazegaki-suffix ""
  "�Ѵ����оݳ��Ȥʤ������졣")

(defvar tcode-mazegaki-conversion-overlay nil
  "�򤼽��Ѵ����Ѵ��оݤ�ɽ�� overlay��")
(make-variable-buffer-local 'tcode-mazegaki-conversion-overlay)

(defvar tcode-mazegaki-inflection-only nil
  "�����Ѵ��椫�ɤ�����")

(defvar tcode-mazegaki-with-inflection nil
  "���Ѥ����ɤߤ��Ѵ��оݤȤ��Ƥ��뤫�ɤ�����")

(defvar tcode-mazegaki-candidate-state nil
  "�򤼽񤭸���ɽ�����ơ��ȡ�
'next     ������ɽ���Τ������־���
'one      ���䤬�ҤȤĤ����ʤ�����饤��ɽ����
'priority ʣ������κǽ�����򥤥�饤��ɽ����
'table    ɽ������������

�Ѵ�����(�Ѵ�ľ��ޤ����ɤ�Ĺ���ѹ�ľ��)
  �� ������ˤ�ä�one�ޤ���next

next
  candidate-index���������꾮��������
  inline-candidate-count��꾮����
    �� priority
  else
    �� table

one
  SPC����
    �� �̤�
  = ����
    �� one

priority
  SPC����
    �� candidate-index++����next
  = ����
    �� table

table
  SPC����
    �� ���ڡ���
")

(defvar tcode-mazegaki-candidate-index nil
  "priority���ơ��Ȥǥ���饤��ɽ����θ���ϲ����ܤ�(0���ꥸ��)��
See also variable `tcode-mazegaki-candidate-state'.")

(defvar tcode-mazegaki-start-marker nil)
(make-variable-buffer-local 'tcode-mazegaki-start-marker)

;;;; ����θ������Ѵ�

(cond ((or (tcode-nemacs-p) (tcode-mule-1-p))
       (defun tcode-mazegaki-delete-conversion-face ()
	 (if tcode-mazegaki-prefix-overlay
	     (save-excursion
	       (goto-char tcode-mazegaki-prefix-overlay)
	       (if (looking-at (regexp-quote tcode-mazegaki-prefix-mark))
		   (delete-char 1))))
	 (setq tcode-mazegaki-prefix-overlay nil))
       (defun tcode-mazegaki-put-conversion-face ()
	 (let ((point (or tcode-mazegaki-mode
			  tcode-mazegaki-prefix)))
	   (when point
	     (save-excursion
	       (goto-char point)
	       (tcode-mazegaki-delete-conversion-face)
	       (setq tcode-mazegaki-prefix-overlay (point))
	       (insert tcode-mazegaki-prefix-mark))))))
      ((tcode-xemacs-p)
       (defun tcode-mazegaki-put-conversion-face ()
	 "�Ѵ����ϰ��֤��� point �ޤǤ���ꤵ�줿 face �ˤ��롣
face �λ�����ѿ� `tcode-mazegaki-prefix-mark' �����ꤹ�뤳�Ȥˤ��Ԥ���"
	 (let ((point (or tcode-mazegaki-mode
			  tcode-mazegaki-prefix)))
	   (when point
	     (when tcode-mazegaki-prefix-mark
	       (setq tcode-mazegaki-prefix-overlay
		     (if (extentp tcode-mazegaki-prefix-overlay)
			 (set-extent-endpoints tcode-mazegaki-prefix-overlay
					       point
					       point)
		       (make-extent point point)))
	       (set-extent-begin-glyph tcode-mazegaki-prefix-overlay
				       tcode-mazegaki-prefix-mark))
	     (if (extentp tcode-mazegaki-conversion-overlay)
		 (set-extent-endpoints tcode-mazegaki-conversion-overlay
				       point
				       (point))
	       (setq tcode-mazegaki-conversion-overlay
		     (make-extent point (point)))
	       (set-extent-face tcode-mazegaki-conversion-overlay
				tcode-mazegaki-face)))))
       (defun tcode-mazegaki-delete-conversion-face ()
	 "�򤼽��Ѵ��Ѥ� face �� prefix �������"
	 (if (extentp tcode-mazegaki-prefix-overlay)
	     (detach-extent tcode-mazegaki-prefix-overlay))
	 (if (extentp tcode-mazegaki-conversion-overlay)
	     (detach-extent tcode-mazegaki-conversion-overlay))))
      (t
       (unless (fboundp 'make-overlay)
	 (require 'overlay))
       ;; face �ˤ���Ѵ��о��ΰ��ɽ�����뤿��δؿ������
       (defun tcode-mazegaki-put-conversion-face ()
	 "�Ѵ����ϰ��֤��� point �ޤǤ���ꤵ�줿 face �ˤ��롣
face �λ�����ѿ� `tcode-mazegaki-face' �����ꤹ�뤳�Ȥˤ��Ԥ���
�ѿ� `tcode-mazegaki-prefix-mark' �����ꤵ��Ƥ���С�����ʸ�����
��Ƭ���֤���"
	 (let ((point (or tcode-mazegaki-mode
			  tcode-mazegaki-prefix)))
	   (when point
	     ;; prefix ���֤���
	     (when tcode-mazegaki-prefix-mark
	       (setq tcode-mazegaki-prefix-overlay
		     (if (overlayp tcode-mazegaki-prefix-overlay)
			 (move-overlay tcode-mazegaki-prefix-overlay
				       point
				       point)
		       (make-overlay point point)))
	       (overlay-put tcode-mazegaki-prefix-overlay
			    'before-string
			    tcode-mazegaki-prefix-mark))
	     ;; face �����ꤹ�롣
	     (if (overlayp tcode-mazegaki-conversion-overlay)
		 (move-overlay tcode-mazegaki-conversion-overlay point (point))
	       (setq tcode-mazegaki-conversion-overlay
		     (make-overlay point (point)))
	       (overlay-put tcode-mazegaki-conversion-overlay
			    'face
			    tcode-mazegaki-face)))))
       (defun tcode-mazegaki-delete-conversion-face ()
	 "�򤼽��Ѵ��Ѥ� face �� prefix �������"
	 (if (overlayp tcode-mazegaki-prefix-overlay)
	     (delete-overlay tcode-mazegaki-prefix-overlay))
	 (if (overlayp tcode-mazegaki-conversion-overlay)
	     (delete-overlay tcode-mazegaki-conversion-overlay)))))

;;;###autoload
(defun tcode-mazegaki-switch-to-dictionary ()
  "current-buffer ��򤼽񤭼�����ڤ��ؤ��롣
�򤼽񤭼��񤬤ޤ��ɤ߹��ޤ�Ƥ��ʤ����ˤ��ɤ߹��ࡣ"
  (interactive)
  (let ((buffer (tcode-set-work-buffer tcode-mazegaki-buffer-name
				       tcode-mazegaki-dictionary-name)))
    (if (interactive-p)
	(switch-to-buffer buffer))))

(defun tcode-mazegaki-construct-yomi (len &optional offset inflection)
  "`tcode-mazegaki-yomi-list' ���顢Ĺ�� LEN ���ɤߤ��롣
OFFSET �����ꤵ��Ƥ�����ϡ�OFFSET ʸ���ϳ��Ѹ����Ȥ����ɤߤˤϴޤ᤺��
INFLECTION �� nil �Ǥʤ���� `tcode-mazegaki-inflection-mark' ���֤������롣"
  (let ((list (mapcar 'cdr
		      (if offset
			  (nthcdr offset tcode-mazegaki-yomi-list)
			tcode-mazegaki-yomi-list)))
	(str ""))
    (while (> len 0)
      (setq str (concat (car list) str)
	    list (cdr list)
	    len (1- len)))
    (if inflection
	(concat str tcode-mazegaki-inflection-mark)
      str)))

(defun tcode-mazegaki-list-to-string (list from len)
  "ʸ���� LIST �� FROM ���� LEN ʸ��ʬ��ʸ������롣"
  (let ((str ""))
    (setq list (nthcdr from list))
    (while (> len 0)
      (setq str  (concat (cdr (car list)) str)
	    list (cdr list)
	    len  (1- len)))
    str))

(defun tcode-mazegaki-get-reverse-yomi-list ()
  "�� point �������ˤ������ܸ���ޤ��ϱ�ñ���ĤΥꥹ�Ȥ��֤���
�ꥹ�Ȥ�Ĺ���Ϻ��� `tcode-mazegaki-yomi-max' ʸ����"
  (setq tcode-mazegaki-suffix ""
	tcode-mazegaki-yomi-fixed tcode-mazegaki-mode)
  (save-excursion
    (let (context reverse-list)
      (catch 'finish
	(while (and (< (length reverse-list)
		       tcode-mazegaki-yomi-max)
		    (setq context
			  (tcode-scan-backward
			   1 tcode-mazegaki-terminate-char-list)))
	  (let ((ch (cdr (car context)))
		(point (car (car context))))
	    (if (and reverse-list
		     (or (= (point) point)
			 (>= (count-lines point (point)) (if (bolp) 2 3))))
		(throw 'finish nil))
	    (if (and (null reverse-list)
		     (memq (tcode-string-to-char ch)
			   tcode-mazegaki-terminate-char-list))
;; `tcode-mazegaki-terminate-char-list' �ˤ���ʸ����ȤФ���
		(setq tcode-mazegaki-suffix
		      (concat ch tcode-mazegaki-suffix))
	      ;; �ɤߤ�1������
	      (setq reverse-list (cons (car context) reverse-list))
	      (if (and tcode-mazegaki-prefix
		       (= point tcode-mazegaki-prefix))
		  (throw 'finish nil)))
	    (goto-char point))))
      (nreverse reverse-list))))

(defun tcode-mazegaki-search-yomi (yomi)
  "���ߤΥХåե����� YOMI �򸫤Ĥ��� point ���ư���롣
���Ĥ���ʤ����ϡ� point �Ϥ��� YOMI ������٤����˰�ư���롣"
  (let ((min (point-min))
	(max (point-max))
	str)
    (catch 'found
      (and (eobp)
	   (forward-line -1))
      (while (< min max)
	(beginning-of-line)
	(cond ((string< (setq str (buffer-substring
				   (point)
				   (progn
				       (while (/= (tcode-following-char) ?/)
					 (tcode-forward-char 1))
				       (1- (point)))))
			yomi)
	       ;; ��ä��礭��
	       (forward-line 1)
	       (goto-char (ash (+ (setq min (point)) max) -1)))
	      ((string< yomi str)
	       ;; ��äȾ�����
	       (beginning-of-line)
	       (goto-char (ash (+ min (setq max (point))) -1)))
	      ;; ���Ĥ���
	      (t
	       (beginning-of-line)
	       (throw 'found (point))))))))

(defun tcode-mazegaki-lookup (new)
  "���ߤ��ɤߤ��û����Ĺ���ɤߤ�õ����
NEW �� nil �Ǥʤ���С�������õ���Ϥ�롣
���Ĥ���Ф����ɤߤ�Ĺ������� offset (��� 0) �򡢤ʤ���� nil ���֤���
�򤼽񤭼���Υݥ���ȤϤ����ɤߤΤȤ���˰�ư���롣"
  (save-excursion
    (setq tcode-mazegaki-with-inflection nil)
    (tcode-mazegaki-switch-to-dictionary)
    (let* ((max (length tcode-mazegaki-yomi-list))
	   (min (if tcode-mazegaki-yomi-fixed (1- max) 0))
	   (i (if new max (1- tcode-mazegaki-current-yomi-length))))
      (catch 'found
	(while (> i min)
	  (when (tcode-mazegaki-search-yomi (tcode-mazegaki-construct-yomi i))
	    (setq tcode-mazegaki-current-yomi-point (point))
	    (throw 'found (cons (car (nth (1- i) tcode-mazegaki-yomi-list))
				(cons i 0))))
	  (setq i (1- i)))))))

(defun tcode-mazegaki-lookup-with-inflection (new)
  "���ߤ��ɤߤ��û�������Ѥ����Ĺ���ɤߤ�õ����
NEW �� nil �Ǥʤ���С�������õ���Ϥ�롣
���Ĥ���Ф����ɤߤ�Ĺ������� offset �򡢤ʤ���� nil ���֤���
�򤼽񤭼���Υݥ���ȤϤ����ɤߤΤȤ���˰�ư���롣"
  (save-excursion
    (setq tcode-mazegaki-with-inflection t)
    (tcode-mazegaki-switch-to-dictionary)
    (let* ((max (length tcode-mazegaki-yomi-list))
	   (min 0)
	   (i (if new max tcode-mazegaki-current-yomi-length))
	   (offset (cond (new
			  (cond ((>= (+ i tcode-mazegaki-max-suffix-length)
				     max)
				 (- max i))
				(tcode-mazegaki-yomi-fixed
				 -1)
				(t
				 tcode-mazegaki-max-suffix-length)))
			 (tcode-mazegaki-yomi-fixed
			  -1)
			 (t
			  (1- tcode-mazegaki-current-offset)))))
      (catch 'found
	(while (> i min)
	  (while (>= offset 0)
	    (and (string-match "^[��-��]*$"
			       (tcode-mazegaki-construct-yomi offset))
		 (tcode-mazegaki-search-yomi
		  (tcode-mazegaki-construct-yomi i offset t))
		 (setq tcode-mazegaki-current-yomi-point (point))
		 (throw 'found (cons (car (nth (1- (+ i offset))
					       tcode-mazegaki-yomi-list))
				     (cons i offset))))
	    (setq offset (if tcode-mazegaki-yomi-fixed -1 (1- offset))))
	  (setq i (1- i)
		offset (cond ((>= (+ i tcode-mazegaki-max-suffix-length)
				  max)
			      (- max i))
			     (tcode-mazegaki-yomi-fixed
			      -1)
			     (t
			      tcode-mazegaki-max-suffix-length))))))))

(defun tcode-mazegaki-lookup-reverse (new)
  "���ߤ��ɤߤ���Ĺ����û���ɤߤ򸫤Ĥ��롣
���Ĥ���Ф����ɤߤ�Ĺ������� offset (��� 0) �򡢤ʤ���� nil ���֤���
�򤼽񤭼���Υݥ���ȤϤ����ɤߤΤȤ���˰�ư���롣"
  (save-excursion
    (setq tcode-mazegaki-with-inflection nil)
    (tcode-mazegaki-switch-to-dictionary)
    (let* ((max (length tcode-mazegaki-yomi-list))
	   (i (cond (new (if tcode-mazegaki-yomi-fixed max 1))
		    (tcode-mazegaki-yomi-fixed (1+ max))
		    (t (1+ tcode-mazegaki-current-yomi-length)))))
      (catch 'found
	(while (<= i max)
	  (when (tcode-mazegaki-search-yomi (tcode-mazegaki-construct-yomi i))
	    (setq tcode-mazegaki-current-yomi-point (point))
	    (throw 'found (cons (car (nth (1- i) tcode-mazegaki-yomi-list))
				(cons i 0))))
	  (setq i (1+ i)))))))

(defun tcode-mazegaki-lookup-with-inflection-reverse (new)
  "���ߤ��ɤߤ���Ĺ����û�γ��Ѥ����ɤߤ򸫤Ĥ��롣
���Ĥ���Ф����ɤߤ�Ĺ������� offset �򡢤ʤ���� nil ���֤���
�򤼽񤭼���Υݥ���ȤϤ����ɤߤΤȤ���˰�ư���롣"
  (save-excursion
    (setq tcode-mazegaki-with-inflection t)
    (tcode-mazegaki-switch-to-dictionary)
    (let* ((max (length tcode-mazegaki-yomi-list))
	   (i (if new 1 tcode-mazegaki-current-yomi-length))
	   (offset (cond (new (cond ((not tcode-mazegaki-yomi-fixed)
				     0)
				    ((<= (- max i)
					tcode-mazegaki-max-suffix-length)
				     (- max i))
				    (t
				     max)))
			 ((or tcode-mazegaki-yomi-fixed
			      (>= tcode-mazegaki-current-offset
				  tcode-mazegaki-max-suffix-length))
			  max)
			 (t (1+ tcode-mazegaki-current-offset)))))
      (catch 'found
	(while (<= i max)
	  (while (<= (+ i offset) max)
	    (and (string-match "^[��-��]*$"
			       (tcode-mazegaki-construct-yomi offset))
		 (tcode-mazegaki-search-yomi
		  (tcode-mazegaki-construct-yomi i offset t))
		 (setq tcode-mazegaki-current-yomi-point (point))
		 (throw 'found (cons (car (nth (1- (+ i offset))
					       tcode-mazegaki-yomi-list))
				     (cons i offset))))
	    (setq offset (if (>= offset
				 tcode-mazegaki-max-suffix-length)
			     max
			   (1+ offset))))
	  (setq i (1+ i)
		offset (cond ((not tcode-mazegaki-yomi-fixed)
			      0)
			     ((<= (- max i)
				  tcode-mazegaki-max-suffix-length)
			      (- max i))
			     (t max))))))))

(defun tcode-mazegaki-erase-previous-candidate ()
  "���ߥХåե���ɽ������Ƥ����Ѵ������õ�롣"
  (if tcode-mazegaki-mode
      (delete-region tcode-mazegaki-mode (point))))

;;;###autoload
(defun tcode-mazegaki-convert (arg &optional inflection)
  "�� point �������ˤ������ܸ������ɤߡפȤ��Ƹ򤼽��Ѵ����롣

ARG �ϼ����̣���롣
 * C-u �Τߤޤ��� - �Τ�
   ���Ѥ����Ȥ����Ѵ�(�ɤߤ�Ĺ���Ϻ�Ĺ����)
 * ����
   ���������ͤ��ɤߤ�Ĺ���Ȥ����Ѵ�
   ���������Ѵ�����Τϡ����ξ��ϳ��Ѥ��ʤ��졢
   ��ξ��ϳ��Ѥ����Ȥ��롣

INFLECTION �� nil �Ǥʤ���С�ARG ���ͤˤ�餺�����Ѥ����Ȥ����Ѵ���Ԥ���"
  (interactive "*P")
  (tcode-mazegaki-candidate-select-init)
  (setq tcode-mazegaki-inflection-only (or (eq arg '-)
					   (and (integerp arg)
						(< arg 0))
					   (and arg
						(listp arg))
					   inflection))
  (let ((tcode-mazegaki-yomi-max (if (integerp arg)
				     (if (>= arg 0) arg (- arg))
				   tcode-mazegaki-yomi-max)))
    (unless (setq tcode-mazegaki-yomi-list
		  (tcode-mazegaki-get-reverse-yomi-list))
      (let (tcode-auto-help)
	(tcode-mazegaki-finish)
	(error "�ɤߤ�����ޤ���"))))
  (and (integerp arg)
       (or tcode-mazegaki-yomi-fixed
	   (= (length tcode-mazegaki-yomi-list)
	      (if (>= arg 0) arg (- arg)))
	   (error "�ɤߤ�û�������ޤ���"))
       (setq tcode-mazegaki-yomi-fixed t))
  ;; ������ɤߤ����뤫Ĵ�١�������Ѵ����롣
  (let ((with-inflection (and tcode-mazegaki-enable-inflection
			      (or inflection
				  (not (and (integerp arg)
					    (> arg 0)))))))
    (let* ((tcode-mazegaki-enable-inflection with-inflection)
	   (yomi-info (or (and (not tcode-mazegaki-inflection-only)
			       (tcode-mazegaki-lookup t))
			  (and (or tcode-mazegaki-enable-inflection
				   tcode-mazegaki-inflection-only)
			       (tcode-mazegaki-lookup-with-inflection t)))))
      (if yomi-info
	  ;; ���䤬���Ĥ��ä���
	  (prog1
	      (setq tcode-mazegaki-current-yomi-length (car (cdr yomi-info))
		    tcode-mazegaki-current-offset (cdr (cdr yomi-info)))
	    ;; i ʸ���θ������Ƭ�˰�(��)��Ĥ��롣
	    (save-excursion
	      (goto-char (car yomi-info))
	      (if tcode-mazegaki-mode
		  (if (/= tcode-mazegaki-mode (point))
		      (let (tcode-auto-help)
			(tcode-mazegaki-finish)
			(error "�ɤߤ�Ĺ�����ޤ���")))
		(setq tcode-mazegaki-mode (point))))
	    (condition-case nil
		(let ((echo-keystrokes 0))
		  (tcode-mazegaki-put-conversion-face)
		  (tcode-mazegaki-select-candidate)
		  (while tcode-mazegaki-mode
		    (let* ((keyseq (read-char))
			   (command (lookup-key tcode-mazegaki-map
						(char-to-string keyseq))))
		      (if (not (commandp command))
			  (progn
			    (tcode-mazegaki-finish)
			    (tcode-redo-command keyseq))
			(setq prefix-arg current-prefix-arg
			      this-command command)
			(command-execute command))))
		  (tcode-mazegaki-finish))
	      (quit
	       (ding)
	       (tcode-mazegaki-restore-yomi-and-quit))))
	;; ���䤬̵���ä���
	(setq this-command 'tcode-mazegaki-begin-conversion)
					; ��Ͽ��Ԥ��Ȥ����Ѥ���ٹ�
	(ding)
	(tcode-verbose-message
	 (tcode-substitute-command-keys
	  (concat "Ŭ���ʴ����Ϥ���ޤ���"
		  " (��\\[tcode-mazegaki-make-entry-and-finish]�פ���Ͽ)")))
	nil))))

;;;###autoload
(defun tcode-mazegaki-begin-conversion (arg)
  "�򤼽��Ѵ��򳫻Ϥ��롣"
  (interactive "*P")
  (undo-boundary)
  (cond (tcode-mazegaki-mode
	 (if tcode-mazegaki-yomi-list
	     (tcode-mazegaki-get-reverse-yomi-list))
	 (tcode-mazegaki-convert arg))
	(tcode-use-prefix-mazegaki
	 (tcode-mazegaki-put-prefix))
	(t
	 (tcode-mazegaki-convert arg))))

;;;###autoload
(defun tcode-mazegaki-begin-alternate-conversion (arg)
  "�򤼽��Ѵ��򳫻Ϥ��롣�����������ַ������ַ����ա�"
  (interactive "*P")
  (call-interactively (if tcode-use-prefix-mazegaki
			  'tcode-mazegaki-convert
			'tcode-mazegaki-put-prefix)))

;;;###autoload
(defun tcode-mazegaki-lookup-with-prefix (char-list)
  "CHAR-LIST���ɤߤ���Ƭ�ˤʤäƤ������Υꥹ�Ȥ��֤���"
  (let ((tcode-mazegaki-yomi-list 
	 (mapcar (lambda (e)
		   (cons 0 (char-to-string e)))
		 (reverse char-list)))
	(prefix (regexp-quote (mapconcat 'char-to-string char-list nil)))
	candidate-list
	result)
    (save-excursion
      (tcode-mazegaki-switch-to-dictionary)
      (tcode-mazegaki-search-yomi prefix)
      (while (looking-at prefix)
	(setq tcode-mazegaki-current-yomi-point (point)
	      candidate-list (nconc candidate-list 
				    (tcode-mazegaki-get-candidate-list)))
	(forward-line 1))
      ;; ��ʣ�κ��
      (while candidate-list
	(let ((candidate (car candidate-list)))
	  (setq result (nconc result (list candidate))
		candidate-list (delete candidate candidate-list)))))
    result))

;;;; ���������
(defun tcode-mazegaki-candidate-select-init ()
  "���ơ��Ⱦ�������ͤ��᤹��"
  (setq tcode-mazegaki-candidate-state nil
	tcode-mazegaki-candidate-index 0))

(defun tcode-mazegaki-find-kanji-entry ()
  "���ߤ��ɤߤ�(�ǽ��)����ȥ�ޤǸ򤼽񤭼���� point ���ư���롣
���Τˤϡ�point �Ϻǽ�Υ���ȥ����Ƭ�ˤ���\"/\"��ľ��˰�ư���롣"
  (tcode-mazegaki-switch-to-dictionary)
  (goto-char tcode-mazegaki-current-yomi-point)
  (beginning-of-line)
  (search-forward " /" nil t))

(defun tcode-mazegaki-get-number-of-candidate ()
  "���ߤ��ɤߤθ���ο������롣"
  (save-excursion
    (tcode-mazegaki-find-kanji-entry)
    (let ((noc 0)
	  (p (point)))
      (while (not (eolp))
	(if (= (following-char) ?/)
	    (setq noc (1+ noc)))
	(tcode-forward-char 1))
      noc)))

(defun tcode-mazegaki-get-candidate-list ()
  "���ߤ��ɤߤ������Υꥹ�Ȥ��롣"
  (save-excursion
    (tcode-mazegaki-find-kanji-entry)
    (let (list)
      (while (not (eolp))
	(setq list (nconc list
			  (list (buffer-substring
				 (point)
				 (prog2
				     (search-forward "/" nil t)
				     (1- (point))))))))
      list)))

(defun tcode-mazegaki-make-candidate-table (candidate-list)
  " CANDIDATE-LIST ��������ɽ���롣
�����ɽ�ˤ�������֤ϡ��ѿ� `tcode-mazegaki-stroke-priority-list' �˽�����"
  (let ((plist tcode-mazegaki-stroke-priority-list)
	(table (make-vector 40 nil)))
    (while (and candidate-list plist)
      (aset table (car plist) (car candidate-list))
      (setq candidate-list (cdr candidate-list)
	    plist (cdr plist)))
    table))

(defun tcode-mazegaki-select (candidate-table noc current-offset
					  &optional msg suffix)
  "CANDIDATE-TABLE �����������򤵤��롣
NOC (����ο�)�� CURRENT-OFFSET ���鸽�߲����ܤ�ɽ��ɽ�����Ƥ��뤫�׻����롣"
  (let* ((plist-size (length tcode-mazegaki-stroke-priority-list))
	 (whole-page (/ (+ noc (1- plist-size)) plist-size))
	 (page (- (1+ whole-page)
		  (/ (+ (- noc current-offset) (1- plist-size)) plist-size)))
	 (whole-table (or (and (catch 'found
				 ;; 3���ܰʳ���Ȥ����Ȥ�Τ���롣
				 (mapcar (lambda (e) (if (or (< e 20)
							(>= e 30))
						    (throw 'found t)))
					 tcode-mazegaki-stroke-priority-list)
				 nil)
			       (> whole-page 1))
			  ;; 3���ܰʳ��˸��䤬���뤫Ĵ�٤�
			  (catch 'found
			    (let ((i 0))
			      (while (< i 40)
				(and (aref candidate-table i)
				     (throw 'found t))
				(setq i (if (= i 19) 30 (1+ i)))))))))
    (if whole-table
	(progn
	  (if suffix
	      (setq msg (concat msg "  " suffix)))
	  (if (not (and (window-minibuffer-p (selected-window))
			(null msg)))
	      (message (or msg "")))
	  (tcode-display-help-buffer
	   (tcode-draw-table candidate-table page whole-page) t))
      (let ((candidate-list (mapcar (lambda (n)
				      (or (aref candidate-table n)
					  "-"))
				    '(20 21 22 23 24 25 26 27 28 29))))
	(message
	 (concat msg
		 (if (= whole-page 1)
		     ""
		   (format "(%d/%d)  " page whole-page))
		 (apply 'format
			(cons "[%s %s %s %s] %s  %s [%s %s %s %s]"
			      candidate-list))
		 "  "
		 suffix)))))
  (let* ((echo-keystrokes 0)
	 (ch (read-char))
	 (key (tcode-char-to-key ch)))
    (message "")
    (if (< key 0)
	ch
      (or (aref candidate-table (mod key 40))
	  ch))))

(defun tcode-mazegaki-make-table-and-select (&optional
					     msg candidate-list inline)
  "���ߤ��ɤߤ����������򤵤�������ʸ����ޤ���ʸ��(����)���֤���"
  (or candidate-list
      (setq candidate-list (tcode-mazegaki-get-candidate-list)))
  (let* ((noc (length candidate-list))
	 (plist-size (length tcode-mazegaki-stroke-priority-list))
	 (suffix (and tcode-mazegaki-with-inflection
		       (concat (tcode-mazegaki-construct-yomi
				tcode-mazegaki-current-yomi-length
				tcode-mazegaki-current-offset)
			       "("
			       (if (zerop tcode-mazegaki-current-offset)
				   tcode-mazegaki-inflection-mark
				 (tcode-mazegaki-construct-yomi
					      tcode-mazegaki-current-offset))
			       ")"))))
    (if (<= noc 1)
	(car candidate-list)
      (if (and inline
	       (= noc 2)
	       tcode-mazegaki-alternative-select-first-keys
	       tcode-mazegaki-alternative-select-second-keys)
	  (let ((first-candidate (car candidate-list))
		(second-candidate (car (cdr candidate-list))))
	    (tcode-mazegaki-erase-previous-candidate)
	    (insert "{" first-candidate "," second-candidate "}")
	    (if tcode-mazegaki-with-inflection
		(insert (tcode-mazegaki-construct-yomi
			 tcode-mazegaki-current-offset)
			tcode-mazegaki-suffix))
	    (tcode-mazegaki-put-conversion-face)
	    (let* ((c (read-char))
		   (key (tcode-char-to-key c)))
	      (tcode-mazegaki-restore-yomi-and-quit t)
	      (cond ((memq key tcode-mazegaki-alternative-select-first-keys)
		     first-candidate)
		    ((memq key tcode-mazegaki-alternative-select-second-keys)
		     second-candidate)
		    (t
		     c))))
	;; noc >(=) 2
	(save-excursion
	  (let ((current-offset 0)
		(candidate (tcode-mazegaki-select
			(tcode-mazegaki-make-candidate-table candidate-list)
			noc 0 msg suffix)))
	    (while (and (char-or-string-p candidate)
			(not (stringp candidate))
			(or (= candidate ? )
			    (= candidate ?\C-?)
			    (= candidate ?\C-h)))
	      (setq current-offset (if (= candidate ? )
				       (+ current-offset plist-size)
				     (- current-offset plist-size)))
	      (if (>= current-offset noc)
		  (setq current-offset 0))
	      (if (< current-offset 0)
		  (let ((v current-offset))
		    (while (< (setq v (+ v plist-size)) noc)
		      (setq current-offset v))))
	      (setq candidate
		    (tcode-mazegaki-select
		     (tcode-mazegaki-make-candidate-table
		      (nthcdr current-offset candidate-list))
		     noc current-offset msg suffix)))
	    (tcode-auto-remove-help t)
	    candidate))))))

(defun tcode-mazegaki-show-candidate-inline (candidate)
  "����򥤥�饤��ɽ�����롣"
  (tcode-mazegaki-erase-previous-candidate)
  (insert candidate
	  (tcode-mazegaki-list-to-string
	   tcode-mazegaki-yomi-list
	   0
	   tcode-mazegaki-current-offset)
	  tcode-mazegaki-suffix)
  (tcode-mazegaki-put-conversion-face))

(defun tcode-mazegaki-select-candidate ()
  "���ߤ��ɤߤ����������򤹤롣"
  (let* ((candidate-list (tcode-mazegaki-get-candidate-list))
	 (noc (length candidate-list))
	 (msg (and (not (window-minibuffer-p (selected-window)))
		   (tcode-verbose-message "(? �ǥإ��)"))))
    (if (eq tcode-mazegaki-candidate-state 'priority)
	(setq tcode-mazegaki-candidate-index 
	      (1+ tcode-mazegaki-candidate-index)
	      tcode-mazegaki-candidate-state 'next))
    (if (null tcode-mazegaki-candidate-state)
	(setq tcode-mazegaki-candidate-state (if (= noc 1) 'one 'next)))
    (if (eq tcode-mazegaki-candidate-state 'next)
	(setq tcode-mazegaki-candidate-state
	      (if (and (< tcode-mazegaki-candidate-index noc)
		       (< tcode-mazegaki-candidate-index
			  tcode-mazegaki-inline-candidate-count))
		  'priority
		'table)))
    (cond ((eq tcode-mazegaki-candidate-state 'one)
	   (tcode-mazegaki-show-candidate-inline (car candidate-list))
	   (when (and tcode-mazegaki-yomi-fixed
		      (not tcode-mazegaki-enable-inflection))
	     (tcode-mazegaki-finish)
	     (setq msg nil))
	   (if msg
	       (message msg)))
	  ((eq tcode-mazegaki-candidate-state 'priority)
	   (tcode-mazegaki-show-candidate-inline
	    (nth tcode-mazegaki-candidate-index candidate-list))
	   (if msg
	       (message msg)))
	  ((eq tcode-mazegaki-candidate-state 'table)
	   (let ((selected-candidate (tcode-mazegaki-make-table-and-select
				  msg candidate-list t)))
	     (cond ((stringp selected-candidate)
		    (tcode-mazegaki-show-candidate-inline selected-candidate)
		    (tcode-mazegaki-finish))
		   ((char-or-string-p selected-candidate)
		    (tcode-redo-command selected-candidate))))))))

(unless (memq 'tcode-mazegaki-select-candidate 
	      tcode-no-wait-display-help-command-list)
  (setq tcode-no-wait-display-help-command-list
	(cons 'tcode-mazegaki-select-candidate 
	      tcode-no-wait-display-help-command-list)))

(defun tcode-mazegaki-table-mode ()
  "�������ɽ�����ڤ괹���롣"
  (interactive)
  (cond ((eq tcode-mazegaki-candidate-state 'one)
	 ;; nop
	 )
	((eq tcode-mazegaki-candidate-state 'priority)
	 (setq tcode-mazegaki-candidate-state 'table
	       tcode-mazegaki-candidate-index 
	       (tcode-mazegaki-get-number-of-candidate))
	 (tcode-mazegaki-select-candidate))
	(t
	 ;; nop
	 )))

(defun tcode-mazegaki-select-candidate-from-table ()
  "���ߤ��ɤߤ���������ɽ������������������������򤹤롣"
  (interactive "*")
  (let ((selected-candidate (tcode-mazegaki-make-table-and-select
			 (and (not (window-minibuffer-p (selected-window)))
			      (tcode-verbose-message "(? �ǥإ��)"))
			 nil t))
	(noc (tcode-mazegaki-get-number-of-candidate)))
    (cond ((stringp selected-candidate)
	   (tcode-mazegaki-show-candidate-inline selected-candidate)
	   (unless (and (= noc 1)
			(or tcode-mazegaki-enable-inflection
			    (not tcode-mazegaki-yomi-fixed)))
	     (tcode-mazegaki-finish)))
	  ((char-or-string-p selected-candidate)
	   (tcode-redo-command selected-candidate)))))

;;;; �ɤߤζ��ڤ�ľ��������γ���

(defun tcode-mazegaki-restore-yomi-and-quit (&optional not-quit)
  "�ɤߤξ��֤��ᤷ�ơ��򤼽��Ѵ���λ���롣
NOT-QUIT �� nil �Ǥʤ��Ȥ��ϡ��ɤߤξ��֤��᤹�����ǡ���λ�Ϥ��ʤ���"
  (interactive "P")
  (tcode-mazegaki-erase-previous-candidate)
  (tcode-mazegaki-candidate-select-init)	; ǰ�Τ���
  (insert (tcode-mazegaki-list-to-string
	   tcode-mazegaki-yomi-list 0
	   (+ tcode-mazegaki-current-yomi-length
	      tcode-mazegaki-current-offset))
	  tcode-mazegaki-suffix)
  (tcode-mazegaki-put-conversion-face)
  (unless not-quit
    (tcode-mazegaki-delete-conversion-face)
    (unless (window-minibuffer-p (selected-window))
	(message ""))
    (tcode-do-auto-fill)
    (setq tcode-mazegaki-mode nil
	  tcode-mazegaki-prefix nil)
    (while tcode-mazegaki-start-marker
      (goto-char (car tcode-mazegaki-start-marker))
      (setq tcode-mazegaki-start-marker (cdr tcode-mazegaki-start-marker)))))

(defun tcode-mazegaki-relimit (length offset)
  "���ߤ��ɤߤ���ڤ�ľ������������򤵤��롣
���ڤ�ľ���ɤߤϡ�Ĺ�� LENGTH �� OFFSET �Ȥ�ɽ����롣"
  (tcode-mazegaki-candidate-select-init)
  (save-excursion
    (goto-char tcode-mazegaki-mode)
    (let ((old-yomi-total (+ tcode-mazegaki-current-yomi-length
			     tcode-mazegaki-current-offset))
	  (new-yomi-total (+ length offset)))
      (if (<= old-yomi-total new-yomi-total)
	  ;; �ޡ����������ɤߤ�ä�
	  (delete-region
	   (car (nth (- new-yomi-total old-yomi-total)
		     (nthcdr (1- old-yomi-total) tcode-mazegaki-yomi-list)))
	   (point))
	;; �ޡ����������ɤߤ������
	(insert (tcode-mazegaki-list-to-string
		 tcode-mazegaki-yomi-list
		 new-yomi-total
		 (- old-yomi-total new-yomi-total)))))
    (setq tcode-mazegaki-mode (point)))
  (tcode-mazegaki-put-conversion-face)
  (setq tcode-mazegaki-current-yomi-length length
	tcode-mazegaki-current-offset offset)
  (tcode-mazegaki-restore-yomi-and-quit t)
  (tcode-mazegaki-select-candidate))

(defun tcode-mazegaki-relimit-right ()
  "�ɤߤ�̤�롣"
  (interactive)
  (let ((p (save-excursion
	     (tcode-mazegaki-switch-to-dictionary)
	     (point)))
	(orig-with-inflection tcode-mazegaki-with-inflection)
	(yomi-info (or (and (not tcode-mazegaki-inflection-only)
			    (not tcode-mazegaki-with-inflection)
			    (tcode-mazegaki-lookup nil))
		       (and (or tcode-mazegaki-enable-inflection
				tcode-mazegaki-inflection-only)
			    (tcode-mazegaki-lookup-with-inflection
			     (not tcode-mazegaki-with-inflection))))))
    (if yomi-info
	(tcode-mazegaki-relimit (car (cdr yomi-info)) (cdr (cdr yomi-info)))
      (save-excursion
	(tcode-mazegaki-switch-to-dictionary)
	(goto-char p))
      (setq tcode-mazegaki-with-inflection orig-with-inflection)
      (ding)
      (tcode-verbose-message "����ʾ��ɤߤϽ̤���ޤ���"))))

(defun tcode-mazegaki-relimit-left ()
  "�ɤߤ򿭤Ф���"
  (interactive)
  (let ((p (save-excursion
	     (tcode-mazegaki-switch-to-dictionary)
	     (point)))
	(orig-with-inflection tcode-mazegaki-with-inflection)
	(yomi-info (or (and (or tcode-mazegaki-enable-inflection
				tcode-mazegaki-inflection-only)
			    tcode-mazegaki-with-inflection
			    (tcode-mazegaki-lookup-with-inflection-reverse 
			     nil))
		       (and (not tcode-mazegaki-inflection-only)
			    (tcode-mazegaki-lookup-reverse
			     tcode-mazegaki-with-inflection)))))
    (if yomi-info
	(tcode-mazegaki-relimit (car (cdr yomi-info)) (cdr (cdr yomi-info)))
      (save-excursion
	(tcode-mazegaki-switch-to-dictionary)
	(goto-char p))
      (setq tcode-mazegaki-with-inflection orig-with-inflection)
      (ding)
      (tcode-verbose-message "����ʾ��ɤߤϿ��Ф��ޤ���"))))

(defun tcode-mazegaki-select-candidate-or-relimit ()
  "�ɤߤ�̤�뤫�������ΰ��������������򤹤롣
�ɤߤ�̤��Τϡ����ߤθ���ο�����Ĥξ��Τߡ�"
  (interactive)
  (if (/= (tcode-mazegaki-get-number-of-candidate) 1)
      (tcode-mazegaki-select-candidate)
    (cancel-undo-boundary)
    (tcode-mazegaki-relimit-right)))

(defun tcode-mazegaki-prioritize (candidate)
  "���ߤ��ɤߤ� CANDIDATE ��ؽ����롣
���������ؽ��������ϡ��ѿ� `tcode-mazegaki-fixed-priority-count' ��
���ꤷ��������������Τ�ΤΤߡ�"
  (save-excursion
    (tcode-mazegaki-find-kanji-entry)
    (if tcode-mazegaki-with-inflection
	(setq candidate
	      (substring candidate
			 0
			 (- (length (tcode-mazegaki-construct-yomi
				     tcode-mazegaki-current-offset))))))
    (let ((eol (save-excursion (end-of-line) (point)))
	  (latest-point (point))
	  (i 1)
	  (beg (point)))
      (catch 'done
	(while (search-forward "/" eol t)
	  (when (equal (buffer-substring beg (1- (point)))
			 candidate)
	    (if (<= i tcode-mazegaki-fixed-priority-count)
		(throw 'done nil)
	      (delete-region beg (point))
	      (goto-char latest-point)
	      (insert candidate "/")
	      (throw 'done t)))
	  (if (= i tcode-mazegaki-fixed-priority-count)
	      (setq latest-point (point)))
	  (setq beg (point)
		i (1+ i)))))))

(defun tcode-mazegaki-finish ()
  "�򤼽��Ѵ�����ꤷ�ơ��򤼽��Ѵ��⡼�ɤ�ȴ���롣"
  (interactive)
  (tcode-mazegaki-candidate-select-init)
  (when tcode-mazegaki-prefix
    (setq tcode-mazegaki-prefix nil)
    (tcode-mazegaki-delete-conversion-face))
  (if tcode-mazegaki-mode
      (let* ((beg (if (and (stringp tcode-mazegaki-prefix-mark)
			   (save-excursion 
			     (goto-char tcode-mazegaki-mode)
			     (looking-at 
			      (regexp-quote tcode-mazegaki-prefix-mark))))
		      (match-end 0)
		    tcode-mazegaki-mode))
	     (end (point))
	     (kakutei (prog1
			  (buffer-substring beg end)
			(and tcode-mazegaki-selected-candidate-register
			     (copy-to-register
			      tcode-mazegaki-selected-candidate-register 
			      beg end)))))
	(if overwrite-mode
	    (delete-text-in-column nil (+ (current-column)
					  (string-width kakutei)))
	  (tcode-do-auto-fill))
	(and (boundp 'self-insert-after-hook)
	     self-insert-after-hook
	     (funcall self-insert-after-hook tcode-mazegaki-mode (point)))
	(run-hooks 'input-method-after-insert-chunk-hook)
	(tcode-mazegaki-prioritize kakutei)
	(and tcode-record-file-name
	     (setq tcode-mazegaki-occurrence
		   (+ (chars-in-string kakutei) tcode-mazegaki-occurrence)))
	(setq tcode-mazegaki-mode nil
	      tcode-mazegaki-prefix nil)
	(tcode-mazegaki-delete-conversion-face)
	(when tcode-mazegaki-start-marker
	  ;; recursive conversion
	  (goto-char (car tcode-mazegaki-start-marker))
	  (setq tcode-mazegaki-start-marker (cdr tcode-mazegaki-start-marker))
	  (tcode-mazegaki-convert nil))
	(unless (window-minibuffer-p (selected-window))
	  (message ""))
	(unless (string= kakutei "")
	  (setq tcode-help-char
		(let ((kakutei-chars (string-to-list kakutei)))
		  (while (cdr kakutei-chars)
		    (setq kakutei-chars (cdr kakutei-chars)))
		  (char-to-string (car kakutei-chars)))))
	(and tcode-auto-help
	     kakutei
	     (not (string= kakutei ""))
	     (tcode-display-direct-stroke
	      (substring kakutei
			 0
			 (and (not (string= tcode-mazegaki-suffix ""))
			      (- (length tcode-mazegaki-suffix))))
	      (tcode-mazegaki-construct-yomi
	       (+ tcode-mazegaki-current-yomi-length
		  tcode-mazegaki-current-offset)))
	     (tcode-auto-remove-help-char)))))

(add-hook 'tcode-clear-hook 'tcode-mazegaki-finish)

(defun tcode-mazegaki-recursive-convert-backward ()
  (interactive)
  (let ((marker (make-marker))
	(beg tcode-mazegaki-mode))
    (let (tcode-mazegaki-start-marker)	; protect
      (tcode-mazegaki-restore-yomi-and-quit))
    (setq tcode-mazegaki-start-marker 
	  (cons (set-marker marker (point))
		tcode-mazegaki-start-marker))
    (goto-char beg)
    (tcode-mazegaki-convert nil)))

(defun tcode-mazegaki-cancel-previous-recursive-convert ()
  (interactive)
  (when tcode-mazegaki-start-marker
    (let (tcode-mazegaki-start-marker)	; protect
      (tcode-mazegaki-restore-yomi-and-quit))
    (goto-char (car tcode-mazegaki-start-marker))
    (setq tcode-mazegaki-start-marker (cdr tcode-mazegaki-start-marker))
    (tcode-mazegaki-convert nil)))

;;;; ��Ͽ�Ⱥ��

(defun tcode-mazegaki-get-yomi-and-kanji (prompt &optional str)
  "�򤼽���Ͽ���Ρ��ɤߡפȡִ����פ�Ŭ�������롣"
  (let* ((minibuffer-setup-hook
	  ;; avoid referencing undefined variables in NEmacs.
	  (and (boundp 'minibuffer-setup-hook)
	       (cons 'toggle-input-method minibuffer-setup-hook)))
	 (yomi (if tcode-mazegaki-mode
		   (buffer-substring tcode-mazegaki-mode (point))
		 (read-from-minibuffer (concat prompt "�ɤ� ")
				       (if (and str
						(or (tcode-mule-2-p)
						    (tcode-mule-3-p)
						    (tcode-mule-4-p)))
					   (cons str 1)
					 str))))
	 (kanji (read-from-minibuffer
		 (format "%s����(�ɤ�=%s) " prompt yomi))))
    (list (mapconcat 'char-to-string
		     (delq ?\n (string-to-list yomi))
		     nil)
	  (mapconcat 'char-to-string
		     (delq ?\n (string-to-list kanji))
		     nil))))

(defun tcode-split-string-by-regexp (string splitter)
  "ʸ���� STRING ������ɽ�� SPLITTER ����ڤ�Ȥ���ʬ�䤹�롣"
  (let (l)
    (while (string-match splitter string)
      (setq l (nconc l (list (substring string 0 (match-beginning 0))))
	    string (substring string (match-end 0))))
    (nconc l (list string))))

(defun tcode-mazegaki-yomi-combination (yomi-list kanji-list)
  "�ɤߤȴ����Ȥμ�������Ȥ߹�碌�򤹤٤���󤹤롣"
  (if (<= (length yomi-list) 1)
      (append kanji-list yomi-list)
    (let ((yomi-car (car yomi-list))
	  (yomi-cdr (cdr yomi-list))
	  (kanji-car (car kanji-list))
	  (kanji-cdr (cdr kanji-list)))
      (if (string= yomi-car kanji-car)
	  (mapcar (lambda (a) (concat kanji-car a))
		  (tcode-mazegaki-yomi-combination yomi-cdr kanji-cdr))
	(nconc
	 (mapcar (lambda (a) (concat kanji-car a))
		 (tcode-mazegaki-yomi-combination yomi-cdr kanji-cdr))
	 (mapcar (lambda (a) (concat yomi-car a))
		 (tcode-mazegaki-yomi-combination yomi-cdr kanji-cdr)))))))

;;;###autoload
(defun tcode-mazegaki-make-entry (yomi kanji)
  "�ɤ� YOMI������ KANJI �ǡ������ʥ���ȥ��򤼽񤭼������Ͽ���롣
��������Ͽ������ t�������Ǥʤ���� nil ���֤���
�ɤߤ���Ӵ����� `tcode-mazegaki-splitter' �Ƕ��ڤäƤ�����ˤϡ�
�������Ȥ߹�碌���ɤߤ��٤ƤˤĤ�����Ͽ���롣"
  (interactive (tcode-mazegaki-get-yomi-and-kanji "��Ͽ��"))
  (and (interactive-p)
       (> (string-width yomi) tcode-mazegaki-yomi-max)
       (message (concat "�ɤߡ�%s�פ�Ĺ��(%d)�� `tcode-mazegaki-yomi-max' "
			"���ͤ�Ķ���Ƥ��ޤ���")
		yomi (string-width yomi))
       (setq tcode-mazegaki-yomi-max (string-width yomi)))
  (let ((yomi-list (tcode-split-string-by-regexp yomi
						 tcode-mazegaki-splitter)))
    (if (> (length yomi-list) 1)
	;; multi pattern
	(let ((kanji-list
	       (tcode-split-string-by-regexp kanji
					     tcode-mazegaki-splitter)))
	  (unless (= (length yomi-list) (length kanji-list))
	    (error "���ڤ������ְ�äƤ��ޤ���"))
	  (setq yomi-list (tcode-mazegaki-yomi-combination
			   yomi-list kanji-list)
		kanji (car yomi-list)
		yomi-list (cdr yomi-list))
	  (mapcar (lambda (yomi)
		    (tcode-mazegaki-make-entry yomi kanji))
		  yomi-list))
      (save-excursion
	(tcode-mazegaki-switch-to-dictionary)
	(tcode-mazegaki-search-yomi yomi)
	(setq tcode-mazegaki-current-yomi-point (point))
	;; �����ɤߤ���Ͽ����Ƥ���С��������ɲä���
	;; ��Ͽ����Ƥ��ʤ���С������˥���ȥ���롣
	(cond ((not (looking-at (concat yomi " /")))
	       (insert yomi " /" kanji "/\n")
	       t)
	      ((re-search-forward (concat "/" kanji "/")
				  (save-excursion (end-of-line) (point))
				  t)
	       (and (interactive-p)
		    (progn
		      (ding)
		      (message "��%s�פϤ��Ǥ���Ͽ����Ƥ��ޤ���" kanji)))
	       nil)
	      (t
	       (tcode-mazegaki-find-kanji-entry)
	       (insert kanji "/")))))))

(defun tcode-mazegaki-inflection-p (yomi)
  "�ɤߤ����Ѥ����Τ��ɤ�����ɽ���Ҹ졣"
  (let ((length (length tcode-mazegaki-inflection-mark)))
    (and (stringp yomi)
	 (> (length yomi) length)
	 (string= (substring yomi (- length))
		  tcode-mazegaki-inflection-mark))))

;;;###autoload
(defun tcode-mazegaki-make-entry-and-finish ()
  "�����ʥ���ȥ��򤼽񤭼������Ͽ�������ꤹ�롣
�ɤߤϡ��򤼽��Ѵ��椢�뤤��ľ�����Ѵ��˼��Ԥ��Ƥ������ϡ������������롣
�����ơ���Ͽ�����Ͽ������������ꤹ�롣
�򤼽��Ѵ��椢�뤤���Ѵ��˼��Ԥ���ľ��ʳ��ξ��ϡ�����ϹԤ�ʤ���"
  (interactive)
  (let ((yomi-exists (or tcode-mazegaki-mode
			 (eq last-command 'tcode-mazegaki-begin-conversion))))
    (if tcode-mazegaki-mode
	(tcode-mazegaki-restore-yomi-and-quit))
    (let* ((yomi-kanji (tcode-mazegaki-get-yomi-and-kanji
			"��Ͽ��" (if yomi-exists
				     (tcode-mazegaki-list-to-string
				      tcode-mazegaki-yomi-list 0
				      (length tcode-mazegaki-yomi-list))
				   nil)))
	   (yomi (car yomi-kanji)))
      (tcode-mazegaki-make-entry yomi (car (cdr yomi-kanji)))
      (if yomi-exists
	  ;; ���ꤹ��
	  (let* ((split-yomi
		  (tcode-split-string-by-regexp yomi
						tcode-mazegaki-splitter))
		 (real-yomi (let (s)
			      (while split-yomi
				(setq s (concat s (car split-yomi))
				      split-yomi (cdr split-yomi)))
			      s)))
	    (tcode-mazegaki-convert
	     (let ((yomi-list (string-to-list real-yomi)))
	       (if (<= (car yomi-list) 255)
		   1			; alphabet
		 (length yomi-list)))
	     (tcode-mazegaki-inflection-p real-yomi)))))))

;;;###autoload
(defun tcode-mazegaki-delete-entry (yomi kanji)
  "�ɤ� YOMI������ KANJI �Υ���ȥ��򤼽񤭼��񤫤������롣"
  (interactive
   (let* ((minibuffer-setup-hook
	   ;; avoid referencing undefined variables in NEmacs.
	   (and (boundp 'minibuffer-setup-hook)
		(cons 'toggle-input-method minibuffer-setup-hook)))
	  (yomi (if tcode-mazegaki-mode
		    (buffer-substring tcode-mazegaki-mode (point))
		  (read-from-minibuffer "������ɤ� ")))
	  (found (save-excursion
		   (tcode-mazegaki-switch-to-dictionary)
		   (tcode-mazegaki-search-yomi yomi)))
	  (candidate (and found
		      (tcode-mazegaki-make-table-and-select
		       (format "������ɤ� %s " yomi))))
	  (yes (and (stringp candidate)
		    (y-or-n-p
		     (format
		      "�ɤߡ�%s�״�����%s�פ������ޤ���? "
		      yomi candidate))))
	  (kanji (cond (yes
			candidate)
		       ((not found)
			(error "�ɤߡ�%s�פ���Ͽ����Ƥ��ޤ���" yomi))
		       ((error "������")))))
     (list yomi kanji)))
  (save-excursion
    (tcode-mazegaki-switch-to-dictionary)
    (if (and (tcode-mazegaki-search-yomi yomi)
	     (setq tcode-mazegaki-current-yomi-point (point))
	     (re-search-forward (concat "\\(/" kanji "\\)/")
				(save-excursion (end-of-line) (point))
				t))
	(prog2		  ; 1�����Τ��������Ȥ� t ����
				     ; �ʳ��� nil ���֤�
	    (progn
	      (delete-region (match-beginning 1) (match-end 1))
	      (tcode-mazegaki-find-kanji-entry))
	    (and (looking-at "$")
		 (prog1 t		; 1�����Τ���
		   (beginning-of-line)
		   (delete-region (point) (progn (forward-line 1) (point)))))
	  (and (interactive-p)
	       (message "�ɤߡ�%s�״�����%s�פ������ޤ�����" yomi kanji)))
      (when (interactive-p)
	(ding)
	(message "�ɤߡ�%s�״�����%s�פ���Ͽ����Ƥ��ޤ���"
		 yomi kanji)))))

(defun tcode-mazegaki-apply-entries-region (beg end func msg)
  "�꡼�������μ���ι��ܤ��줾����Ф��� FUNC ��Ŭ�Ѥ��롣
FUNC �ؤϡ���1�������ɤߡ���2�����˴������缡�Ϥ���롣

����ι��ܤ�1�Ԥ��Ļ��ꤹ�롣�ƹԤν񼰤�

�ɤ� /����/[����/]*\\n"
  (interactive "r")
  (save-excursion
    (save-restriction
      (message "���%s��..." msg)
      (narrow-to-region beg end)
      (goto-char (point-min))
      (let ((line 1))
	(condition-case nil
	    (while (not (eobp))
	      (let* ((eol (save-excursion (end-of-line) (point)))
		     (bol (point))
		     (yomi (buffer-substring (point)
					     (progn
					       (search-forward " /" eol)
					       (goto-char (- (point) 2))
					       (point)))))
		(re-search-forward "/$" eol)
		(goto-char (1- (point)))
		(while (re-search-backward "/\\(.+\\)" bol t)
		  (funcall func yomi (buffer-substring
				      (match-beginning 1)
				      (match-end 1))))
		(setq line (1+ line))
		(forward-line 1)))
	  (error
	   (ding)
	   (message "%d ���ܤǼ���ν񼰤��ְ�äƤ��ޤ���" line)))
	(message "���%s��...��λ" msg))
      (widen))))

;;;###autoload
(defun tcode-mazegaki-make-entries-region (beg end)
  "�꡼�������μ���ι��ܤ��礷����Ͽ���롣
����ν񼰤ϴؿ� `tcode-mazegaki-apply-entries-region' �򻲾ȡ�"
  (interactive "r")
  (tcode-mazegaki-apply-entries-region 
   beg end 'tcode-mazegaki-make-entry "��Ͽ"))

;;;###autoload
(defun tcode-mazegaki-make-entries-buffer (&optional buffer)
  "�Хåե���μ���ι��ܤ��礷����Ͽ���롣
���ޥ�� `tcode-mazegaki-make-entries-region' ���ȡ�"
  (interactive)
  (save-excursion
    (if buffer
	(set-buffer buffer))
    (tcode-mazegaki-make-entries-region (point-min) (point-max))))

;;;###autoload
(defun tcode-mazegaki-delete-entries-region (beg end)
  "�꡼�������μ���ι��ܤ��礷�ƺ�����롣
����ν񼰤ϴؿ� `tcode-mazegaki-apply-entries-region' �򻲾ȡ�"
  (interactive "r")
  (tcode-mazegaki-apply-entries-region
   beg end 'tcode-mazegaki-delete-entry "���"))

;;;###autoload
(defun tcode-mazegaki-delete-entries-buffer (&optional buffer)
  "�Хåե���μ���ι��ܤ��礷�ƺ�����롣
���ޥ�� `tcode-mazegaki-delete-entries-region' ���ȡ�"
  (interactive)
  (save-excursion
    (if buffer
	(set-buffer buffer))
    (tcode-mazegaki-delete-entries-region (point-min) (point-max))))

;;;###autoload
(defun tcode-mazegaki-delete-entry-by-last-yomi (arg)
  "�Ǹ�����Ϥ����ɤߤ�����������򤷡�����������롣
���� ARG �� nil �Ǥʤ��Ȥ��ϡ��ɤߤ⿷�������Ϥ��롣"
  (interactive "P")
  (if (or (null tcode-mazegaki-yomi-list)
	  arg)
      (call-interactively 'tcode-mazegaki-delete-entry)
    (let* ((yomi (save-excursion
		   (tcode-mazegaki-switch-to-dictionary)
		   (beginning-of-line)
		   (looking-at "\\([^/]+\\) /")
		   (buffer-substring (match-beginning 1) (match-end 1))))
	   (kanji (tcode-mazegaki-make-table-and-select
		   (format "������ɤߡ�%s�״�����? " yomi))))
      (if (not (stringp kanji))
	  (message "������")
	(if (y-or-n-p (format "�ɤߡ�%s�״�����%s�פ������ޤ��� "
			      yomi kanji))
	    (tcode-mazegaki-delete-entry yomi kanji))))))

;;;; �ɤߤ�����䴰

(defun tcode-mazegaki-make-completion-prompt (yomi comp-list)
  "�䴰���䤬ʣ��������Ρ�����Τ���Υץ��ץȤ�ʸ������롣"
  (let* ((prompt (concat yomi "{"
			 (substring (car comp-list) (length yomi) nil)))
	 (max-len (- (frame-width) 12)) ; 12 is for suffix
	 (yomi-len (length yomi))
	 (diff (substring (car (setq comp-list (cdr comp-list)))
			  yomi-len nil))
	 (new-prompt (concat prompt ", " diff)))
    (concat
     (catch 'overflow
       (while comp-list
	 (or (< (string-width new-prompt) max-len)
	     (throw 'overflow prompt))
	 (setq prompt new-prompt
	       comp-list (cdr comp-list)
	       diff (and comp-list
			 (substring (car comp-list) yomi-len nil))
	       new-prompt (concat prompt ", " diff)))
       prompt)
     (if comp-list
	 (format ", �� (+%d)" (length comp-list))
       "}"))))

;;;###autoload
(defun tcode-mazegaki-complete (&optional conversion)
  "�򤼽񤭼�����ɤߤ����䴰��Ԥ���
���䤬ʣ������Ȥ��Υ���������Ƥϼ��ΤȤ��ꡣ

    SPC		���θ������Ƭ�ˤ���
    DEL		�Ǹ�θ������Ƭ�ˤ���
    TAB		��Ƭ�θ�����䴰����λ����
    LFD		��Ƭ�θ�����䴰����λ�����θ��Ѵ�����
    RET		���򤻤����䴰��λ����

CONVERSION �� nil �Ǥʤ��Ȥ����䴰��(�䴰��Ԥä����Τ�)�Ѵ���Ԥ���"
  (interactive "*P")
  (setq tcode-mazegaki-yomi-list (tcode-mazegaki-get-reverse-yomi-list))
  (unless tcode-mazegaki-yomi-list
    (error "�䴰�Ǥ��ޤ���"))
  (let* ((yomi-len (length tcode-mazegaki-yomi-list))
	 (yomi-prefix
	  (catch 'found
	    (while (> yomi-len 0)
	      (let ((yomi (regexp-quote
			   (tcode-mazegaki-construct-yomi yomi-len))))
		(save-excursion
		  (tcode-mazegaki-switch-to-dictionary)
		  (tcode-mazegaki-search-yomi yomi)
		  (and (looking-at yomi)
		       (or (not (looking-at (concat yomi " /")))
			   (save-excursion
			     (forward-line 1)
			     (looking-at yomi)))
		       (throw 'found yomi))))
	      (and tcode-mazegaki-mode
		   tcode-mazegaki-yomi-fixed
		   (throw 'found nil))
	      (setq yomi-len (1- yomi-len)))))
	 (completion-list
	  (and yomi-prefix
	       (save-excursion
		 (tcode-mazegaki-switch-to-dictionary)
		 (let (list 
		       yomi 
		       (i 0)
		       (yomi-len (length yomi-prefix)))
		   (while (string= yomi-prefix
				   (buffer-substring (point) 
						     (+ (point) yomi-len)))
		     (setq yomi (buffer-substring (point)
						  (save-excursion
						    (search-forward " /")
						    (- (point) 2)))
			   i (1+ i))
		     (if (> i tcode-mazegaki-complete-max)
			 (error "��%s�פϸ���ο���¿�����ޤ���"
				yomi-prefix))
		     (unless (string= yomi yomi-prefix)
		       (setq list (nconc list (list (cons yomi nil)))))
		     (forward-line 1))
		   list))))
	 (most (and completion-list
		    (try-completion yomi-prefix completion-list)))
	 comp)
    (or completion-list
	(error "�䴰�Ǥ��ޤ���"))
    ;; for NEmacs
    (and (stringp most)
	 (let ((last-char (aref most (1- (length most)))))
	   (and (= (mod (string-width most) 2) 1)
		(> last-char 127)
		(<= last-char 255)
		(setq most (substring most 0 -1))
		(string= yomi-prefix most)
		(setq most nil))))
    ;; �䴰�Ǥ�����ʬ���䴰
    (when most
      (delete-region (car (nth (1- yomi-len) tcode-mazegaki-yomi-list))
		     (point))
      (insert (if (stringp most)
		  most
		yomi-prefix))
      (tcode-do-auto-fill))
    (if (stringp most)
	(setq yomi-prefix most))
    (setq comp (and completion-list
		    (all-completions yomi-prefix completion-list)))
    (when (> (length comp) 1)
      ;; ����⡼��
      (catch 'quit
	(while t
	  (message (tcode-mazegaki-make-completion-prompt yomi-prefix comp))
	  (let ((ch (read-char)))
	    (cond ((= ch ? )
		   ;; ���θ������Ƭ��
		   (setq comp (nconc (cdr comp) (list (car comp)))))
		  ((= ch ?\C-?)
		   ;; �Ǹ�θ������Ƭ��
		   (let ((list comp))
		     (while (and (cdr list)
				 (cdr (cdr list)))
		       (setq list (cdr list)))
		     (setq comp (append (cdr list) comp))
		     (rplacd list nil)))
		  ((or (= ch ?\t)
		       (= ch last-command-char))
		   ;; ��Ƭ�θ�������򤷤ƽ�λ
		   (insert (substring (car comp)
				      (length yomi-prefix) nil))
		   (tcode-do-auto-fill)
		   (throw 'quit t))
		  ((= ch ?\n)
		   ;; ��Ƭ�θ�������򤷤ƽ�λ����λ����Ѵ�
		   (insert (substring (car comp)
				      (length yomi-prefix) nil))
		   (tcode-do-auto-fill)
		   (setq conversion t)
		   (throw 'quit t))
		  ((= ch ?\r)
		   ;; ��������򤻤���λ
		   (setq conversion nil)
		   (throw 'quit t))
		  (t
		   ;; ��λ�������Υ��ޥ�ɤ�¹�
		   (tcode-redo-command ch)
		   (setq conversion nil)
		   (throw 'quit t))))))
      (unless (window-minibuffer-p (selected-window))
	(message "")))
    (if conversion
	(tcode-mazegaki-convert nil))))

;;;###autoload
(defun tcode-mazegaki-complete-and-convert ()
  "�򤼽񤭼�����ɤߤ����䴰��Ԥ������θ��Ѵ����롣
�ܺ٤ϥ��ޥ�� `tcode-mazegaki-complete' ���ȡ�"
  (interactive "*")
  (tcode-mazegaki-complete t))

;;;; ����¾(����������Ƥ伭�����¸�ʤ�)

(defun tcode-mazegaki-backward-delete-char (arg)
  "���ޥ�� `backward-delete-char' ��Ʊ���������˸򤼽��Ѵ���λ���롣
�򤼽��Ѵ���λ����Τϡ���������ä����Ȥ���"
  (interactive "*p")
  (if (<= (point) tcode-mazegaki-mode)
      (tcode-mazegaki-finish)
    (backward-delete-char arg)))

(defun tcode-mazegaki-command-summary ()
  "�򤼽��Ѵ����Υ����γ�����ư�����ɽ�����롣
ɽ���������Ƥϡ��ѿ� `tcode-mazegaki-command-summary-alist' �ǻ��ꤹ�롣"
  (interactive)
  (message
   (mapconcat
    (lambda (elm)
      (let* ((key (where-is-internal (cdr elm) tcode-mazegaki-map t))
	     (key-str (if (null key)
			  (error
			   (concat "`tcode-mazegaki-command-summary-alist' "
				   "�δؿ�̾�˴ְ㤤������ޤ���"))
			(key-description key))))
	(format "%s=%s" key-str (car elm))))
    tcode-mazegaki-command-summary-alist " "))
  (sit-for 5))

;;;###autoload
(defun tcode-mazegaki-put-prefix ()
  "���ַ��򤼽��Ѵ��γ��������ΰ����դ��롣"
  (interactive)
  (setq tcode-mazegaki-prefix (point))
  (add-hook 'post-command-hook 'tcode-mazegaki-put-conversion-face))

;;;###autoload
(defun tcode-mazegaki-add-prefix (char)
  "���ַ��򤼽��Ѵ���Ϥ�뤿��Υե��륿��"
  (tcode-mazegaki-put-prefix)
  char)

(defun tcode-mazegaki-self-insert-or-convert (arg)
  "���ޥ�� `self-insert-command' ��Ʊ�������򤼽��Ѵ���Ԥ���
�򤼽��Ѵ���Ԥ��Τϡ����ɤߡפ����Ϥ���Ƥ�����Τߡ�
�Ѵ���Ԥ��� t���Ԥ�ʤ���� nil ���֤���"
  (interactive "*P")
  (if (not tcode-mazegaki-prefix)
      (self-insert-command (prefix-numeric-value arg))
					; `self-insert-command' �� nil ���֤���
    (setq tcode-mazegaki-yomi-list (tcode-mazegaki-get-reverse-yomi-list))
    (tcode-mazegaki-convert (length tcode-mazegaki-yomi-list)
			    current-prefix-arg)
    t))			       ; �Ѵ������Ȥ� t ���֤���

(unless (featurep 'tc-mazegaki)
  (setq tcode-mazegaki-map (make-keymap))

  (tcode-set-key " " 'tcode-mazegaki-self-insert-or-convert)

  (mapcar
   (lambda (elm)
     (define-key tcode-mazegaki-map (car elm) (cdr elm)))
   '((" "     . tcode-mazegaki-select-candidate-or-relimit)
     ("\C-u"  . tcode-mazegaki-restore-yomi-and-quit)
     ("\C-m"  . tcode-mazegaki-finish)
     ("<"     . tcode-mazegaki-relimit-left)
     (">"     . tcode-mazegaki-relimit-right)
     ("|"     . tcode-mazegaki-make-entry-and-finish)
     ("\C-\?" . tcode-mazegaki-backward-delete-char)
     ("="     . tcode-mazegaki-table-mode)
     ("\C-l"  . recenter)
     ("\C-b"  . tcode-mazegaki-recursive-convert-backward)
     ("\C-f"  . tcode-mazegaki-cancel-previous-recursive-convert)
     ("?"     . tcode-mazegaki-command-summary)))

  (run-hooks 'tcode-mazegaki-init-hook))

(provide 'tc-mazegaki)

;;; tc-mazegaki.el ends here
