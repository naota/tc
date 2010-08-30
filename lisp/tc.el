;;; tc.el --- a Japanese input method with T-Code on Emacs

;; Copyright (C) 1989--2002 Kaoru Maeda, Yasushi Saito and KITAJIMA Akira.

;; Author: Kaoru Maeda <maeda@src.ricoh.co.jp>
;;	Yasushi Saito <yasushi@cs.washington.edu>
;;      KITAJIMA Akira <kitajima@isc.osakac.ac.jp>
;; Maintainer: KITAJIMA Akira
;; Keyword: input method, japanese

;; $Id: tc.el,v 1.60 2003/03/29 03:42:27 kitajima Exp $

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

(require 'tc-pre)
(require 'tc-sysdep)

;;
;; Variables for customization
;;

(defcustom tcode-bushu-ready-hook nil
  "��������Ѵ��ν����ľ��˼¹Ԥ����եå���"
  :type 'hook :group 'tcode)

(defvar tcode-bushu-on-demand 2
  "��������Ѵ�����򤤤Ľ�������뤫��
	0 : tc.el ���ɻ�
	1 : ����T�����ɥ⡼�ɤ����ä��Ȥ�
	2 : ��������Ѵ��򳫻Ϥ����Ȥ���
            �ޤ���ʸ���Υإ�פ򸫤褦�Ȥ����Ȥ�")

(defcustom tcode-use-postfix-bushu-as-default nil
  "* nil �Ǥʤ��Ȥ���jf�Ǹ��ַ���������Ѵ���77�����ַ���������Ѵ���Ԥ���
nil�λ��ˤϤ��εա�" :type 'boolean :group 'tcode)

(defcustom tcode-use-prefix-mazegaki nil
  "* ���ַ�(���跿)�θ򤼽��Ѵ��λ���t, ���ַ�(����)�θ򤼽��Ѵ��λ���nil��

���ַ��Ǥϡ�fj�����Ϥ���ȡ��ɤ����ϥ⡼�ɤ����ꡢ������� ' '�����Ϥ����
�Ѵ���Ԥ������ַ��Ǥϡ�fj�����Ϥ���ȡ��ݥ�������ˤ���ʸ�����Ȥä�
�Ѵ���Ԥ���"
  :type 'boolean :group 'tcode)

(defvar tcode-kuten "��" "* ����")
(make-variable-buffer-local 'tcode-kuten)
(defvar tcode-touten "��" "* ����")
(make-variable-buffer-local 'tcode-touten)

(defvar tcode-switch-table-list
  '(((tcode-touten . "��")
     (tcode-kuten . "��"))

    ((tcode-touten . ", ")
     (tcode-kuten . ". ")))
  "�ơ��֥�����ѿ��ͤ��ڤ��ؤ��뤿���ɽ��")

(defcustom tcode-record-file-name "~/.tc-record"
  "* non-nil �ΤȤ���T�����ɤ����פ򤳤Υե�����˵�Ͽ���롣"
  :type 'string :group 'tcode)

(defcustom tcode-ready-hook nil
  "Emacs��Ω���夲�Ƥ���ǽ��tcode-mode�����ä��Ȥ��¹Ԥ����եå���"
  :type 'hook :group 'tcode)

(defcustom tcode-mode-hook nil
  "���ΥХåե��ǽ���tcode-mode�����ä��Ȥ��¹Ԥ����եå���"
  :type 'hook :group 'tcode)

(defcustom tcode-toggle-hook nil
  "tcode-mode��ȥ��뤹���٤˼¹Ԥ����եå���"
  :type 'hook :group 'tcode)

(defcustom tcode-before-load-table-hook nil
  "`tcode-load-table' �ˤ��ơ��֥���ɤ߹���ľ���˼¹Ԥ����եå���"
  :type 'hook :group 'tcode)

(defcustom tcode-after-load-table-hook nil
  "`tcode-load-table' �ˤ��ơ��֥���ɤ߹����٤˼¹Ԥ����եå���"
  :type 'hook :group 'tcode)

(defcustom tcode-before-read-stroke-hook nil
  "2���ȥ����ܰʹߤΥ������ȥ������ɤ����˼¹Ԥ����եå���"
  :type 'hook :group 'tcode)

(defcustom tcode-clear-hook nil
  "�ǥե�����Ⱦ��֤��᤹�Ȥ��˼¹Ԥ����եå���"
  :type 'hook :group 'tcode)

(defvar tcode-auto-zap-table nil
  "* non-nil �ΤȤ������ȥ���ɽ�򼡤��Ǹ��Ǽ�ưŪ�˾ä���")

(defcustom tcode-auto-help t
  "* non-nil �ΤȤ������Ϥ���ʸ���Υإ��ɽ��ưŪ��ɽ�����롣
��������ɽ������Τϡ��򤼽��Ѵ�����������Ѵ��ˤ�ä�ľ�����ϤǤ��������
�����������Τߡ�
�ޤ����ͤ�����ܥ� delete-the-char �ΤȤ��ϡ��إ�פ�ɽ������
����˺Ǹ�˥إ�פ��оݤȤʤä�ʸ����õ�롣

��������`input-method-verbose-flag' �� nil �Ǥʤ����� t �Ȥʤ�
�Τ���ա�"
  :group 'tcode)

(defvar tcode-auto-remove-help-count nil
  "* �إ��ɽ����̤���̵�����ޤǤ˸ƤФ�� `tcode-auto-remove-help' �β����
�ؿ� `tcode-auto-remove-help' �ϡ������ѿ��β�������ƤФ��ȡ�
�إ��ɽ��ưŪ�˺�����롣nil �ξ��ϼ�ư�����Ԥ�ʤ���")

(defcustom tcode-adjust-window-for-help nil
  "* non-nil �ΤȤ����إ�פ�ɽ�����륦����ɥ����礭����ưŪ��Ĵ�����롣"
  :type 'boolean :group 'tcode)

(defcustom tcode-adjust-window-additional-height
  (if (and (tcode-mule-4-p)
	   (= emacs-major-version 21))
      1
    0)
  "* �إ�פ�ɽ�����륦����ɥ��ι⤵���ɲä���⤵��"
  :type 'integer :group 'tcode)

(defcustom tcode-display-help-delay 2
  "* ľ����ʸ�������Ϥ��Ƥ��鲾�۸��פ�ɽ������ޤǤλ���(��)��

��������`input-method-verbose-flag' �� nil �Ǥʤ����� 2 �Ȥʤ�
�Τ���ա�"
  :group 'tcode)

(defcustom tcode-verbose-message t
  "* non-nil �ΤȤ������¿���Υ�å�������ɽ�����롣

���������ѿ� `input-method-verbose-flag' �� nil �Ǥʤ����ϡ�
�����ѿ����ͤˤ�餺��t �����ꤵ��Ƥ������Ʊ��ư��ˤʤ�Τ���ա�"
  :type 'boolean :group 'tcode)

(defvar tcode-shift-lowercase nil
  "T���������ϻ��Υ��եȤξ��ˡ���ʸ���ǤϤʤ���ʸ�������Ϥ��롣")

(defvar tcode-no-wait-display-help-command-list nil
  "���ڡ��������Ϥǥإ��ɽ�ΥХåե���ä���ǽ��Ȥ�ʤ����ޥ�ɤΥꥹ�ȡ�
���Υꥹ����Υ��ޥ�ɤǤϡ� `tcode-display-help-buffer' �ˤ��
�إ�פ�ɽ�����줿���ˡ�³���ƥ��ڡ��������Ϥ����Ȥ��Ǥ�
�إ��ɽ�Ͼä��ʤ���")

(defvar tcode-help-window-height-max 21
  "* �إ�פ�ɽ�����뤿��Υ�����ɥ��ι⤵�κ�����")

(defvar tcode-cancel-stroke-list '(?\C-\? ?\C-h)
  "ʸ�����Ϥ�����Ū�˼��ä������Υꥹ��")
(defvar tcode-verbose-stroke-list '(? )
  "ʸ�����Ϥ�����ǡ����ޤǤ����Ϥ򤹤٤Ƥ��Τޤ��������륭���Υꥹ��")

;;
;; Global Variables
;;

(defvar tcode-mode-map nil
  "tcode-mode �ΤȤ� T�����ɥ����ʳ��Υ����Τ���Υ����ޥåס�
���Υ����ޥåפ���Ͽ����Ȥ��ϡ�`tcode-set-key'���Ѥ��롣
`tcode-key-translation-rule-table' �򻲾ȡ�")

(defvar tcode-key-translation-rule-table
;;   0  1  2  3  4  5  6  7  8  9
;;  10 11 12 13 14 15 16 17 18 19
;;  20 21 22 23 24 25 26 27 28 29
;;  30 31 32 33 34 35 36 37 38 39

;;   1  2  3  4  5  6  7  8  9  0
;;   q  w  e  r  t  y  u  i  o  p
;;   a  s  d  f  g  h  j  k  l  ;
;;   z  x  c  v  b  n  m  ,  .  /

;;      !  \"   #   $   %   &   '   (   )   *   +   ,   -   .   /
;;  0   1   2   3   4   5   6   7   8   9   :   ;   <   =   >   ?
;;  @   A   B   C   D   E   F   G   H   I   J   K   L   M   N   O
;;  P   Q   R   S   T   U   V   W   X   Y   Z   [   \   ]   ^   _
;;  `   a   b   c   d   e   f   g   h   i   j   k   l   m   n   o
;;  p   q   r   s   t   u   v   w   x   y   z   {   |   }   ~
  [
   -1  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1  37  -1  38  39
   09  00  01  02  03  04  05  06  07  08  -1  29  -1  -1  -1  -1
   -1  -2  -2  -2  -2  -2  -2  -2  -2  -2  -2  -2  -2  -2  -2  -2
   -2  -2  -2  -2  -2  -2  -2  -2  -2  -2  -2  -1  -1  -1  -1  -1
   -1  20  34  32  22  12  23  24  25  17  26  27  28  36  35  18
   19  10  13  21  14  16  33  11  31  15  30  -1  -1  -1  -1
   ]
  "T�����ɥ����Ѵ��ѥơ��֥롣����1ʸ�������Ϥ����Ȥ��ΰ�̣��ɽ����
0..39:	T�����ɥ�����
-1:	����ʸ����
-2:	�б�����Ѿ�ʸ����
-3:	`tcode-mode-map' �ˤ������ä����ޥ�ɡ�
< -3:	- (ʸ��������)��")

;;
;; These variables are set in a table file, e.g. tc-tbl.el.
;;
(defvar tcode-input-method nil "�������򤵤�Ƥ�������ˡ")

(defvar tcode-transparent-mode-indicator nil)
(defvar tcode-tcode-mode-indicator nil)
(defvar tcode-alnum-2byte-tcode-mode-indicator nil)
(defvar tcode-hiragana-mode-indicator nil)
(defvar tcode-katakana-mode-indicator nil)

;;; �ʲ����ѿ��ϡ�tc-tbl.el �ʤɤ��ͤ����ꤵ�졢
;;; �����ơ��֥� `tcode-table' �ˤ������Ƥ���Ͽ����롣
(defvar tcode-tbl nil)
(defvar tcode-non-2-stroke-char-list nil)
(defvar tcode-another-table nil
  "`tcode-verbose-stroke-list' �Υ����������줿�Ȥ�����������ʸ���Υơ��֥롣
�ͤ� nil �ʤ�Ф���ʸ����vector �ǻ��ꤵ��Ƥ���Х����˱���
��������ʸ�����������롣")
(defvar tcode-special-commands-alist nil)

(defvar tcode-mode-help-string nil
  "`tcode-mode-help' �ˤ�ä�ɽ�������ʸ����
nil �ΤȤ��� `tcode-mode' �������롣")
(defvar tcode-stroke-file-name nil "���ȥ���ɽ�Υѥ�̾")

;;
;; misc. global variables
;;

;;; ���ϥ����ɤξ�����ݻ�����ơ��֥�
(defvar tcode-table nil "�Ѵ����Ѥ���ɽ")
(defvar tcode-stroke-table nil)
(defvar tcode-stroke-table-size 511)

;;; ���׵�Ͽ�Ѥ��ѿ�
(defvar tcode-input-chars 0)
(defvar tcode-number-strokes 0)
(defvar tcode-bushu-occurrence 0)
(defvar tcode-mazegaki-occurrence 0)
(defvar tcode-special-occurrence 0)

(defvar tcode-help-char nil "�إ�פ��о�ʸ��")
(defvar tcode-auto-remove-help-current-count 0)
(defvar tcode-window-configuration-before-help nil)

(defvar tcode-mode-in-minibuffer nil)

(defvar tcode-dictionaries nil)
;; (�Хåե�̾ . �ե�����̾)�Υꥹ�ȡ�

(defconst tcode-stroke-buffer-name " *tcode: stroke*")
(defconst tcode-help-buffer-name "*T-Code Help*")

;;; input-method-verbose-flag ��
(defvar tcode-input-method-verbose-flag nil)
(defvar tcode-orig-auto-help nil)
(defvar tcode-orig-verbose-message nil)
(defvar tcode-orig-display-help-delay nil)

(defvar tcode-use-input-method nil)	; ***experimental***

(defvar tcode-input-filter-functions
  '(((or tcode-katakana-mode tcode-shift-state) . japanese-katakana)
    ((and (boundp 'tcode-bushu-prefix-list)
	  tcode-bushu-prefix-list) 
     . tcode-bushu-prefix-convert)
    (tcode-alnum-2-byte . tcode-1-to-2)))

(defvar tcode-shift-state nil)

(defvar tcode-input-command-list
  '(c-electric-semi&comma
    c-electric-slash
    canna-self-insert-command
    digit-argument
    egg-self-insert-command
    electric-c-brace
    electric-c-semi
    electric-c-terminator
    electric-perl-terminator
    self-insert-command
    sgml-slash
    tcode-electric-comma
    tcode-self-insert-command
    tcode-mazegaki-self-insert-or-convert)
  "*T�����ɥ⡼�ɤΤȤ��ˤ�T���������Ϥ��Ѥ��륳�ޥ�ɤΥꥹ�ȡ�")

;;
;; Buffer Local Variables
;;
(defvar tcode-mode nil "T�����ɥ⡼�ɤΤȤ�t��")
(make-variable-buffer-local 'tcode-mode)

(defvar tcode-ready-in-this-buffer nil "���ΥХåե����T�����ɤν�����OK")
(make-variable-buffer-local 'tcode-ready-in-this-buffer)

(defvar tcode-current-switch-table 0)
(make-variable-buffer-local 'tcode-current-switch-table)

(defvar tcode-alnum-2-byte nil
  "�ѿ����ΥХ���Ĺ�ڤ괹���ե饰��t �Ǥ�2�Х��ȷϡ�nil ��1�Х��ȷϡ�")
(make-variable-buffer-local 'tcode-alnum-2-byte)

(defvar tcode-katakana-mode nil "���ߥ������ʥ⡼�ɤ��ɤ���")
(make-variable-buffer-local 'tcode-katakana-mode)

;;
;; �������֡������ޥåפ�����
;;
(defvar tcode-key-layout-list
  '(("qwerty" . (?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9 ?0
		    ?q ?w ?e ?r ?t ?y ?u ?i ?o ?p
		    ?a ?s ?d ?f ?g ?h ?j ?k ?l ?\;
		    ?z ?x ?c ?v ?b ?n ?m ?, ?. ?/))
    ("qwerty-jis-shift" . (?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9 ?0
			      ?q ?w ?e ?r ?t ?y ?u ?i ?o ?p
			      ?a ?s ?d ?f ?g ?h ?j ?k ?l ?\;
			      ?z ?x ?c ?v ?b ?n ?m ?, ?. ?/
			      ?! ?\" ?# ?$ ?% ?& ?' ?( ?) ?~
			      ?Q ?W ?E ?R ?T ?Y ?U ?I ?O ?P
			      ?A ?S ?D ?F ?G ?H ?J ?K ?L ?+
			      ?Z ?X ?C ?V ?B ?N ?M ?< ?> ??))
    ("qwerty-us-shift" . (?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9 ?0
			     ?q ?w ?e ?r ?t ?y ?u ?i ?o ?p
			     ?a ?s ?d ?f ?g ?h ?j ?k ?l ?\;
			     ?z ?x ?c ?v ?b ?n ?m ?, ?. ?/
			     ?! ?\@ ?# ?$ ?% ?^ ?& ?* ?( ?)
			     ?Q ?W ?E ?R ?T ?Y ?U ?I ?O ?P
			     ?A ?S ?D ?F ?G ?H ?J ?K ?L ?:
			     ?Z ?X ?C ?V ?B ?N ?M ?< ?> ??))
    ("dvorak" . (?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9 ?0
		    ?' ?, ?. ?p ?y ?f ?g ?c ?r ?l
		    ?a ?o ?e ?u ?i ?d ?h ?t ?n ?s
		    ?\; ?q ?j ?k ?x ?b ?m ?w ?v ?z))
    ("dvorak-shift" . (?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9 ?0
			  ?' ?, ?. ?p ?y ?f ?g ?c ?r ?l
			  ?a ?o ?e ?u ?i ?d ?h ?t ?n ?s
			  ?\; ?q ?j ?k ?x ?b ?m ?w ?v ?z
			  ?! ?\@ ?# ?$ ?% ?^ ?& ?* ?( ?)
			  ?\" ?< ?> ?P ?Y ?F ?G ?C ?R ?L
			  ?A ?O ?E ?U ?I ?D ?H ?T ?N ?S
			  ?: ?Q ?J ?K ?X ?B ?M ?W ?V ?Z)))
  "*T�����ɤ��Ѥ��륭�������ѤΥǡ�����
����Ū�ˤϡ�����̾�ȥ����¤Ӥ��ȤΥꥹ�ȡ�
�����¤ӤǤϡ����夫���ˡ����Ϥ����ʸ����40�Ĥޤ���80���¤٤롣
80���¤٤���硢��Ⱦ��40�Ĥϡ���Ⱦ��40�ĤΥ��եȻ���ʸ���Ȥ��ư�����

���ޥ��`tcode-set-key-layout'���Ѥ��롣")

(defun tcode-set-key-layout (layout)
  "T�����ɤ��Ѥ��륭�����֤����ꤹ�롣"
  (interactive (let ((layout (completing-read
			      (format
			       "�������֤����򤷤Ƥ���������[%s] "
			       (car (car tcode-key-layout-list)))
			      tcode-key-layout-list)))
		 (list layout)))
  (let ((table (make-vector (1+ (- ?~ ? )) -1))
	(list (assoc layout tcode-key-layout-list)))
    (unless list
      (if (or (null layout)
	      (= (length layout) 0))
	  (unless (setq list (car tcode-key-layout-list))
	    (error "��������(tcode-key-layout-list)���������Ƥ��ޤ���"))
	(let ((i 0))
	  (while (< i 40)
	    (if (= i 0)
		(message "�����ܡ��ɤΥ����򺸾夫���˲����Ƥ���������")
	      (message "%d��%d����>" (/ i 40) (% i 40)))
	    (setq list (cons (read-char) list)
		  i (1+ i)))
	  (setq list (cons layout (nreverse list))))))
    (let ((i 0))
      (mapcar (lambda (char)
		(cond ((null char)
		       (aset table (- char ? ) -1))
		      ((and tcode-shift-lowercase
			    (/= (upcase char) char))
		       (aset table (- char ? ) i)
		       (aset table (- (upcase char) ? ) -2))
		      (t
		       (aset table (- char ? ) i)))
		(setq i (1+ i)))
	      (cdr list)))
    (let ((char ? ))
      (while (<= char ?~)
	(if (lookup-key tcode-mode-map (char-to-string char))
	    (aset table (- char ? ) -3))
	(setq char (1+ char))))
    (setq tcode-key-translation-rule-table table)
    list))

(defun tcode-set-key (key func &optional type)
  "T�����ɥ⡼�ɤΥ��������ꤹ�롣
�����ϼ��ΤȤ��ꡣ

KEY  ... ���ꤹ�륭����string��char��vector �Τ����줫��
FUNC ... TYPE �����ޥ�ɤξ��δؿ�̾�� TYPE �� 'command �ޤ��� -3 �ʳ���
         ���ϰ�̣������ʤ���(nil �ˤ��Ƥ����Ф褤)
TYPE ... ��ǽ�μ��ࡣ(�ѿ� `tcode-key-translation-rule-table' ����)
         ��ά���ϡ�FUNC �� nil �ΤȤ� 'literal �ǡ������Ǥʤ��Ȥ���
         'command��
         �ʲ��Υ���ܥ��Ȥ��롣

         literal      ����ʸ����
         lowercase    �б�����Ѿ�ʸ����
         command      tcode-mode-map �ˤ������ä����ޥ�ɡ�"
  (interactive (list (progn
		       (message "T�����ɥ⡼�ɤ������Ԥ�������? ")
		       (setq key (read-char)))
		     (read-command (format "��%c�פ˳�����Ƥ륳�ޥ�ɤ�? "
					   key))
		     prefix-arg))
  ;; type
  (cond ((null type)
	 (setq type (if (if (interactive-p) (commandp func) func)
			-3 -1)))
	((integerp type))
	((eq type 'literal)
	 (setq type -1))
	((eq type 'lowercase)
	 (setq type -2))
	((eq type 'command)
	 (setq type -3))
	(t
	 (error "TYPE ������������")))
  ;; key
  (cond ((char-or-string-p key)
	 (or (stringp key)
	     (setq key (char-to-string key))))
	((vectorp key)
	 (setq key (char-to-string (aref key 0))))
	(t
	 (error "KEY ������������")))
  ;; set keymap address
  (let ((addr (string-to-char key)))
    (if (and (>= addr ? )
	     (<= addr ?~))
	(aset tcode-key-translation-rule-table (- addr ? ) type)
      (error "��%s�פˤϳ�����Ƥ��ޤ���" key)))
  ;; set key binding
  (define-key tcode-mode-map key (and (= type -3)
				      func)))

(unless tcode-mode-map
  (setq tcode-mode-map (make-sparse-keymap))
  (tcode-set-key "?" 'tcode-mode-help)
  ;; tcode-mode �ΤȤ��ˡ���|�פ���Ͽ���Ѵ�
  (tcode-set-key "|" 'tcode-mazegaki-make-entry-and-finish)
  ;; tcode-mode �ΤȤ��ˡ���!�פǺ��
  (tcode-set-key "!" 'tcode-mazegaki-delete-entry-by-last-yomi)
  ;; tcode-mode �ΤȤ��ˡ���=�פ��䴰������
  (tcode-set-key "=" 'tcode-mazegaki-complete-and-convert))

(defun tcode-substitute-command-keys (string)
  "`substitute-command-keys' �� `tcode-mode-map' �Τ�Ȥ�Ŭ�Ѥ��롣"
  (let ((orig-map (current-local-map)))
    (prog2
	(use-local-map tcode-mode-map)
	(substitute-command-keys string)
      (use-local-map orig-map))))

(defun tcode-key-to-char (key)
  "�������ֹ椫���б�����ʸ�������롣"
  (let ((max (length tcode-key-translation-rule-table))
	(i 0))
    (catch 'found
      (while (< i max)
	(if (= key (aref tcode-key-translation-rule-table i))
	    (throw 'found t))
	(setq i (1+ i))))
    (+ i ? )))

(defun tcode-char-to-key (c)
  "Return virtual key code of character C.
See also `tcode-key-translation-rule-table'."
  (if (or (< c ? ) (> c ?~))
      -1
    (aref tcode-key-translation-rule-table (- c ? ))))

;;
;; T-Code main
;;

(defun tcode-on-p ()
  "T�����ɥ⡼�ɤ�����ˤʤäƤ��뤫�ɤ������֤���"
  (if (window-minibuffer-p (selected-window))
      tcode-mode-in-minibuffer
    tcode-mode))

(defun tcode-decode (strokes &optional table trace)
  "Decode STROKES with TABLE.
TABLE defaults `tcode-table'.
TRACE is a sub-stroke-sequence that has already been processed for decoding.

Return (status . value) where:

STATUS             VALUE
complete           decoded value
incomplete         sub-table
out-of-table       sub-strokes on table"
  (let* ((key (car strokes))
	 (table (or table
		    tcode-table))
	 (val (cond ((null table)
		     nil)
		    ((vectorp table)
		     (and (< key (length table))
			  (aref table key)))
		    ((and (consp table)
			  (not (eq (car table) 'lambda)))
		     (if (integerp (car table))
			 (if (= key (car table))
			     (cdr table))
		       (cdr (assq key table))))
		    (t
		     nil))))
    (if (null val)
	(cons 'out-of-table (nreverse trace))
      (let ((cont-strokes (cdr strokes)))
	(if cont-strokes
	    (tcode-decode cont-strokes val (cons key trace))
	  (cons (if (or (vectorp val)
			(and (consp val)
			     (not (eq (car val) 'lambda))))
		    'incomplete
		  'complete)
		(if (and (symbolp val)
			 (boundp val))
		    (eval val)
		  val)))))))

(defun tcode-default-key-binding (key)
  "Return the binding for command KEY in non-tcode-mode."
  (let (tcode-mode)
    (key-binding key)))

(defun tcode-decode-chars (c &optional table trace)
  "Decode C with TABLE.
Read characters if need.
TABLE defaults `tcode-table'.
TRACE is a sub-character-sequence that has already been processed for decoding.

Return value:
cons of (decoded value . character sequence)

where decoded value is one of:
char or string or function (symbol or lambda) ... code
nil ... no corresponding code
t ... cancel"
  (let ((key (tcode-char-to-key c)))
    (if (< key 0)
	;; C is not a key on TABLE
	(cons
	 (cond ((and trace
		     (memq c tcode-cancel-stroke-list))
		t)
	       ((and trace
		     (memq c tcode-verbose-stroke-list))
		(let ((rval (and tcode-another-table
				 (tcode-decode (mapcar 'tcode-char-to-key
						       (nreverse trace))
					       tcode-another-table))))
		  (or (cdr rval)
		      (mapconcat 'char-to-string trace nil))))
	       ((= key -1)
		c)
	       ((= key -2)
		(downcase c))
	       ((= key -3)
		(lookup-key tcode-mode-map (char-to-string c)))
	       (t
		(- key)))
	 (nreverse (cons c trace)))
      ;; C is a key on TABLE
      (if (>= key 40)
	  (setq tcode-shift-state t
		key (mod key 40)))
      (let* ((rval (tcode-decode (list key) table))
	     (status (car rval))
	     (val (cdr rval)))
	(cond ((eq status 'complete)
	       (cons val (nreverse (cons c trace))))
	      ((eq status 'incomplete)
	       (setq tcode-number-strokes (1+ tcode-number-strokes))
	       (run-hooks 'tcode-before-read-stroke-hook)
	       (let ((show-table (sit-for tcode-display-help-delay)))
		 (unwind-protect
		     (let ((echo-keystrokes 0))
		       (if show-table
			   (tcode-display-help-buffer
			    (tcode-draw-current-table val)
			    t))
		       (condition-case nil
			   (tcode-decode-chars (read-char) val (cons c trace))
			 (t (cons t (nreverse trace)))))
		   (if show-table
		       (tcode-auto-remove-help t)))))
	      (t
	       nil))))))

(defun tcode-eval-action (action)
  (cond ((null action)
	 (ding)
	 nil)
	((eq action t)
	 (setq this-command 'keyboard-quit) ; dummy
	 nil)
	((stringp action)
	 (string-to-list action))
	((char-or-string-p action)
	 (list action))
	(t
	 (undo-boundary)
	 (if (commandp action)
	     (progn
	       ;; ���ޥ�ɤμ¹�
	       (setq prefix-arg current-prefix-arg
		     this-command action)
	       (command-execute action))
	   ;; ���ޥ�ɤǤʤ��ؿ��μ¹�
	   (funcall action)
	   (setq this-command 'lambda))	; dummy
	 (setq tcode-special-occurrence
	       (1+ tcode-special-occurrence))
	 nil)))

(defun tcode-apply-filters (list)
  (mapcar (lambda (alist)
	    (if (eval (car alist))
		(let ((v (mapcar (lambda (c) (funcall (cdr alist) c))
				 list)))
		  (setq list (apply 'nconc
				    (mapcar (lambda (e) (if (stringp e)
						       (string-to-list e)
						     (list e)))
					    v))))))
	  tcode-input-filter-functions)
  list)

(defun tcode-input-method (ch)
  "The input method function for T-Code."
  (setq last-command 'self-insert-command
	last-command-char ch)
  (if input-method-verbose-flag
      (unless tcode-input-method-verbose-flag
	;; push some variables' values
	(setq tcode-orig-auto-help tcode-auto-help
	      tcode-orig-verbose-message tcode-verbose-message
	      tcode-orig-display-help-delay tcode-display-help-delay)
	;; set new values
	(setq tcode-auto-help t
	      tcode-verbose-message t
	      tcode-display-help-delay 2
	      tcode-input-method-verbose-flag t))
    (if tcode-input-method-verbose-flag
	;; pop pushed values
	(setq tcode-auto-help tcode-orig-auto-help
	      tcode-verbose-message tcode-orig-verbose-message
	      tcode-display-help-delay tcode-orig-display-help-delay
	      tcode-input-method-verbose-flag nil)))
  (let ((command (tcode-default-key-binding (char-to-string ch))))
    (if (or (and (boundp 'overriding-terminal-local-map)
		 overriding-terminal-local-map)
	    (and (boundp 'overriding-local-map)
		 overriding-local-map)
	    (eq this-command 'quoted-insert)
	    (not (memq command tcode-input-command-list))
	    (not (tcode-on-p)))
	;; pass input-method
	(list ch)
      ;; prosess as input-method
      (setq tcode-number-strokes (1+ tcode-number-strokes))
      (catch 'cancel
	(setq tcode-shift-state nil)
	(let* ((action (let (input-method-function) ; disable
			  (car (tcode-decode-chars ch))))
	       (evaled (let (input-method-function)
			 (tcode-eval-action action)))
	       (result (if evaled
			   (tcode-apply-filters evaled)
			 (throw 'cancel nil))))
	  (if (and result
		   (car result))
	      (setq tcode-input-chars (+ tcode-input-chars (length result))
		    tcode-help-char (char-to-string (car (reverse result)))))
	  (if (not (tcode-on-p)) ; if T-Code is disabled with a command
					; execution
	      (setq input-method-function nil))
	  result)))))

(defun tcode-insert (ch)
  "CH��Хåե����������롣"
  (unless (stringp ch)
    (setq ch (char-to-string ch)))
  (let* ((p (point))
	 (arg (prefix-numeric-value current-prefix-arg))
	 (n (if (consp current-prefix-arg)
		(/ (car current-prefix-arg) 2)
	      arg)))
    (while (> n 0)
      (insert ch)
      (setq n (1- n)))
    (if overwrite-mode
	(let ((str (buffer-substring p (point))))
	  (delete-text-in-column nil (+ (current-column)
					(string-width str)))))
    (if (and (boundp 'self-insert-after-hook)
	     self-insert-after-hook)
	(funcall self-insert-after-hook p (point)))
    (tcode-do-auto-fill)
    (run-hooks 'input-method-after-insert-chunk-hook)))

;;
;; 2�Х��ȱѿ���
;;

(defvar tcode-alnum-1-to-2-table
  (concat "�����ɡ������ǡʡˡ��ܡ��ݡ������������������������������䡩"
	  "�����£ãģţƣǣȣɣʣˣ̣ͣΣϣУѣңӣԣգ֣ףأ٣ڡΡ�ϡ���"
	  "�ƣ���������������������������������Сáѡ�")
  "1�Х��ȱѿ��� ' '..'~' ��2�Х��ȱѿ������Ѵ�/���Ѵ����뤿��Υơ��֥�")

(defun tcode-toggle-alnum-mode ()
  (interactive)
  (setq tcode-alnum-2-byte (not tcode-alnum-2-byte))
  (tcode-mode-line-redisplay))

(defun tcode-check-alnum-1-to-2-table ()
  (if (stringp tcode-alnum-1-to-2-table)
      (setq tcode-alnum-1-to-2-table
	    (vconcat (string-to-list tcode-alnum-1-to-2-table)))))

(defun tcode-1-to-2-region (beg end)
  "�꡼��������1�Х��ȱѿ�����2�Х��Ȥ��Ѵ����롣"
  (interactive "*r")
  (tcode-check-alnum-1-to-2-table)
  (save-excursion
    (save-restriction
      (goto-char beg)
      (narrow-to-region beg end)
      (let (char)
	(while (progn (skip-chars-forward "^!-~" (point-max))
		      (< (point) (point-max)))
	  (setq char (following-char))
	  (tcode-delete-char 1)
	  (insert (char-to-string (tcode-1-to-2 char))))))))

(defun tcode-1-to-2 (char)
  "1�Х��ȱѿ���CHAR��2�Х��Ȥ��Ѵ����롣"
  (tcode-check-alnum-1-to-2-table)
  (if (and (<= ?! char) (<= char ?~))
      (aref tcode-alnum-1-to-2-table (- char ? ))
    char))

(defun tcode-2-to-1-region (beg end)
  "�꡼��������2�Х��ȱѿ�����1�Х��Ȥ��Ѵ����롣"
  (interactive "*r")
  (tcode-check-alnum-1-to-2-table)
  (save-excursion
    (save-restriction
      (goto-char beg)
      (narrow-to-region beg end)
      (let ((alnum-2byte-regexp (concat "["
					(mapconcat 'char-to-string
						   tcode-alnum-1-to-2-table
						   nil)
					"]+"))
	    str)
	(while (re-search-forward alnum-2byte-regexp nil t)
	  (setq str (buffer-substring (match-beginning 0) (match-end 0)))
	  (delete-region (match-beginning 0) (match-end 0))
	  (insert (mapconcat (lambda (c)
			       (char-to-string (tcode-2-to-1 c)))
			     (string-to-list str)
			     nil)))))))

(defun tcode-2-to-1 (char)
  "2�Х��ȱѿ���CHAR��1�Х��Ȥ��Ѵ����롣"
  (tcode-check-alnum-1-to-2-table)
  (let ((ch 0))
    (catch 'found
      (while (< ch 95)
	(if (= (aref tcode-alnum-1-to-2-table ch) char)
	    (throw 'found (+ ch 32)))
	(setq ch (1+ ch)))
      char)))

;;
;; ������ɽ
;;

(defun tcode-switch-variable (&optional arg)
  "(`tcode-table' ���) �ѿ����ͤ��ڤ��ؤ��롣
�ڤ��ؤ����ѿ��Ȥ����ͤ� `tcode-switch-table-list' �ǻ��ꤹ�롣
ARG �� nil �Ǥʤ��Ȥ���ARG ���ܤ��Ȥ��ڤ��ؤ��롣"
  (interactive "P")
  (message
   (mapconcat
    'identity
    (mapcar
     (lambda (elm)
       (set (car elm) (cdr elm)))
     (let ((table (nth (setq tcode-current-switch-table
			     (if arg
				 (1- (prefix-numeric-value arg))
			       (1+ tcode-current-switch-table)))
		       tcode-switch-table-list)))
       (unless table
	 (setq tcode-current-switch-table 0
	       table (car tcode-switch-table-list)))
       table))
    nil)))

(defun tcode-function-p (obj)
  (cond ((or (null obj)
	     (arrayp obj))
	 nil)
	((symbolp obj)
	 (fboundp obj))
	((consp obj)
	 (eq (car obj) 'lambda))
	(t
	 nil)))

(defun tcode-put-stroke-property (value strokes &optional without-overwrite)
  (cond ((char-or-string-p value)
	 (let ((str (if (stringp value)
			value
		      (char-to-string value))))
	   (when strokes
	     (if (not without-overwrite)
		 ;; remove old property
		 (tcode-put-stroke-property
		  (cdr (tcode-decode (append strokes nil)))
		  nil
		  without-overwrite)))
	   (put (intern str tcode-stroke-table) 'stroke strokes)))
	((and (symbolp value)
	      (boundp value))
	 (let ((value (eval value)))
	   (if (char-or-string-p value)
	       (tcode-put-stroke-property value strokes without-overwrite))))))

(defun tcode-set-stroke-property (table strokes &optional without-overwrite)
  (cond ((or (null table)
	     (tcode-function-p table)))
	((char-or-string-p table)
	 (tcode-put-stroke-property table (vconcat strokes) without-overwrite))
	((consp table)
	 (mapcar (lambda (ent)
		   (tcode-set-stroke-property (cdr ent)
					      (append strokes
						      (list (car ent)))
					      without-overwrite))
		 table))
	((vectorp table)
	 (let ((i 0))
	   (while (< i 40)
	     (tcode-set-stroke-property (aref table i)
					(append strokes (list i))
					without-overwrite)
	     (setq i (1+ i)))))
	((and (symbolp table)
	      (boundp table))
	 (tcode-set-stroke-property (eval table) strokes without-overwrite))))

(defun tcode-set-action (table strokes value)
  "TABLE ��Ρ��������� STROKES ������������ VALUE �����ꤹ�롣"
  (let ((key (car strokes))
	(substrokes (cdr strokes))
	(subsubstrokes (nthcdr 2 strokes)))
    (cond (subsubstrokes
	   (tcode-set-action (if (consp table)
				 (cdr (assq key table))
			       (aref table key))
			     substrokes
			     value))
	  (substrokes
	   (cond ((or (and (char-or-string-p table)
			   (not (stringp table)))
		      (null table))
		  (list key (tcode-set-action nil substrokes value)))
		 ((vectorp table)
		  (aset table key
			(tcode-set-action (aref table key) substrokes value))
		  table)
		 (t
		  (let ((orig table))
		    (if (null (setq table (assq key table)))
			(nconc orig
			       (list (list key
					   (tcode-set-action
					    nil substrokes value))))
		      (setcdr table
			      (tcode-set-action (cdr table) substrokes value))
		      table)))))
	  (t
	   ;; add key
	   (if (listp table)
	       (cond ((null table)
		      (cons key value))
		     ((and (char-or-string-p (car table))
			   (not (stringp (car table)))
			   (not (consp (cdr table))))
		      (list table (cons key value)))
		     (t
		      (let ((old (assq key table)))
			(while old
			  (setq table (delq old table)
				old (assq key table)))
			(if (null table)
			    (cons key value)
			  (nconc (list (cons key value)) table)))))
	     (if (vectorp table)
		 (progn
		   (aset table key value)
		   table)
	       (list (cons key value))))))))

(defun tcode-set-action-to-table (strokes value)
  "�����������Ѥ������ơ��֥�������� STROKES ���Ф��� VALUE �����ꤹ�롣
ư��(VALUE)�Ȥ��ƻ���Ǥ���Τϰʲ��ΤȤ��ꡣ
    - ���ޥ�� (symbol)		���Υ��ޥ�ɤ�¹Ԥ��롣
    - �ؿ� (symbol, lambda��)	���δؿ�������ʤ��ǸƤ֡�
    - �ѿ� (symbol)		ɾ��������̤�ư���Ԥ���
    - ɽ (vector)		���ˤ���ɽ�˽��ä�ư���Ԥ���
    - �ꥹ�� (list)		���ˤ��Υꥹ�Ȥ˽��ä�ư���Ԥ���
    - ʸ���� (string)		����ʸ������������롣
    - ʸ�� (char)		����ʸ�����������롣

  ������ϥ��������ϤΥꥹ�Ȥޤ��ϥ��������ϡ�
���������Ϥ���ꤹ��ȡ��Ǹ�� SPC �򲡤����Ȥ���ư������ꤹ�롣"
  (cond ((consp strokes)
	 (tcode-set-stroke-property value strokes)
	 (tcode-set-action tcode-table strokes value))
	((and (char-or-string-p strokes)
	      (not (stringp strokes)))
	 (unless tcode-another-table
	   (setq tcode-another-table (make-vector 40 nil)))
	 (aset tcode-another-table strokes value))
	(t
	 (error "������λ��̵꤬���Ǥ���"))))

(defun tcode-load-table (filename)
  (run-hooks 'tcode-before-load-table-hook)
  (let ((k1 0) k2 newval char)
    (load filename)
    (setq tcode-table (make-vector 40 nil))
    (while (< k1 40)
      (aset tcode-table k1 (make-vector 40 nil))
      (setq k1 (1+ k1)))
    (setq k1 0)
    (while (< k1 40)
      (let ((v (aref tcode-tbl k1)))
	(if (null v)
	    ()
	  (setq newval (vconcat (delq ?  (string-to-list v))))
	  (unless (= (length newval) 40)
	    (error "Table corrupted at line %d." (1+ k1)))
	  (setq k2 0)
	  (while (< k2 40)
	    (unless (memq (setq char (aref newval k2))
			  tcode-non-2-stroke-char-list)
	      (aset (aref tcode-table k2) k1 char))
	    (setq k2 (1+ k2)))))
      (setq k1 (1+ k1)))
    (setq tcode-tbl nil)		; free
    ;; 'stroke property ������롣
    (setq tcode-stroke-table (and (> tcode-stroke-table-size 0)
				  (make-vector tcode-stroke-table-size 0)))
    (tcode-set-stroke-property tcode-table nil t)
    ;; ���ޥ�ɤ�ơ��֥����Ͽ���롣
    (mapcar (lambda (elm)
	      (tcode-set-action-to-table (car elm) (cdr elm)))
	    tcode-special-commands-alist)
    (setq tcode-special-commands-alist nil) ; free
    (if (get-buffer tcode-stroke-buffer-name)
	(kill-buffer tcode-stroke-buffer-name))
    (run-hooks 'tcode-after-load-table-hook)
    (tcode-bushu-init 1)
    (tcode-clear)
    (tcode-mode-line-redisplay)))

(defun tcode-encode (char)
  "ʸ��CHAR���Ǥ���(�����Υꥹ��)���Ѵ����롣ľ�����ϤǤ��ʤ����nil���֤���"
  (let ((ch (char-to-string char)))
    (or (append (get (intern-soft ch tcode-stroke-table) 'stroke) nil)
	(let ((ch (char-to-string (tcode-2-to-1 char))))
	  (append (get (intern-soft ch tcode-stroke-table) 'stroke) nil)))))

;;
;; �������
;;

(defun tcode-bushu-init (level)
  "��������Ѵ��ν������Ԥ���
����LEVEL���ѿ�`tcode-bushu-on-demand'��꾮�����ä���Ԥ�ʤ���"
  (interactive (list 999))
  (when (>= level tcode-bushu-on-demand)
    (require 'tc-bushu)
    (tcode-bushu-load-dictionary)
    (run-hooks 'tcode-bushu-ready-hook)))

(defun tcode-initialize ()
  "������ɽ�ʤɤν������Ԥ���
Emacs����ư����Ƥ���ǽ��tcode-mode�Ǽ¹Ԥ���롣
`tcode-ready-hook' ��Ƥ֡�"
  (when (and tcode-init-file-name
	     (not (file-exists-p tcode-init-file-name)))
    (if (y-or-n-p (format "����ե�����%s������ޤ��󡣺������ޤ���?"
			  tcode-init-file-name))
	(tcode-install)
      (call-interactively 'tcode-set-key-layout)))
  (if (and tcode-data-directory
	   (not (file-exists-p tcode-data-directory)))
      (error "`tcode-data-directory'(%s)��¸�ߤ��ޤ���"
	     tcode-data-directory))
  (if tcode-use-isearch
      (require tcode-isearch-type))
  (add-hook 'minibuffer-exit-hook 'tcode-exit-minibuffer)
  (when (and (fboundp 'inactivate-input-method)
	     (fboundp 'defadvice))
    (defadvice inactivate-input-method (after
					turn-off-current-input-method-title
					activate)
      "Turn off `current-input-method-title' for mode line."
      (and (string= (car input-method-history)
		    tcode-current-package)
	   (setq current-input-method-title
		 tcode-transparent-mode-indicator)))
    (defadvice activate-input-method (after
				      turn-on-current-input-method-title
				      activate)
      "Turn on `current-input-method-title' for mode line."
      (and (string= (car input-method-history)
		    tcode-current-package)
	   (tcode-mode-line-redisplay))))
  (provide 'tcode-ready)
  (run-hooks 'tcode-ready-hook)
  (tcode-verbose-message
   (tcode-substitute-command-keys
    "T�����ɥ⡼�ɤǤϡ�\\[tcode-mode-help]�פǥإ�פ�ɽ������ޤ���")))

;;
;; �⡼�ɤ��ڤ��ؤ�
;;

(defun tcode-mode-line-redisplay ()
  (setq current-input-method-title
	(if (tcode-on-p)
	    (concat (if tcode-alnum-2-byte
			tcode-alnum-2byte-tcode-mode-indicator
		      tcode-tcode-mode-indicator)
		    (if tcode-katakana-mode
			tcode-katakana-mode-indicator
		      tcode-hiragana-mode-indicator))
	  tcode-transparent-mode-indicator))
  (and (window-minibuffer-p (selected-window))
       (boundp 'minibuffer-preprompt)
       (setq minibuffer-preprompt
	     (list "[" (eval tcode-mode-indicator) "]")))
  (set-buffer-modified-p (buffer-modified-p)))

(defun tcode-clear ()
  "��䤳�����⡼�ɤ����äƤ���Τ��������ꥢ���롣
�إ���ѥ�����ɥ���õ�롣"
  (interactive)
  (tcode-auto-remove-help t)
  (run-hooks 'tcode-clear-hook))

(defun tcode-activate (&optional arg)
  "T�����ɥ⡼�ɤ�ͭ���ˤ��롣ARG����������ΤȤ���̵���ˤ��롣

T�����ɥ⡼�ɤˤĤ��Ƥϡ�\\[tcode-mode-help] ��ɽ�������إ�פ򻲾ȡ�"
  (if (and arg
	   (< (prefix-numeric-value arg) 0))
      ;; inactivate T-Code mode
      (unwind-protect
	  (progn
	    (if (window-minibuffer-p (selected-window))
		(setq tcode-mode-in-minibuffer nil))
	    (setq tcode-mode nil
		  tcode-self-insert-non-undo-count 1)
	    (tcode-clear)
	    (run-hooks 'input-method-inactivate-hook))
	(setq input-method-function nil))
    ;; activate T-Code mode
    (if (window-minibuffer-p (selected-window))
	(setq tcode-mode-in-minibuffer t))
    (setq tcode-mode t
	  tcode-self-insert-non-undo-count 1)
    (unless (featurep 'tcode-ready)
      (tcode-initialize))
    (unless tcode-ready-in-this-buffer
      ;; �Хåե����Ȥ�T�����ɤν������Ԥ���`tcode-mode-hook'��Ƥ֡�
      (setq tcode-ready-in-this-buffer t)
      (run-hooks 'tcode-mode-hook))
    (run-hooks 'input-method-activate-hook)
    (when tcode-use-input-method
      (set (make-local-variable 'input-method-function)
	   'tcode-input-method)))
  (run-hooks 'tcode-toggle-hook)
  (tcode-mode-line-redisplay))

(defun tcode-inactivate ()
  "T�����ɥ⡼�ɤ�̵���ˤ��롣"
  (tcode-activate -1))

(defun tcode-exit-minibuffer ()
  (tcode-inactivate)
  (setq current-input-method nil)
  (if (boundp 'minibuffer-preprompt)
      (setq minibuffer-preprompt nil)))

(defun tcode-toggle-katakana-mode (arg)
  "�������ʥ⡼�ɤ��ڤ��ؤ��롣"
  (interactive "P")
  (setq tcode-katakana-mode (if (null arg)
				(not tcode-katakana-mode)
			      (>= (prefix-numeric-value arg) 0)))
  (tcode-mode-line-redisplay))

;;
;; input method interface
;;
(defvar tcode-current-package nil)

(defvar tcode-transparent-mode-indicator "--")

;;;###autoload
(defun tcode-use-package (package-name &rest libraries)
  "Start using T-Code package PACKAGE-NAME.
The remaining arguments are libraries to be loaded before using the package."
  (if (equal tcode-current-package package-name)
      ()
    (let ((table-name (cdr (assoc package-name tcode-package-name-alist))))
      (or table-name
	  (error "No package named %s" package-name))
      (while libraries
	(load (car libraries))
	(setq libraries (cdr libraries)))
      (tcode-load-table table-name))
    (setq tcode-current-package package-name))
  (setq inactivate-current-input-method-function 'tcode-inactivate
	describe-current-input-method-function 'tcode-mode-help)
  (setq current-input-method-title 'tcode-mode-indicator)
  (tcode-activate))

;;
;; input method emulation
;;

(defvar tcode-self-insert-non-undo-count 0)
(defvar tcode-cancel-undo-boundary-commands '(self-insert-command))

(defun tcode-cancel-undo-boundary ()
  "ʸ�������������Ȥ��ˡ��ޤȤ�� undo �Ǥ���褦��Ĵ�����롣"
  (if (or (not (memq last-command
		     (if (memq this-command
			       tcode-cancel-undo-boundary-commands)
			 tcode-cancel-undo-boundary-commands
		       (setq tcode-cancel-undo-boundary-commands
			     (cons this-command
				   tcode-cancel-undo-boundary-commands)))))
	  (>= tcode-self-insert-non-undo-count (if (tcode-on-p) 10 20)))
      (progn
	(undo-boundary)
	(setq tcode-self-insert-non-undo-count 1))
    (cancel-undo-boundary)
    (setq tcode-self-insert-non-undo-count
	  (1+ tcode-self-insert-non-undo-count))))

(defun tcode-self-insert-command (&optional arg)
  "Encode Tcode character and insert."
  (interactive "P")
  (tcode-cancel-undo-boundary)
  (let ((events (tcode-input-method last-command-char)))
    (while events
      (let* ((ch (car events))
	     (command (tcode-default-key-binding (char-to-string ch))))
	(if (and (tcode-nemacs-p)
		 (not (commandp command)))
	    (tcode-insert ch)
	  (if (not (commandp command))
	      (setq command 'self-insert-command))
	  (setq prefix-arg current-prefix-arg
		this-command command
		last-command-char ch) ; for self-insert-command
	  (command-execute command)))
      (setq events (cdr events)))))

(unless tcode-use-input-method
  ;; �ޥ��ʡ��⡼��
  (unless (assq 'tcode-mode minor-mode-alist)
    (setq minor-mode-alist
	  (cons (list 'tcode-mode nil) minor-mode-alist)))

  (defvar tcode-minor-mode-map nil)

  (if (boundp 'minor-mode-map-alist)
      (progn
	(unless tcode-minor-mode-map
	  (setq tcode-minor-mode-map (make-keymap))
	  (let ((i ? ))
	    (while (< i ?~)
	      (define-key tcode-minor-mode-map
		(char-to-string i)
		'tcode-self-insert-command)
	      (setq i (1+ i)))))
	;; ��ưŪ�˥ޥ��ʡ��⡼�ɥ����ޥåפ��ѹ�
	(or (assq 'tcode-mode minor-mode-map-alist)
	    (setq minor-mode-map-alist
		  (cons (cons 'tcode-mode tcode-minor-mode-map)
			minor-mode-map-alist)))

	(defadvice tcode-activate (after
				   raise-minor-mode-map
				   activate)
	  "Raise minor mode keymap."
	  ;; detach and re-attach tcode-minor-mode-map
	  (setq minor-mode-map-alist 
		(let ((map (or (assq 'tcode-mode minor-mode-map-alist)
			       (cons 'tcode-mode tcode-minor-mode-map))))
		  (cons map (delq map minor-mode-map-alist)))))

	(defun tcode-mode (&optional arg)
	  "T-Code mode.
Type \\[tcode-mode-help] for more detail."
	  (interactive "P")
	  (tcode-activate (or arg
			      (if tcode-mode -1 1)))
	  (force-mode-line-update)))
    ;; �����ǥޥ��ʡ��⡼�ɥ����ޥåפ��ѹ�
    (make-variable-buffer-local 'tcode-minor-mode-map)
    (defvar tcode-original-local-map nil)
    (make-variable-buffer-local 'tcode-original-local-map)

    ;; override
    (defun tcode-default-key-binding (key)
      "Return the binding for command KEY in non-tcode-mode."
      (or (lookup-key tcode-original-local-map key)
	  (lookup-key global-map key)))

    (unless (fboundp 'tcode:tcode-activate)
      (fset 'tcode:tcode-activate
	    (symbol-function 'tcode-activate))
      (defun tcode-activate (&optional arg)
	(or tcode-mode
	    (setq tcode-original-local-map (or (current-local-map)
					       (current-global-map))))
	(unless tcode-minor-mode-map
	  (setq tcode-minor-mode-map (if tcode-original-local-map
					 (copy-keymap tcode-original-local-map)
				       (make-sparse-keymap)))
	  (let ((i ? ))
	    (while (< i ?~)
	      (define-key tcode-minor-mode-map
		(char-to-string i)
		'tcode-self-insert-command)
	      (setq i (1+ i)))))
	(tcode:tcode-activate arg)
	(use-local-map (if tcode-mode
			   tcode-minor-mode-map
			 (or tcode-original-local-map
			     (current-local-map)
			     (current-global-map))))))))

;;
;; Help
;;

(defun tcode-action-to-printable (action)
  (cond ((or (null action)
	     (stringp action))
	 action)
	((char-or-string-p action)
	 (char-to-string action))
	((and (symbolp action)
	      (boundp action))
	 (tcode-action-to-printable (eval action)))
	(t
	 "*")))

(defun tcode-draw-current-table (table)
  "TABLE ���顢�������Ϥˤ�����������ʸ������ɽ������ɽ��������"
  (tcode-draw-table
   (if (vectorp table)
       (let ((draw-table (copy-sequence table))
	     (i 0))
	 (while (< i 40)
	   (aset draw-table i (tcode-action-to-printable (aref draw-table i)))
	   (setq i (1+ i)))
	 draw-table)
     ;; table �ϥꥹ��
     (let ((draw-table (make-vector 40 nil)))
       (mapcar (lambda (elm)
		 (aset draw-table
		       (car elm)
		       (tcode-action-to-printable (cdr elm))))
	       table)
       draw-table))
   1 1))

(defun tcode-verbose-message (message &optional non-verbose-message)
  "�ѿ� `tcode-verbose-message' �� non-nil �ξ��ˤϡ� MESSAGE ��ɽ�����롣
�����Ǥʤ��Ȥ��� NON-VERBOSE-MESSAGE ������С������ɽ�����롣"
  (if (or tcode-verbose-message
	  non-verbose-message)
      (message (if tcode-verbose-message message non-verbose-message))))

(defun tcode-draw-table (table page whole-page)
  "����ɽ�� TABLE �˴�Ť�������ɽ���Ϥ��ʤ���"
  (let ((buf (get-buffer-create " *tcode: table*"))
        (sep0 [" " " " " " " " "  " "  " "  " " " " " " " ""])
        (sep1 ["[" " " " " " " "] " "  " " [" " " " " " " "]"])
        (none-str "-"))
    (save-excursion
      (set-buffer buf)
      (erase-buffer)
      (let ((fv (make-vector 10 nil))
	    (i 0)
	    (offset 0))
        (while (< i 10)
          (aset fv i (format "%%%s%ds"
                             (if (or (>= i 5)
				     (tcode-nemacs-p))
				 ""
			       "-")
                             (apply 'max 4
                                    (mapcar
                                     (lambda (x)
                                       (string-width
                                        (or (aref table (+ x i)) none-str)))
                                     '(0 10 20 30)))))
          (setq i (1+ i)))
        (while (< offset 40)
          (setq i 0)
          (while (< i 10)
            (insert (aref (if (= offset 0) sep0 sep1) i)
                    (format (aref fv i) (or (aref table (+ offset i))
                                            none-str)))
            (setq i (1+ i)))
          (insert (aref (if (= offset 0) sep0 sep1) 10) "\n")
          (setq offset (+ offset 10))))
      (unless (= whole-page 1)
        (backward-char 1)
        (insert (format "     (%d/%d)" page whole-page))))
    buf))

(defun tcode-display-help-buffer (buffer &optional display-only append)
  "\"*T-Code Help*\" �Ȥ����Хåե��� BUFFER �����Ƥ�ɽ�����롣
ɽ������ľ��˶������Ϥ����ȡ� DISPLAY-ONLY �� nil �ʤ�Ф��ΥХåե�
��õ�롣 APPEND �� nil �Ǥʤ��Ȥ��ϡ��������Ƥ��ɲä���ɽ�����롣"
  ;; ������ɥ���������¸
  (unless (get-buffer-window tcode-help-buffer-name)
    (setq tcode-window-configuration-before-help
	  (if (one-window-p)
	      nil
	    (current-window-configuration))))
  ;; ɽ���������Ƥ��������ɽ������
  (let (previous-contents)
    (if append
	(let ((buf (get-buffer tcode-help-buffer-name)))
	  (setq previous-contents (and buf
				       (save-excursion
					 (set-buffer buf)
					 (buffer-string))))))
    (with-output-to-temp-buffer tcode-help-buffer-name
      (when previous-contents
	(princ previous-contents)
	(princ "\n"))
      (princ (save-excursion (set-buffer buffer) (buffer-string))))
    (if (fboundp 'help-mode)
	(save-excursion
	  (set-buffer (get-buffer tcode-help-buffer-name))
	  (help-mode))))
  ;; ������ɥ����礭����Ĵ��
  (let ((orig-win (selected-window))
	(new-win (get-buffer-window tcode-help-buffer-name))
	(window-min-height 2))
    (when new-win
      (select-window new-win)
      (if (and (or (not tcode-window-configuration-before-help)
		   tcode-adjust-window-for-help)
	       (= (frame-width) (window-width)))
	  (enlarge-window (- (1+ (min (count-lines (point-min) (point-max))
				      tcode-help-window-height-max))
			     (- (window-height)
				tcode-adjust-window-additional-height))))
      (set-window-start (selected-window) (point-min))
      (unless (one-window-p)
	(select-window orig-win))))
  ;; ɽ����ν���
  (setq tcode-auto-remove-help-current-count 0)
  (unless (or display-only
	      (memq this-command tcode-no-wait-display-help-command-list))
    (tcode-verbose-message "���ڡ��������ǥإ�פ��ä��ޤ���" " ")
    (let ((ch (read-char)))
      (if (/= ch ? )
	  (tcode-redo-command ch)
	(tcode-auto-remove-help t))
      (message ""))))

(defun tcode-auto-remove-help (&optional immediate)
  "�إ�פ�ưŪ�˾õ�롣
�õ���Τϡ��إ�פ�ɽ������Ƥ���
���δؿ��� `tcode-auto-remove-help-count' ��ƤФ줿�Ȥ���"
  (when (or immediate
	    (and tcode-auto-remove-help-count
		 (>= (setq tcode-auto-remove-help-current-count
			   (1+ tcode-auto-remove-help-current-count))
		     tcode-auto-remove-help-count)))
    (let ((help-buf (get-buffer tcode-help-buffer-name))
	  help-win)
      (and help-buf
	   (not (eq help-buf (current-buffer)))
	   (setq help-win (get-buffer-window help-buf))
	   (cond (tcode-window-configuration-before-help
		  (save-excursion
		    (if tcode-adjust-window-for-help
			(set-window-configuration
			 tcode-window-configuration-before-help))))
		 ((not (one-window-p))
		  (delete-window help-win))))
      (and help-buf
	   (or immediate
	       (not (eq help-buf (current-buffer))))
	   (kill-buffer help-buf)))))

;;;
;;; �ƥ⥸�塼��ǻ��Ѥ������Ѵؿ�
;;;

(defun tcode-removable-fill-prefix-p ()
  "�������Ƥ�褤 fill-prefix ����
�������Ƥ�褤 fill-prefix �Ȥϡ�
��Ƭ���� point �ޤǤ� fill-prefix �Ǥ��ꡢ����
�������ιԤ� fill-prefix �ǻϤޤäƤ�����򤤤���"
  (and fill-prefix
       (and (string= fill-prefix
		     (buffer-substring (save-excursion
					 (beginning-of-line)
					 (point))
				       (point)))
	    (save-excursion
	      (and (= (forward-line -1) 0)
		   (looking-at (regexp-quote fill-prefix)))))))

(defun tcode-skip-blank-backward ()
  "��Ƭ���� point �ޤǤ�����ʤ� point �����ιԤι����˰�ư���롣
`fill-prefix'�����ꤵ��Ƥ���Ȥ��ϡ�����ʸ�����̵�뤹�롣"
  (let ((p (point))
	(fill-prefix-end (and fill-prefix
			      (save-excursion
				(beginning-of-line)
				(and (looking-at (regexp-quote fill-prefix))
				     (match-end 0))))))
    (cond ((bobp)
	   nil)
	  ((bolp)
	   ;; ���ιԤ�
	   (forward-line -1)
	   (end-of-line)
	   (if (bobp)
	       nil
	     (point)))
	  ((null fill-prefix-end) ; fill-prefix ���ʤ���硣
	   (if (save-excursion
		 (beginning-of-line)
		 (or (not (re-search-forward "^\\s +" p t))
		     (/= (point) p)))
	       p
	     ;; ��Ƭ����ζ����ȤФ�
	     (forward-line -1)
	     (end-of-line)
	     (if (bobp)
		 nil
	       (point))))
	  ((not (save-excursion
		  (goto-char fill-prefix-end)
		  (tcode-removable-fill-prefix-p)))
	   ;; ľ���ιԤ� fill-prefix �ǻϤޤäƤ��ʤ���硣
	   (if (= fill-prefix-end p)
	       nil    ; �������ƤϤ����ʤ� fill-prefix
	     p))
	  ((<= p fill-prefix-end) ; fill-prefix ����ˤ����硣
	   (forward-line -1)
	   (end-of-line)
	   (if (bobp)
	       nil
	     (point)))
	  (t		  ; fill-prefix + ʸ����ξ�硣
	   (if (save-excursion
		 (beginning-of-line)
		 (or (not (re-search-forward
			   (concat "^" (regexp-quote fill-prefix) "\\s +")
			   p t))
		     (/= (point) p)))
	       p
	     ;; fill-prefix + �����ȤФ���
	     (forward-line -1)
	     (end-of-line)
	     (point))))))

(defun tcode-scan-backward (max &optional terminate-char-list)
  "�� point �����Ƭ�����ˤ������ܸ���ޤ��ϱ�ñ���ĤΥꥹ�Ȥ��֤���
�ꥹ�Ȥ����Ǥ�(POINT . \"ʸ����\")�Ǥ��롣
�����ǡ���ʸ����פϡ����ܸ�ʸ���ξ��1ʸ������ʸ���ξ���1ñ��Ǥ��롣
�ꥹ�Ȥν��֤Ȥ��Ƥϡ��Хåե�����Ƭ�˶ᤤʸ������Ƭ��¦�ˤʤ롣
�ꥹ�Ȥ�Ĺ���Ϻ��� MAX ʸ���Ǥ��롣"
  (save-excursion
    (let (ch context)
      (while (and (< (length context) max)
		  (tcode-skip-blank-backward)
		  (or (not (memq (setq ch (tcode-preceding-char))
				 terminate-char-list))
		      (null context))
		  (not (bobp)))
	(if (<= ch 255)
  	    ;; ����ե��٥åȤξ��
	    (let ((end (point))
		  (beg (progn
			 (if (= (char-syntax ch) ?w)
			     ;; ��
			     (while (and (not (bobp))
					 (= (char-syntax
					     (setq ch (tcode-preceding-char)))
					    ?w)
					 (<= ch 255))
			       (tcode-forward-char -1))
			   ;; ����
			   (tcode-forward-char -1))
			 (point))))
	      (setq context (cons
			     (cons beg
				   (buffer-substring-no-properties beg end))
			     context)))
	  ;; ���ܸ�ʸ���ξ��(1������)
	  (tcode-forward-char -1)
	  (setq context (cons (cons (point) (char-to-string ch))
			      context))))
      context)))

(defun tcode-path-for-read (file)
  (let ((user-file (expand-file-name file tcode-data-directory)))
    (if (file-exists-p user-file)
	user-file
      (expand-file-name file tcode-site-data-directory))))

(defun tcode-path-for-write (file)
  (expand-file-name file tcode-data-directory))

(defun tcode-set-work-buffer (bufname filename &optional force noerror)
  "�Խ��оݥХåե���FILENAME �����Ƥ���ĥХåե� BUFNAME �ˤ��롣
�ե����� FILENAME ���ޤ��ɤ߹��ޤ�Ƥ��ʤ����ˤ��ɤ߹��ࡣ
�֤��ͤȤ��ơ����ꤵ�줿�Хåե����֤���

FORCE �� nil �Ǥʤ��Ȥ��ϡ����ɤ߹��ߤ�Ԥ���
NOERROR �� nil �Ǥʤ��Ȥ��ϡ�FILENAME �����ꤵ��Ƥ��ʤ�����
�ե����뤬¸�ߤ��ʤ����Ǥ⡢���ΥХåե���������롣"
  (let ((buffer (get-buffer bufname))
	(file (tcode-path-for-read filename)))
    (if (and buffer
	     (not force))
	(set-buffer buffer)
      (if (and file (file-exists-p file))
	  (prog2
	      (when tcode-verbose-message
		(message "�ե����� %s �ɤ߹�����..." file))
	      (set-buffer (get-buffer-create bufname))
	    (erase-buffer)
	    (insert-file-contents file)
	    (set-buffer-modified-p nil)
	    (when tcode-verbose-message
	      (message "�ե����� %s �ɤ߹�����...��λ" file)))
	(if noerror
	    (set-buffer (get-buffer-create bufname))
	  (error "�ե����� %s ��¸�ߤ��ޤ���" file))))))

(defun tcode-save-buffer (bufname filename &optional backup-inhibited)
  "BUFNAME�ΥХåե����ѹ�����Ƥ����FILENAME�Υե��������¸���롣
BACKUP-INHIBITED �� nil �Ǥʤ����ϡ��Хå����åץե�����κ�����
�Ԥ�ʤ���"
  (let ((buffer (get-buffer bufname))
	(file-path (tcode-path-for-write filename)))
    (when (and tcode-data-directory
	       buffer
	       (buffer-modified-p buffer)
	       (file-writable-p file-path))
      (save-excursion
	(set-buffer buffer)
	(unless (or backup-inhibited
		    (not (file-exists-p file-path)))
	  (rename-file file-path (make-backup-file-name file-path) t))
	(write-region (point-min) (point-max) file-path)
	(set-buffer-modified-p nil)))))

;;
;; Emacs��λ���Υǡ�����¸
;;

(defun tcode-save-dictionaries (&optional backup-inhibited)
  "T�����ɤ��Ѥ��뼭������ѹ�����Ƥ������¸���롣"
  (interactive)
  (mapcar (lambda (dic)
	    (let ((bufname (car dic))
		  (filename (cdr dic)))
	      (tcode-save-buffer bufname filename
				 backup-inhibited)))
	  tcode-dictionaries))

(defun tcode-kill-emacs-function ()
  (tcode-save-dictionaries)
  (tcode-record))

(add-hook 'kill-emacs-hook 'tcode-kill-emacs-function)

(defun tcode-record ()
  (when (and tcode-record-file-name
	     (> tcode-number-strokes 0))
    (let ((bufname " *tcode: record*"))
      (save-excursion
	(tcode-set-work-buffer bufname tcode-record-file-name nil t)
	(goto-char (point-max))
	(insert
	 (format (concat "%s  ʸ��: %4d  ����: %3d(%d%%)  "
			 "�򤼽�: %3d(%d%%)  ��ǽ: %3d(%d%%)\n")
		 (let ((time (current-time-string)))
		   (if (not (string-match "^... \\(.*:.*\\):" time))
		       ""
		     (substring time (match-beginning 1) (match-end 1))))
		 tcode-input-chars
		 tcode-bushu-occurrence
		 (/ (* 100 tcode-bushu-occurrence) tcode-number-strokes)
		 tcode-mazegaki-occurrence
		 (/ (* 100 tcode-mazegaki-occurrence) tcode-number-strokes)
		 tcode-special-occurrence
		 (/ (* 100 tcode-special-occurrence) tcode-number-strokes)))
	(tcode-save-buffer bufname tcode-record-file-name t)))))

(provide 'tc)

;; ��������Ѵ��ν������
(tcode-bushu-init 0)

;;; tc.el ends here
