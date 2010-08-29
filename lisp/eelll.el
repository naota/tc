;;; eelll.el --- EELLL (ELisp implemented CATTT)

;; Copyright (C) 1991--2002  Kaoru Maeda, Yasushi Saito and Akira Kitajima

;; Author: Kaoru Maeda <maeda@src.ricoh.co.jp>
;;	Yasushi Saito <yasushi@is.s.u-tokyo.ac.jp>
;;	Akira Kitajima <kitajima@isc.osakac.ac.jp>
;;      YAGI Tatsuya <ynyaaa@ybb.ne.jp>
;; Maintainer: Akira Kitajima

;; $Id: eelll.el,v 1.29 2003/05/18 08:37:08 kitajima Exp $

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
(require 'tc-bushu)
(require 'tc-help)

;;  Version
(defun eelll-version ()
  "EELLL �ΥС�������ɽ�����롣"
  (interactive)
  (if (interactive-p)
      (message "EELLL version %s" (eelll-version))
    (substring "$Revision: 1.29 $" 11 -2)))

;;;
;;;  Variable
;;;

(defgroup eelll nil
  "EELLL (A T-Code trainer)."
  :group 'tcode)

(defcustom eelll-expert nil
  "T�����ɤξ��Ԥ��ɤ�����"
  :type 'boolean :group 'eelll)

(defcustom eelll-display-help-threshold 10
  "�إ�פ�ɽ��/��ɽ�����ڤ��ؤ��륨�顼Ψ�Τ�������(%)��"
  :type 'integer :group 'eelll)

(defvar eelll-text "EELLLTXT"
  "EELLL�������ƥ����ȥե�����")

(defvar eelll-move-cursor nil
  "*non-nil�ˤ���ȥ������ϤΤ��Ӥ˥��������ʤ�롣")

(defvar eelll-configuration-file-name (concat tcode-data-directory
					      "eelll-conf.el")
  "*EELLL������ե�����̾��
���Υե�����ϼ�ưŪ�˽񤭴�������Τǡ�
�桼���������Ƥ��ѹ����ƤϤ����ʤ���")

(defvar eelll-last-lesson nil
  "�Ǹ������������å����ֹ档")

(defvar eelll-last-lesson-alist nil
  "�ƥ����ȤΥե�����̾�ȺǸ������������å����ֹ��alist��")

(defconst eelll-buffer-name "*EELLL*"
  "EELLL����������Хåե�̾")

(defconst eelll-text-buffer " *eelll: text*"
  "EELLL�������ƥ����Ȥ�����Ƥ����Хåե�̾")

(defconst eelll-current-text-buffer " *eelll: current text*"
  "EELLL��������Υƥ����Ȥ�����Ƥ����Хåե�̾")

(defvar eelll-help-remove-interval 25
  "*EELLL��������ä��ޤǤλ���(��)")

(defconst eelll-help-buffer-name " *EELLL-Help*"
  "EELLL���Ǥ�����ɽ������Хåե�̾")

(defvar eelll-help-window-margin
  (if (and (>= emacs-major-version 21)
	   window-system)
      3
    1)
  "*EEELL���Ǥ�����ɽ������Ȥ��Υ�����ɥ��ι⤵�Υޡ�����")

(defvar eelll-use-image (or (and (featurep 'bitmap)
				 (or (tcode-mule-2-p)
				     (tcode-mule-3-p)
				     (tcode-mule-4-p))
				 window-system)
			    (and (tcode-mule-4-p)
				 (> emacs-major-version 20)
				 (display-images-p)))
  "* �ӥåȥޥåפ�Ȥä��إ�פ�ɽ�����뤫�ɤ�����
�ӥåȥޥåפ�ɽ���Ǥ��륦����ɥ������ƥ��ǤΤ߻��Ѳ�ǽ��")

(defvar eelll-by-text-dummy-char "��"
  "*ľ�����ϤǤ��ʤ�ʸ������������Ϥ�����ʸ����")

(defvar eelll-by-text-max-line 30)
(defvar eelll-by-text-fill-column 60)

(when (and (fboundp 'defface)
	   window-system)
    (defface eelll
      '((t (:font "-*-fixed-medium-r-normal-*-16-*-*-*-*-*-*-*"
		  :foreground "black"
		  :background "white")))
      "*�ӥåȥޥåפ�Ȥä� EELLL �ǤΥإ�פΥե�����"
      :group 'eelll))

(defvar eelll-previous-error-rate 0)

(defvar eelll-stroke-to-string-function
  (if (>= emacs-major-version 21)
      'tc-image-stroke-to-string
    'tc-bitmap-stroke-to-string))

(defvar eelll-original-window-configuration nil)
(defvar eelll-window-configuration nil)

(defvar eelll-total-time 0)
(defvar eelll-total-stroke 0)
(defvar eelll-total-error 0)
(defvar eelll-lesson-no nil)
(defvar eelll-first-hand nil)
(defvar eelll-second-hand nil)
(defvar eelll-upper-row nil)
(defvar eelll-lesson-chars nil)
(defvar eelll-text-line nil)
(defvar eelll-start-time nil)
(defvar eelll-strokes 0)
(defvar eelll-error-strokes 0)
(defvar eelll-lesson-string nil)
(defvar eelll-lesson-pattern-string nil)
(defvar eelll-key-table nil)

(defvar eelll-mode-hook nil
  "*EELLL��¹Ԥ���Ȥ��˼¹Ԥ����եå���")

(defvar eelll-exit-hook (if (featurep 'frame)
			    'eelll-remove-frame))
;;; for `eelll-other-frame'
(defvar eelll-frame-parameters 
  '((name . "EELLL")
    (font . "-*-fixed-medium-r-normal-*-16-*-*-*-*-*-*-*"))
  "*`eelll-other-frame'���Ѥ���ե졼��Υѥ�᡼����")
(defvar eelll-frame nil)

(defvar eelll-current-text nil)
(defvar eelll-random-mode nil)
(defvar eelll-random-max-line 4
  "*EELLL�Υ�����⡼�ɤ�1������򤵤��Կ��κ����͡�")

;;;
;;;  Text
;;;

(defun eelll-random-text (max-line)
  "���ߤΥХåե��ιԤ��������¤��ؤ��롣
����MAX-LINE�ޤǻĤ���"
  (let ((lines (count-lines (point-min) (point-max)))
	line-list)
    (goto-char (point-min))
    (while (and (> max-line 0)
		(> lines 0))
      (goto-line (1+ (random lines)))
      (if (eolp)
	  (progn
	    (unless (eobp)
	      (delete-char 1))
	    (setq lines (1- lines)))
	(let ((point-eol (save-excursion (end-of-line) (point))))
	  (setq line-list (cons (buffer-substring (point) point-eol) line-list)
		max-line (1- max-line)
		lines (1- lines))
	  (delete-region (point) (1+ point-eol)))))
    (erase-buffer)
    (insert (mapconcat 'identity line-list "\n") "\n")))

(defun eelll-prepare-text (num)
  "�����ƥ�����NUM���Ѱդ��롣NUM��nil�ʤ�м��Υ�å�����Ѱդ��롣"
  (save-excursion
    (tcode-set-work-buffer eelll-text-buffer eelll-text)
    (widen)
    (goto-char (point-min))
    (or (if num
	    (re-search-forward
	     (concat "\f\nLesson \\(" (int-to-string num) "\\):") nil t)
	  (and (or (null eelll-last-lesson)
		   (re-search-forward (concat "\f\nLesson \\("
					      (int-to-string eelll-last-lesson)
					      "\\):")
				      nil t))
	       (re-search-forward "\f\nLesson \\([0-9]+\\):" nil t)))
	(error "�����ƥ�����%s�Ϥ���ޤ���" (if num (int-to-string num) "")))
    (setq eelll-lesson-string (buffer-substring (match-beginning 1)
						(match-end 1))
	  eelll-lesson-no (string-to-int eelll-lesson-string))
    (setq eelll-first-hand (looking-at "[Rr]"))
    (setq eelll-second-hand (looking-at ".[Rr]"))
    (setq eelll-upper-row (looking-at "..!"))
    (forward-line 1)
    (if (looking-at "^Lesson-chars: ")
	(let ((eol (save-excursion (end-of-line 1) (point))))
	  (skip-chars-forward "^ " eol)
	  (setq eelll-lesson-chars (buffer-substring (1+ (point)) eol))
	  (forward-line 1)))
    (let ((text (buffer-substring (point)
				  (save-excursion
				    (forward-page 1)
				    (1- (point))))))
      (set-buffer (get-buffer-create eelll-current-text-buffer))
      (erase-buffer)
      (insert text)
      (if eelll-random-mode
	  (eelll-random-text eelll-random-max-line))
      (goto-char (point-min))))
  (setq eelll-last-lesson eelll-lesson-no))

(defun eelll-lesson-line ()
  "�����ƥ����Ȥμ��ιԤ�ȤäƤ��롣�����ʤ�nil���֤���
eelll-text-line:	�������᡼��"
  (save-excursion
    (set-buffer eelll-current-text-buffer)
    (skip-chars-forward " \t\n\f" (point-max))
    (and (not (eobp))
	 (let ((p (point)))
	   (forward-line 1)
	   (setq eelll-text-line (buffer-substring p (1- (point))))
	   eelll-text-line))))

(defun eelll-tcode-encode (ch)
  (if (eq ch (tcode-string-to-char "��"))
      (if (equal tcode-default-input-method "japanese-T-Code")
	  '(26 23)
	'(20 28 20))
    (tcode-encode ch)))

(defun eelll-stroke-for-char (ch)
  "CH(���Ѱ�ʸ��)���Ǥ������֤���T�����ɤ����ϤǤ��ʤ����nil���֤���"
  (let* ((char (tcode-string-to-char ch))
	 (strokes (eelll-tcode-encode char)))
    (if (and strokes
	     (= (length strokes) 2))
	strokes
      nil)))

(defun eelll-inputs-for-char (ch)
  "CH (����1ʸ��)���Ǥ���(ʸ����Υꥹ��)���֤���
T�����ɤ����ϤǤ��ʤ����nil���֤���"
  (let ((strokes (eelll-tcode-encode (tcode-string-to-char ch))))
    (cond (strokes
	   (mapcar (lambda (key)
		     (aref eelll-key-table key))
		   strokes))
	  (tcode-another-table
	   (let ((i (1- (length tcode-another-table))))
	     (catch 'found
	       (while (>= i 0)
		 (let ((c (aref tcode-another-table i)))
		   (if (string= ch (if (and c (symbolp c))
				       (eval c)
				     c))
		       (throw 'found (list (aref eelll-key-table i) ? ))))
		 (setq i (1- i))))))
	  ((string= ch " ")
	   (list ? )))))

;;;
;;;  Stroke chart
;;;
(defun eelll-draw-chart ()
  "�����ƥ����Ȥ������оݤȤʤ�ʸ���Υ��ȥ���ɽ���롣"
  (save-excursion
    (set-buffer (get-buffer-create eelll-help-buffer-name))
    (widen)
    (erase-buffer)
    (goto-char (point-min))
    (let ((i 0)
	  j k
	  (c (if eelll-second-hand 1 4)))
      (while (< i 4)
	(setq i (1+ i) j 0)
	(while (< j 4)
	  (insert "    ")
	  (setq j (1+ j) k 0)
	  (while (< k 5)
	    (setq k (1+ k))
	    (insert "����������  ")
	    (if (= k c) (insert "  ")))
	  (delete-horizontal-space)
	  (insert "\n"))
	(if (< i 4) (insert "\n"))))
    (mapcar
     (lambda (c)
       (let ((stroke (eelll-stroke-for-char (char-to-string c))))
	 (when (and stroke
		    (= (length stroke) 2))
	   (let* ((second (car (cdr stroke)))
		  (fc (% (car stroke) 5))
		  (fr (/ (car stroke) 10))
		  (sc (% second 5))
		  (sr (/ second 10)))
	     (goto-line (+ (* sr 5) fr 1))
	     (move-to-column (+ 4 (* 12 sc) (* 2 fc)
				(if (> sc (if eelll-second-hand 0 3)) 2 0)))
	     (tcode-delete-char 1)
	     (insert (char-to-string c))))))
     (string-to-list eelll-lesson-chars))
    (goto-char (point-min))
    (forward-line 1)
    (delete-region (point-min) (point))
    (unless eelll-upper-row
      (forward-line 5)
      (delete-region (point-min) (point))
      (forward-line 4)
      (delete-region (point)
		     (save-excursion (forward-line 1)
				     (point)))
      (forward-line 4)
      (delete-region (point)
		     (save-excursion (forward-line 1)
				     (point))))
    (setq eelll-lesson-pattern-string (concat (if eelll-first-hand "R" "L")
					      "->"
					      (if eelll-second-hand "R" "L")))
    (set-buffer-modified-p nil)))

;;;
;;;  String Matching
;;;
(defun eelll-subsequence (seq n)
  ;; return first N elements of SEQ
  (let ((seq (copy-sequence seq)))
    (if (> (length seq) n)
	(setcdr (nthcdr (1- n) seq) nil))
    (if (<= n 0)
	nil
      seq)))

(defun eelll-match (string quest)
  "QUEST�򤪼��ܤȤ������Ϥ��줿ʸ����STRING�κ����򤹤롣
2���ǤΥꥹ��(RESULT ERROR)���֤���"
  (let* ( ;; template �� ((ʸ�� . ���ȥ�����) . ���ȥ���) �Υꥹ��
	 (template (delq nil
			 (mapcar
			  (lambda (c)
			    (let ((stroke (eelll-inputs-for-char
					   (char-to-string c))))
			      (and stroke
				   (cons (cons c (length stroke))
					 stroke))))
			  (string-to-list quest))))
	 ;; template-length �� template ��Ĺ��
	 (template-length (length template))
	;; vtemplate �� (ʸ�� . ���ȥ�����) �Υ٥�����
	 (vtemplate (vconcat (mapcar 'car template)))
	 ;; key-list �����Ϥ��줿ʸ���Υꥹ��
	 (key-list (string-to-list string))
	 (key-length (length string))
	 (link (list '(0 . 0)))
	 (last (1- (length string)))
	 (j 0))
    (while template
      (let ((correct (mapconcat 'char-to-string (cdr (car template)) nil))
	    (correct-length (cdr (car (car template))))
	    (inputted key-list))
	(while inputted
	  (when (string= correct
			 (mapconcat 'char-to-string
				    (eelll-subsequence inputted correct-length)
				    nil))
	    (let* ((l link)
		   (next-input (nthcdr correct-length inputted))
		   (next-i (- key-length (length next-input)))
		   (next-template (cdr template))
		   (next-j (- template-length (length next-template)))
		   pair)
	      (catch 'finish
		(while l
		  (if (and (>= (- key-length (length inputted))
			       (car (car l)))
			   (>= (- template-length (length template))
			       (cdr (car l))))
		      (throw 'finish nil)
		    (setq pair (car l)
			  l (cdr l)))))
	      (if pair
		  (let ((dif (+ (- (car pair) next-i) (- (cdr pair) next-j))))
		    (when (or (> dif 0)
			      (and (= dif 0)
				   (> next-i (car pair))))
		      (setcar pair next-i)
		      (setcdr pair next-j)))
		(setq link (cons (cons next-i next-j) link)))))
	  (setq inputted (cdr inputted))))
      (setq template (cdr template)))
    (let* ((res (vconcat (string-to-list string)))
	   (pi (length string))
	   (pj template-length)
	   (err 0)
	   (l link)
	   (i (car (car l))))
      (while (> i 0)
	(let* ((j (cdr (car l)))
	       (correct (aref vtemplate (1- j))))
	  (aset res (- i 1) (car correct))
 	  (let ((n (1- (cdr correct))))
 	    (while (>= n 1)
 	      (aset res (- i 1 n) nil)
 	      (setq n (1- n))))
	  (setq err (+ err (max (- pi i) (- pj j)) (- (cdr correct))))
	  (setq pi i
		pj j
		l (cdr l)
		i (car (car l)))))
      (list (mapconcat (lambda (c) (if c (char-to-string c)))
		       res nil)
	    (+ err (max pi pj))))))

;;;
;;;  Mode setup
;;;

(defvar eelll-mode-map nil
  "EELLL �⡼�ɤǻȤ������ޥå�")
(if eelll-mode-map
    ()
  (setq eelll-mode-map (make-keymap))
  (substitute-key-definition nil 'eelll-undefined eelll-mode-map)
  (define-key eelll-mode-map "\177" 'eelll-delete-char)
  (define-key eelll-mode-map "\e" 'ESC-prefix)
  (define-key eelll-mode-map "\C-c" 'mode-specific-command-prefix)
  (define-key eelll-mode-map "\C-j" 'eelll-return)
  (define-key eelll-mode-map "\C-m" 'eelll-return)
  (define-key eelll-mode-map "\C-g" 'eelll-confirm-quit)
  (define-key eelll-mode-map "\C-l" 'eelll-redisplay)
  (define-key eelll-mode-map "\C-u" 'universal-argument)
  (define-key eelll-mode-map "\C-x" 'Control-X-prefix)
  (define-key eelll-mode-map "\C-]" 'abort-recursive-edit)
  (define-key eelll-mode-map "?" 'eelll-help)
  (define-key eelll-mode-map " " 'eelll-key))

(defun eelll-mode ()
  "EELLL �� Emacs Lisp �Ǽ¸����줿T�����������ץ����Ǥ���
���̤�ɽ�����줿ʸ����򤽤Τޤ����Ϥ��Ƥ���������
1�Ԥ����Ϥ�����ä���꥿���󥭡����ǤäƤ���������

���̤ξ�Ⱦʬ�ˤϺ��Υ�å���ǽ���ʸ���Υ��ȥ���ɽ��ɽ������Ƥ��ޤ���

EELLL ��ǤϤۤȤ�ɤΥ��ޥ�ɤ��ػߤ���Ƥ��ޤ���
�ޤ� \\[switch-to-buffer] ��¾�ΥХåե��˰ܤäƤ���\
���ޥ�ɤ�¹Ԥ��Ƥ���������
�ʤ���\\[eelll-confirm-quit] �� EELLL �����Ǥ��ޤ���"
  (use-local-map eelll-mode-map)
  (setq major-mode 'eelll-mode)
  (setq mode-name "EELLL")
  (setq mode-line-format '("-----EELLL"
			   (eelll-lesson-string
			    (": lesson " eelll-lesson-string))
			   (eelll-random-mode "(random)")
			   "%-"))
  (run-hooks 'eelll-mode-hook))

(defun eelll-help ()
  (interactive)
  (if eelll-use-image
      (progn
	(forward-line -2)
	(save-restriction
	  (narrow-to-region (point) (point))
	  (eelll-insert-bitmap-help eelll-text-line))
	(insert "\n"))
    (setq eelll-upper-row t)
    (if (null eelll-second-hand)
	(setq eelll-second-hand t
	      eelll-first-hand eelll-first-hand)
      (setq eelll-second-hand nil
	    eelll-first-hand (not eelll-first-hand)))
    (let ((eelll-lesson-chars (eelll-select-chars eelll-text-line))
	  (cur (selected-window)))
      (eelll-draw-chart)
      (delete-other-windows)
      (split-window-vertically
       (save-excursion
	 (set-buffer eelll-help-buffer-name)
	 (setq mode-line-format
	       '("-----EELLL Help"
		 (eelll-lesson-string
		  (": lesson " eelll-lesson-string))
		 (eelll-lesson-pattern-string
		  ("  (" eelll-lesson-pattern-string ")"))
		 "%-"))
	 (+ (count-lines (point-min) (point-max)) 
	    eelll-help-window-margin)))
      (switch-to-buffer eelll-help-buffer-name)
      (other-window 1))
    (setq eelll-window-configuration (current-window-configuration)))
  (eelll-redisplay))

(defun eelll-select-chars (text)
  (let ((ret nil))
    (mapcar (lambda (c)
	      (let* ((stroke (eelll-stroke-for-char (char-to-string c)))
		     (1st (car stroke))
		     (2nd (car (cdr stroke)))
		     (lks '(0 1 2 3 4
			      10 11 12 13 14
			      20 21 22 23 24
			      30 31 32 33 34))
		     (rks '(5 6 7 8 9
			      15 16 17 18 19
			      25 26 27 28 29
			      35 36 37 38 39)))
		(and (memq 1st (if eelll-first-hand rks lks))
		     (memq 2nd (if eelll-second-hand rks lks))
		     (setq ret (cons c ret)))))
	    (string-to-list text))
    (if ret
	(mapconcat 'char-to-string ret nil)
      "")))

(defun eelll-undefined ()
  (interactive)
  (message (substitute-command-keys
	    "\\[switch-to-buffer] SOME-BUFFER first.")))

(defun eelll-delete-char ()
  (interactive)
  (message "�ְ㤤�򵤤ˤ����ɤ�ɤ����Ϥ��Ƥ���������"))

;;;
;;; ���Ѥ������䴰�դ���completing-read
;;;
;;; "?"�ǰ���������ޤ���

(defun eelll-completing-read ()
  "�����䴰�դ��ǡ������ƥ������ֹ��ߥ˥Хåե��������Ϥ��롣
Emacs������completion�μ����������Τ��ᡢ��?�פ�
���Ϥ������ˤ��������ϸ����ʤ���"
  (unless (featurep 'tcode-ready)
    (tcode-use-package (or tcode-current-package
			   tcode-default-input-method))
    (tcode-inactivate))
  (load eelll-configuration-file-name t)
  (if (not (equal eelll-current-text eelll-text))
      (let ((config (assoc eelll-text eelll-last-lesson-alist)))
	(setq eelll-last-lesson (if config
				    (cdr config)
				  nil))))
  (let (lesson-alist key val orig-minibuffer-completion-help)
    (save-excursion
      (if (and (not (equal eelll-current-text eelll-text))
	       (get-buffer eelll-text-buffer))
	  (kill-buffer eelll-text-buffer))
      (tcode-set-work-buffer eelll-text-buffer eelll-text)
      (widen)
      (goto-char (point-min))
      (while (re-search-forward "^Lesson \\([0-9]+\\):[rRlL]+" nil t)
	(setq key (buffer-substring (match-beginning 1) (match-end 1)))
	(forward-line 2)
	(setq val (buffer-substring (point)
				    (save-excursion (end-of-line) (point))))
	(setq lesson-alist (cons (cons key val) lesson-alist))))
    (setq orig-minibuffer-completion-help
	  (symbol-function 'minibuffer-completion-help))
    (unwind-protect
	(progn
	  (fset 'minibuffer-completion-help
		(symbol-function 'eelll-minibuffer-completion-help))
	  (let* ((hist (mapcar 'car lesson-alist))
		 (pos (if eelll-last-lesson
			  (- (length hist)
			     (length (member (format "%d" eelll-last-lesson)
					     hist))
			     -1)))
		 (str (cond ((tcode-nemacs-p)
			     (completing-read 
			      "�����ƥ�����[`?'�ǰ���] "
			      lesson-alist nil t
			      (if eelll-last-lesson
				  (format "%d" eelll-last-lesson)
				"")))
			    ((tcode-xemacs-p)
			     (completing-read
			      "�����ƥ�����[`?'�ǰ���] "
			      lesson-alist nil t
			      (if eelll-last-lesson
				  (format "%d" eelll-last-lesson)
				"")
			      (if pos (cons 'hist (list pos)) (list 'hist))))
			    (t
			     (completing-read 
			      "�����ƥ�����[`?'�ǰ���] "
			      lesson-alist nil t
			      (if eelll-last-lesson
				  (format "%d" eelll-last-lesson)
				"")
			      (if pos (cons 'hist pos) 'hist))))))
	    (if (string= str "")
		nil
	      (list (string-to-int str)))))
      (fset 'minibuffer-completion-help orig-minibuffer-completion-help))))

(defun eelll-minibuffer-completion-help ()
  "EELLL�Ѥ��䴰�����ɽ�����롣
`minibuffer-completion-help'���֤������롣"
  (interactive)
  (with-output-to-temp-buffer "*Completions*"
    (eelll-display-completion-list
     (all-completions (save-excursion
			(buffer-substring (progn (beginning-of-line) (point))
					  (progn (end-of-line) (point))))
			minibuffer-completion-table))))

(defun eelll-display-completion-list (x)
  "�����ƥ����Ȱ�����ɽ�����롣
`display-completion-list'���֤������롣"
  (princ "    ---- �����ƥ����Ȱ��� ----\n")
  (setq x (sort x (lambda (x y)
		    (< (string-to-int x) (string-to-int y)))))
  (while x
    (princ (car x))
    (princ ":")
    (princ (cdr (assoc (car x) lesson-alist)))
    (princ "\n")
    (setq x (cdr x))))

;;;; �ӥåȥޥå�ɽ��

(defun eelll-insert-with-face (str)
  (let ((beg (point)))
    (insert str)
    (when (and (fboundp 'put-text-property)
	       window-system)
      (put-text-property beg (point) 'face 'eelll))))

(defun eelll-put-help-char (c)
  (let* ((ch (char-to-string c))
	 (stroke (or (eelll-tcode-encode c)
		     (let ((another (member ch
					    (mapcar 'eval 
						    (string-to-list
						     tcode-another-table)))))
		       (if another
			   (list (- (length tcode-another-table)
				    (length another)))))))
	 (s (if stroke
		(funcall eelll-stroke-to-string-function stroke)))
	 (ex-col (/ (1- (length stroke)) 2)))
    (if (<= (* (+ help-count ex-col) 7)
	    (* (frame-width) 0.60))
	(setq help-count (1+ help-count))
      (insert "\n")
      (end-of-line)
      (insert "\n")
      (setq help-count 1))
    (insert "|")
    (eelll-insert-with-face (or s "-- --"))
    (save-excursion
      (forward-line)
      (end-of-line)
      (insert "|")
      (eelll-insert-with-face (format "  %2s " ch))
      (let ((l ex-col))
	(while (> l 0)
	  (eelll-insert-with-face "      ")
	  (setq l (1- l))
	  (setq help-count (1+ help-count)))))))

(defun eelll-insert-bitmap-help (string)
  (let ((chars (string-to-list string))
	(help-count 0)
	(tcode-stroke-to-string-separator "-")
	(tcode-stroke-to-string-opener "")
	(tcode-stroke-to-string-closer "")
	(beg (progn 
	       (insert "\n")
	       (point))))
    (mapcar 'eelll-put-help-char chars)
    (insert "\n")
    (forward-line)))

;;;
;;;  Main
;;;

(defun eelll-init ()
  (tcode-use-package (or tcode-current-package
			 tcode-default-input-method))
  (tcode-inactivate)
  (unless eelll-key-table
    ;; initialize `eelll-key-table' and `eelll-keymap-table'
    (let ((n (1- (length tcode-key-translation-rule-table))))
      (setq eelll-key-table (make-vector 80 nil))
      (while (>= n 0)
	(let ((key (aref tcode-key-translation-rule-table n)))
	  (when (>= key 0)
	    (aset eelll-key-table key (+ n ? ))
	    (define-key eelll-mode-map (char-to-string (+ n ? )) 'eelll-key))
	  (setq n (1- n))))))
  (setq eelll-original-window-configuration (current-window-configuration))
  (setq eelll-previous-error-rate (if eelll-expert 0 100))
  (setq eelll-current-text eelll-text)
  (if (not (stringp lesson))
      (eelll-prepare-text lesson)
    (setq eelll-current-text nil
	  eelll-lesson-string "Temporary"
	  eelll-lesson-no 0
	  eelll-lesson-chars "")
    (save-excursion
      (set-buffer (get-buffer-create eelll-current-text-buffer))
      (erase-buffer)
      (insert lesson)
      (if eelll-random-mode
	  (eelll-random-text eelll-random-max-line))
      (eelll-normalize-text-buffer)))
  (eelll-setup-lesson)
  (message (substitute-command-keys "\\[eelll-help] �ǥإ��")))

(defun eelll-normalize-text-buffer ()
  ;; delete white space
  (goto-char (point-min))
  (while (re-search-forward "[ \t]+" nil t)
    (replace-match ""))
  ;; delete null line
  (goto-char (point-min))
  (while (re-search-forward "\n\n+" nil t)
    (replace-match "\n"))
  (goto-char (point-min))
  (let ((line 0) c stroke cell (str "��")
	(bol-kinsoku
	 (string-to-list ")!?������������������������������\
����������������������������\
�������¡áġšǡɡˡ͡ϡѡӡաס١ۡ����\
�����������ä������������å�������"))
	(eol-kinsoku (string-to-list "(�ơȡʡ̡ΡСҡԡ֡ءڡ�������")))
    (while (and (< line eelll-by-text-max-line)
		(not (eobp)))
      (setq c (tcode-following-char)
	    stroke (tcode-encode c))
      (when (and (bolp) (not (bobp))
		 (memq c bol-kinsoku))
	(delete-char -1))
      (if (or stroke (eolp))
	  (if (eolp) (delete-char 1) (forward-char))
	(delete-char 1)
	(setq cell (tcode-decompose-char (char-to-string c) t))
	(if (not (and (consp cell)
		      (stringp (car cell))
		      (stringp (cdr cell))))
	    (insert eelll-by-text-dummy-char)
	  (if tcode-use-postfix-bushu-as-default
	      (insert (car cell) (cdr cell) str)
	    (insert str (car cell) (cdr cell)))))
      (when (and (< eelll-by-text-fill-column (current-column))
		 (not (memq c eol-kinsoku)))
	(insert ?\n)
	(setq line (1+ line)))))
  (or (bolp) (insert ?\n))
  (delete-region (point) (point-max))
  (goto-char (point-min)))

;;;###autoload
(defun eelll (&optional lesson)
  "EELLL ��Ϥ�롣�ܤ����� `eelll-mode' �β���򸫤衣"
  (interactive (eelll-completing-read))
  (setq eelll-random-mode nil)
  (eelll-init))

;;;###autoload
(defun eelll-random (&optional lesson)
  "������⡼�ɤ� EELLL ��Ϥ�롣
���ꤷ���ƥ����Ȥ��椫��������
����(`eelll-random-max-line'�ǻ��ꤷ���Կ�)���򤵤�롣"
  (interactive (eelll-completing-read))
  (setq eelll-random-mode t)
  (eelll-init))

;;;###autoload
(defun eelll-region (beg end)
  "�꡼�������Υƥ����Ȥ� EELLL ��Ϥ�롣"
  (interactive "r")
  (unless (featurep 'tcode-ready)
    (tcode-use-package (or tcode-current-package
			   tcode-default-input-method))
    (tcode-inactivate))
  (load eelll-configuration-file-name t)
  (let ((lesson (buffer-substring beg end)))
    (setq eelll-random-mode nil)
    (eelll-init)))

(defun eelll-setup-lesson ()
  (eelll-draw-chart)
  (switch-to-buffer eelll-buffer-name)
  (buffer-disable-undo)
  (eelll-mode)
  (widen) 
  (erase-buffer)
  (delete-other-windows)
  (if eelll-use-image
      (if (>= emacs-major-version 21)
	  (require 'tc-image)
	(require 'tc-bitmap))
    (when (>= eelll-previous-error-rate eelll-display-help-threshold)
      (split-window-vertically
       (save-excursion
	 (set-buffer eelll-help-buffer-name)
	 (setq mode-line-format
	       '("-----EELLL Help"
		 (eelll-lesson-string
		  (": lesson " eelll-lesson-string))
		 (eelll-lesson-pattern-string
		  ("  (" eelll-lesson-pattern-string ")"))
		 "%-"))
	 (+ (count-lines (point-min) (point-max)) 
	    eelll-help-window-margin)))
      (switch-to-buffer eelll-help-buffer-name)
      (goto-char (point-min))
      (other-window 1)))
  (setq eelll-window-configuration (current-window-configuration))
  (setq eelll-start-time nil
	eelll-error-strokes 0
	eelll-strokes 0)
  (insert "�꥿���󥭡����ǤƤлϤޤ�ޤ��� "))

(defun eelll-key ()
  (interactive)
  (save-excursion
    (set-buffer " *eelll: strokes*")
    (insert (char-to-string last-command-char)))
  (if eelll-move-cursor
      (insert " ")))

(defun eelll-return ()
  (interactive)
  (if eelll-start-time
      (progn
	(delete-region (point) (progn (beginning-of-line 1) (point)))
	(let* ((str (save-excursion
		      (set-buffer " *eelll: strokes*")
		      (buffer-string)))
	       (res (eelll-match str eelll-text-line))
	       (err (car (cdr res))))
	  (insert (car res))
	  (let ((wrong-chars (tcode-uniq 
			      (tcode-subtract-set
			       (string-to-list eelll-text-line)
			       (string-to-list (car res))))))
	    (when wrong-chars
	      (insert "\n\n[�ְ㤨����]")
	      (if eelll-use-image
		  (eelll-insert-bitmap-help
		   (mapconcat 'char-to-string wrong-chars nil))
		(insert "=> " (mapconcat 'char-to-string wrong-chars nil)))))
	  (setq eelll-strokes (+ eelll-strokes (length str))
		eelll-error-strokes (+ eelll-error-strokes err))))
    (setq eelll-start-time (eelll-current-time)))
  (if (null (eelll-lesson-line))
      (eelll-end-lesson)
    (save-excursion
      (set-buffer (get-buffer-create " *eelll: strokes*"))
      (widen) (erase-buffer))
    (when eelll-use-image
      (insert "\n")
      (if (>= eelll-previous-error-rate eelll-display-help-threshold)
	  (eelll-insert-bitmap-help eelll-text-line)))
    (insert "\n\n" eelll-text-line "\n")
    (eelll-redisplay)))

(defun eelll-redisplay ()
  "EELLL �β��̤�ɽ����ľ����"
  (interactive)
  (set-window-configuration eelll-window-configuration)
  (goto-char (point-max))
  (recenter -1))

(defun eelll-current-time ()
  (let ((str (current-time-string)))
    (string-match "\\([0-9][0-9]\\):\\([0-9][0-9]\\):\\([0-9][0-9]\\)" str)
    (+ (* 3600 (string-to-int (substring str
					 (match-beginning 1)
					 (match-end 1))))
       (* 60 (string-to-int (substring str (match-beginning 2) (match-end 2))))
       (string-to-int (substring str (match-beginning 3) (match-end 3))))))

(defun eelll-percentage (num den)
  (let ((res%  (min 9999 (/ num (max 1 den)))))
    (format "%d.%d" (/ res% 10) (% res% 10))))

(defun eelll-stroke-per-min (stroke sec)
  "1ʬ������Υ��ȥ�������ɽ����ʸ������֤���"
  (let ((stroke-per-min (/ (* (float stroke) 60) sec)))
    (format (if (< stroke-per-min 10) "%.1f" "%.f") stroke-per-min)))

(defun eelll-time-per-stroke (time stroke)
  "1���ȥ���������λ��֤�ɽ����ʸ������֤���"
      (if (>= time stroke)
	  (format "%.1f" (/ (float time) stroke))
	(format "%.f �ߥ�" (/ (* (float time) 1000) stroke))))

(unless (fboundp 'float)
  ;; float �ΰ����ʤ� Emacs (NEmacs) �ξ��
  ;; �����С��ե����ʤ��褦�˷�򤺤餹��

  (defun eelll-stroke-per-min (stroke sec)
    "1ʬ������Υ��ȥ�������ɽ����ʸ������֤���"
    (while (>= stroke 10000)
      (setq sec (/ sec 2)
	    stroke (/ stroke 2)))
    (setq stroke (* stroke 600))
    (let ((10stroke-per-min (/ stroke sec)))
      (if (< 10stroke-per-min 100)
	  (format "%d.%1d" (/ 10stroke-per-min 10) (% 10stroke-per-min 10))
	(format "%d" (/ 10stroke-per-min 10)))))

  (defun eelll-time-per-stroke (time stroke)
    "1���ȥ���������λ��֤�ɽ����ʸ������֤���"
    (if (>= time stroke)
	(let ((10time-per-stroke (/ (* time 10) stroke)))
	  (format "%d.%d " (/ 10time-per-stroke 10) (% 10time-per-stroke 10)))
      (while (>= time 8000)
	(setq time (/ time 2)
	      stroke (/ stroke 2)))
      (let ((1000time-per-stroke (/ (* time 1000) stroke)))
	(format "%d �ߥ�" 1000time-per-stroke)))))

(defun eelll-display-score (time stroke err)
  (if (> stroke 0)
      (let ((cnt (- stroke err)))
	(set-buffer eelll-buffer-name)
	(insert "\n\n(���Ǹ�����)���Ǹ� "
		(eelll-time-per-stroke time stroke) "��  "
		"��ʬ " (eelll-stroke-per-min stroke time) " �Ǹ�\n")
	(if (> cnt 0)
	    (insert "(���Ǹ�����)���Ǹ� "
		    (eelll-time-per-stroke time cnt) "��  "
		    "��ʬ " (eelll-stroke-per-min cnt time) " �Ǹ�\n"))
	(setq eelll-previous-error-rate (if (> stroke 0)
					    (/ (* 100 err) stroke)
					  100))
	(insert "              ���顼�졼�� "
		(eelll-percentage (* 1000 err) stroke)
		"%\n\n"))))

(defun eelll-end-lesson ()
  (let ((time (% (- (eelll-current-time) eelll-start-time -86399) 86400)))
    (setq eelll-total-time (+ eelll-total-time time)
	  eelll-total-stroke (+ eelll-total-stroke eelll-strokes)
	  eelll-total-error (+ eelll-total-error eelll-error-strokes))
    (eelll-display-score time eelll-strokes eelll-error-strokes)
    (delete-other-windows)
    (recenter -1)
    (if (equal eelll-lesson-string "Temporary")
	(if (y-or-n-p "�⤦���٥ȥ饤���ޤ���? ")
	    (progn
	      (save-excursion
		(set-buffer eelll-current-text-buffer)
		(goto-char (point-min)))
	      (eelll-setup-lesson))
	  (eelll-end))
    (if (y-or-n-p "�⤦���٥ȥ饤���ޤ���? ")
	(progn
	  (eelll-prepare-text eelll-last-lesson)
	  (eelll-setup-lesson))
      (if (y-or-n-p "���Υ�å���˿ʤߤޤ���? ")
	  (progn (eelll-prepare-text nil)
		 (setq eelll-previous-error-rate (if eelll-expert 0 100))
		 (eelll-setup-lesson))
	(eelll-end))))
    )
  (message ""))

(defun eelll-save-configuration ()
  "`eelll-last-lesson' ���ͤ�����ե��������¸���롣
����ե�����̾�� `eelll-configuration-file-name' �ǻ��ꤹ�롣"
  ;; `eelll-last-lesson-alist'�򹹿����롣
  (let ((config (assoc eelll-text eelll-last-lesson-alist)))
    (if config
	(setcdr config eelll-last-lesson)
      (setq eelll-last-lesson-alist (cons (cons eelll-text eelll-last-lesson)
					  eelll-last-lesson-alist))))
  ;; `eelll-last-lesson-alist'�����Ƥ�`eelll-configuration-file-name'��
  ;; ��¸���롣
  (and tcode-data-directory
       eelll-configuration-file-name
       (file-writable-p eelll-configuration-file-name)
       (save-excursion
	 (kill-buffer
	  (prog1
	      (set-buffer (get-buffer-create " *eelll: temp*"))
	    (insert ";; ���Υե�����ϼ�ưŪ�˹�������ޤ���"
		    "�Խ����ʤ��Ǥ���������\n")
	    (insert (format "(setq eelll-last-lesson-alist '%s)\n" 
			    (prin1-to-string eelll-last-lesson-alist)))
	    (let ((backup-inhibited t))
	      (write-file eelll-configuration-file-name)))))))

(defun eelll-confirm-quit ()
  "EELLL ��λ���뤫�ɤ�����ǧ������λ���롣"
  (interactive)
  (and (y-or-n-p "EELLL ��λ���ޤ���? ")
       (eelll-end t)))

(defun eelll-end (&optional abort)
  (if abort
      ()
    (set-buffer eelll-buffer-name)
    (widen)
    (erase-buffer)
    (insert "�������\n\n")
    (eelll-display-score eelll-total-time eelll-total-stroke eelll-total-error)
    (insert "\n    �����Ǹ��� " (int-to-string eelll-total-stroke) " �Ǹ�. ")
    (insert "���׻��� " (int-to-string eelll-total-time) " ��.\n")
    (recenter -1)
    (if eelll-current-text
	(eelll-save-configuration))
    (message "���Ĥ��줵�ޤǤ������ɤ줫�����򤪤��Ƥ���������")
    (condition-case ()
	(read-char)
      (error)))
  (kill-buffer eelll-help-buffer-name)
  (set-window-configuration eelll-original-window-configuration)
  (run-hooks 'eelll-exit-hook)
  (message ""))

(defun eelll-remove-frame ()
  "EELLL�ѤΥե졼�ब����к�����롣"
  (interactive)
  (if (and eelll-frame
	   (frame-live-p eelll-frame))
      (make-frame-invisible eelll-frame)))

(defun eelll-other-frame ()
  "�ե졼����������EELLL��Ϥ�롣"
  (interactive)
  (if (or (null eelll-frame)
	  (not (frame-live-p eelll-frame)))
      (setq eelll-frame (make-frame eelll-frame-parameters)))
  (unless (frame-visible-p eelll-frame)
    (make-frame-visible eelll-frame))
  (select-frame eelll-frame)
  (raise-frame eelll-frame)
  (call-interactively 'eelll))

;;;
;;; �����ƥ����ȥǡ���������
;;;

(defun tcode-key-address-right-p (address)
  (let ((location (tcode-get-key-location address)))
    (>= (cdr location) 6)))

(defun tcode-key-address-left-p (address)
  (let ((location (tcode-get-key-location address)))
    (< (cdr location) 6)))

(defun eelll-compile-text (recompile-all)
  "�����ƥ����Ȥ�Lesson-chars: �Ԥ��롣
RECOMPILE-ALL �� non-nil �ξ��ˤϡ�
��¸��Lesson-chars:�򤹤٤ƺ�����Ƥ��鿷���˺��ľ����"
  (interactive "P")
  (find-file (tcode-path-for-write eelll-text))
  (let ((reached-eob nil) (eelll-buffer (current-buffer)))
    (save-restriction
      (widen)
      (goto-char (point-min))
      (save-excursion
	(set-buffer (get-buffer-create " *eelll: lessons*"))
	(delete-region (point-min) (point-max)))
      (while (and (not reached-eob) (not (eobp)))
	(let ((point (point)))
	  (narrow-to-page 1)
	  (if (= point (point)) (setq reached-eob t)))
	(goto-char (point-min))
	(skip-chars-forward "^0-9")
	(message (buffer-substring (point)
				   (save-excursion (end-of-line 1) (point))))
	(skip-chars-forward "^:" (save-excursion (end-of-line 1) (point)))
	(setq eelll-first-hand (looking-at ":[Rr]"))
	(setq eelll-second-hand (looking-at ":.[Rr]"))
	(forward-line 1)
	(if (and recompile-all (looking-at "^Lesson-chars: "))
	    (delete-region (point) (save-excursion (forward-line) (point))))
	(if (not (looking-at "^Lesson-chars: "))
	    (let ((where (point))
		  (chars nil)
		  (upper nil)
		  c last)
	      (while (not (eobp))
		(skip-chars-forward " \t\n\f" (point-max))
		(while (not (eolp))
		  (setq chars (cons (buffer-substring (point)
						      (progn (forward-char 1)
							     (point)))
				    chars))))
	      (setq chars (sort chars 'string<))
	      (goto-char where)
	      (if chars (insert "Lesson-chars: "))
	      (while chars
		(catch 'next
		  (setq c (car chars) chars (cdr chars))
		  (if (string= c last) (throw 'next nil))
		  (setq last c)
		  (setq c (eelll-stroke-for-char c))
		  (if (and c
			   (eq eelll-first-hand
			       (tcode-key-address-right-p (car c)))
			   (eq eelll-second-hand
			       (tcode-key-address-right-p (car (cdr c)))))
		      (progn
			(if (or (< (car c) 10) (< (car (cdr c)) 10))
			    (setq upper t))
			(insert last)))))
	      (insert "\n")
	      (if upper
		  (progn
		    (goto-char (point-min))
		    (end-of-line 1)
		    (or (= (preceding-char) ?!)
			(insert "!"))))
	      (sit-for 0)
	      (if (input-pending-p) (ding)))))
      (message "Compilation end.")
      (let ((buffer (get-buffer eelll-text-buffer)))
	(and buffer
	     (kill-buffer buffer))))))

(provide 'eelll)

;;; eelll.el ends here
