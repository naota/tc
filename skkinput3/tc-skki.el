;;; tc-skki.el  --- setup for tc2 on skkinput3

;; Copyright (C) 2002 KITAJIMA Akira.

;; Author: KITAJIMA Akira <kitajima@isc.osakac.ac.jp>
;; Created: 20 Sept. 2002
;; Version: $Id: tc-skki.el,v 1.2 2002/12/23 05:41:03 kitajima Exp $

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

;;; Commentary:

;;; This file is a wrapper of tc-pre.el, tc-sysdep.el,
;;; and tc-setup.el for skkinput3.

;;; Code:

;;;
;;; supplement code for skkinput3
;;;

;;; new variable

(defvar tcode-im-end-conversion-hook nil)

;;; setup some variables that are not used in skkinput3.
(setq emacs-version "19.28.1"
      emacs-major-version 19
      kill-emacs-hook nil
      self-insert-after-hook nil
      minibuffer-exit-hook nil
      current-prefix-arg nil
      fill-prefix nil)

;;; setup some variables to disable specific features in tc2.
(setq tcode-mazegaki-stroke-priority-list '(23 22 21 20 26 27 28 29)
      tcode-mazegaki-face nil
      tcode-mazegaki-selected-candidate-register nil
      input-method-verbose-flag nil
      tcode-verbose-message nil
      tcode-display-help-delay 0
      tcode-stroke-table-size 0
      tcode-auto-help nil
      tcode-init-file-name (expand-file-name "~/.tc-skk"))

;;; define functions that are not defined on skkinput3.

(defvar features nil
  "A list of symbols which are the features of the executing emacs.
Used by `featurep' and `require', and altered by `provide'.")

(defun require (feature &optional filename noerror)
  "If feature FEATURE is not loaded, load it from FILENAME.
If FEATURE is not a member of the list `features', then the feature
is not loaded; so load the file FILENAME.
If FILENAME is omitted, the printname of FEATURE is used as the file name,
and `load' will try to load this name appended with the suffix `.elc',
`.el' or the unmodified name, in that order.
If the optional third argument NOERROR is non-nil,
then return nil if the file is not found instead of signaling an error.
Normally the return value is FEATURE.
The normal messages at start and end of loading FILENAME are suppressed."
  (if (memq feature features)
      features
    (and (load (or filename 
		   (symbol-name feature))
	       noerror)
	 feature)))

(defun bolp ()
  "Return t if point is at the beginning of a line."
  (save-excursion
    (let ((p (point)))
      (beginning-of-line)
      (= p (point)))))

(or (fboundp 'ash)
    (defun ash (value count)
      (if (>= count 0)
	  (while (> count 0)
	    (setq value (+ value value)
		  count (1- count)))
	(setq count (- count))
	(while (> count 0)
	  (setq value (/ value 2)
		count (1- count))))
      value))

(or (fboundp 'sort)
    (defun sort (list predicate)
      (if (< (length list) 2)
	  list
	(let ((g (list (car list)))
	      l
	      (m (car list)))
	  (setq list (cdr list))
	  (while list
	    (if (funcall predicate (car list) m)
		(setq l (cons (car list) l))
	      (setq g (cons (car list) g)))
	    (setq list (cdr list)))
	  (nconc (sort l predicate) (sort g predicate))))))

(or (fboundp 'make-list)
    (defun make-list (length init)
      (let (l)
	(while (> length 0)
	  (setq l (cons init l)
		length (1- length)))
	l)))

;;; functions below are not yet implemented correctly.

(or (fboundp 'file-newer-than-file-p)
    (defun file-newer-than-file-p (file1 file2)
      nil))

(or (fboundp 'substitute-command-keys)
    (defun substitute-command-keys (string)
      string))

;; (or (fboundp 'buffer-name)
;;     (defun buffer-name (&optional buffer)
;;       buffer-name))

(or (fboundp 'reverse)
    (defun reverse (list)
      (nreverse list)))

(or (fboundp 'make-vector)
    (defun make-vector (num val)
      (let (list)
	(while (> num 0)
	  (setq list (cons val list)
		num (1- num)))
	(vconcat list))))

(or (fboundp 'char-or-string-p)
    (defun char-or-string-p (obj)
      (or (integerp obj)
 	  (stringp obj))))

(or (fboundp 'window-minibuffer-p)
    (defun window-minibuffer-p (window)
      nil))

(or (fboundp 'autoload)
    (defun autoload (function file &optional docstring interactive type)
      "Define FUNCTION to autoload from FILE.
FUNCTION is a symbol; FILE is a file name string to pass to `load'.
Third arg DOCSTRING is documentation for the function.
Fourth arg INTERACTIVE if non-nil says function can be called interactively.
Fifth arg TYPE indicates the type of the object:
   nil or omitted says FUNCTION is a function,
   `keymap' says FUNCTION is really a keymap, and
   `macro' or t says FUNCTION is really a macro.
Third through fifth args give info about the real definition.
They default to nil.
If FUNCTION is already defined other than as an autoload,
this does nothing and returns nil."
      (or (fboundp function)
	  ;; define autoload function
	  )))

(or (fboundp 'char-syntax)
    (defun char-syntax (character)
      (if (memq character '(? ?\n ?\t ?\f ?\r))
	  ? 
	?w)))

(or (fboundp 'make-backup-file-name)
    (defun make-backup-file-name (file)
      (concat file "~")))

(or (fboundp 'all-completions)
    (defun all-completions (string alist &optional predicate hide-spaces)
      (let (list)
	(while alist
	  (if (string= (substring (car (car alist)) 0 (length string))
		       string)
	      (setq list (cons (car (car alist)) list)))
	  (setq alist (cdr alist)))
	(nreverse list))))

(or (fboundp 'screen-width)
    (defun screen-width ()
      (window-width)))

;;;
;;; loading tc-sysdep.el
;;;
(setq tcode-emacs-version 'skkinput3)
(load "tc-sysdep")

;;;
;;; loading tc-setup.el
;;;
(setq tcode-emacs-version 'nemacs)
(load "tc-setup")

;;; tc-skki.el ends here
