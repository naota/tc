;;; tc-image.el --- tc help display using image.

;; Copyright (C) 2001, 2002 YAGI Tatsuya

;; Author: YAGI Tatsuya <yagi@is.titech.ac.jp>
;; Version: $Id: tc-image.el,v 2.5 2002/08/17 01:53:58 kitajima Exp $
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
(or (and (fboundp 'image-type-available-p)
	 (or (image-type-available-p 'xpm)
	     (image-type-available-p 'pbm)))
    (error "You cannot display image."))

(require 'tc-help)

(defvar tc-image-type
  (if (image-type-available-p 'xpm) 'xpm 'pbm))

(defvar tc-image-type-alist
  '((pbm ;; type
     "P4\n%d %d\n";; header
     "" ;; trailer
     tc-image-make-pbm-line ;; function
     )
    (xpm ;; type
     ;; header
     "/* XPM */
static char * image_name[] = {
\"%d %d 7 1\",
\" \tc None\",
\"-\tc white\",
\"X\tc black\",
\"x\tc grey\",
\"R\tc red\",
\"G\tc green\",
\"B\tc blue\",
"
     "};\n" ;; trailer
     tc-image-make-xpm-line ;; function
     ))
  "Associative list of button image type data.
First  element is image type.
Second element is image header format.
Third  element is image trailer string.
")

(defvar tc-image-button
  '(
    ;; no strokes(top or center)
    ("    "
     "    "
     "    "
     "    ")
    ;; no strokes(side)
    ("    "
     " XX "
     " XX "
     "    ")
    ;; first stroke
    (" XX "
     "XXXX"
     "XXXX"
     " XX ")
    ;; second stroke
    (" XX "
     "X  X"
     "X  X"
     " XX ")
    ;; first stroke and second stroke
    (" XX "
     "XX X"
     "X XX"
     " XX ")
    )
  "List of button images.
First  element is image for no strokes(top or center).
Second element is image for no strokes(side).
Third  element is image for first stroke.
Fourth element is image for first stroke.
Fifth  element is image for first stroke and second stroke.
")

(defvar tc-image-margins '(0 0 0 0 0)
  "Margin for image of buttons.
List of left-margin, top-margin, right-margin, bottom-margin and linespace.")

(defun tc-image-make-pbm-line (width offset str)
  (let ((ret (make-string (1+ (lsh (1- width) -3)) 0))
	(i 0)
	(l (length str))
	(pos offset)
	bpos)
    (while (< i l)
      (if (memq (aref str i) '(?\  ?-))
	  nil
	(setq bpos (lsh pos -3))
	(aset ret bpos (logior (aref ret bpos) (lsh 1 (- 7 (logand 7 pos))))))
      (setq i (1+ i)
	    pos (1+ pos)))
    ret))

(defun tc-image-make-xpm-line (width offset str)
  (concat "\""
	  (make-string offset ?\ )
	  str
	  (make-string (- width offset (length str)) ?\ )
	  "\",\n"))

(defun tc-image-make-line (width offset str)
  (funcall (nth 3 (assq tc-image-type tc-image-type-alist))
	   width offset str))

(defun tc-image-make-brank-lines (w h)
  (apply (function concat) (make-list h (tc-image-make-line w 0 nil))))

(defun tc-image-make-button-lines (w h offset l)
  (concat (mapconcat (lambda (s) (tc-image-make-line w offset s)) l nil)
	  (tc-image-make-brank-lines w (- h (length l)))))

(defun tc-image-button-width ()
  (apply (function max)
	 (mapcar (function length)
		 (apply (function append) tc-image-button))))

(defun tc-image-button-height ()
  (apply (function max)
	 (mapcar (function length)
		 tc-image-button)))

(defun tc-image-width ()
  (+ (nth 0 tc-image-margins)
     (tc-image-button-width)
     (nth 2 tc-image-margins)))

(defun tc-image-height ()
  (+ (nth 1 tc-image-margins)
     (* 4 (tc-image-button-height))
     (* 3 (nth 4 tc-image-margins))
     (nth 3 tc-image-margins)))

(defun tc-image-make-cache ()
  (let* ((button-width  (tc-image-button-width))
	 (button-height (tc-image-button-height))
	 (w (tc-image-width))
	 (h (tc-image-height))
	 (alist (assq tc-image-type tc-image-type-alist))
	 (image-header (format (nth 1 alist) w h))
	 (image-trailer(nth 2 alist))
	 (top-lines    (tc-image-make-brank-lines w (nth 1 tc-image-margins)))
	 (bottom-lines (tc-image-make-brank-lines w (nth 3 tc-image-margins)))
	 (space-lines  (tc-image-make-brank-lines w (nth 4 tc-image-margins)))
	 (buttons (mapcar (lambda (l)
			    (tc-image-make-button-lines
			     w button-height
			     (nth 0 tc-image-margins) l))
			  tc-image-button))
	 (v (make-vector 50 nil))
	 (i 0) pos1 pos2 lines-1 lines-2 lines-3 lines-4)
    (while (< i 25)
      (setq pos1 (/ i 5)
	    pos2 (% i 5)
	    lines-1 (tc-image-button-subr 1 pos1 pos2 buttons)
	    lines-2 (tc-image-button-subr 2 pos1 pos2 buttons)
	    lines-3 (tc-image-button-subr 3 pos1 pos2 buttons)
	    lines-4 (tc-image-button-subr 4 pos1 pos2 buttons))
      (aset v i
	    (concat image-header top-lines
		    lines-1 space-lines
		    lines-2 space-lines
		    lines-3 space-lines
		    lines-4 bottom-lines image-trailer))
      (if (eq lines-2 (nth 1 buttons)) (setq lines-2 (nth 0 buttons)))
      (if (eq lines-3 (nth 1 buttons)) (setq lines-3 (nth 0 buttons)))
      (if (eq lines-4 (nth 1 buttons)) (setq lines-4 (nth 0 buttons)))
      (aset v (+ i 25)
	    (concat image-header top-lines
		    lines-1 space-lines
		    lines-2 space-lines
		    lines-3 space-lines
		    lines-4 bottom-lines image-trailer))
      (setq i (1+ i)))
    (setq i 0)
    (while (< i 50)
      (let ((image (create-image (aref v i) tc-image-type t ':ascent 'center))
	    (s (make-string 2 ?\ )))
	(put-text-property 0 1 'display image s)
	(put-text-property 1 2 'invisible t s)
	(aset v i s))
      (setq i (1+ i)))
    v))

(defun tc-image-button-subr (pos pos1 pos2 buttons)
  (if (eq pos1 pos)
      (if (eq pos2 pos) (nth 4 buttons) (nth 2 buttons))
    (if (eq pos2 pos) (nth 3 buttons) (nth (if (eq pos 1) 0 1) buttons))))

(defconst tc-image-cache
  (tc-image-make-cache))

(defun tc-image-get-key-0 (pos1 pos2 &optional side-p)
  (aref tc-image-cache (+ (* pos1 5) pos2 (if side-p 0 25))))

(defun tc-image-get-key-1 (&optional key1 key2)
  (let* ((l (list (tc-image-get-key-0 0 0 t)
		  (tc-image-get-key-0 0 0 t)
		  (tc-image-get-key-0 0 0 t)
		  (tc-image-get-key-0 0 0 t)
		  (tc-image-get-key-0 0 0 nil)
		  (tc-image-get-key-0 0 0 nil)
		  (tc-image-get-key-0 0 0 t)
		  (tc-image-get-key-0 0 0 t)
		  (tc-image-get-key-0 0 0 t)
		  (tc-image-get-key-0 0 0 t)))
	 k1x k1y k1side-p k2x k2y k2side-p)
    (if key1 (setq k1x (% key1 10)
		   k1y (/ key1 10)
		   k1side-p (and (/= k1x 4) (/= k1x 5))))
    (if key2 (setq k2x (% key2 10)
		   k2y (/ key2 10)
		   k2side-p (and (/= k2x 4) (/= k2x 5))))
    (if (and key1 (eq k1x k2x))
	(setcar (nthcdr k1x l)
		(tc-image-get-key-0 (1+ k1y) (1+ k2y) k1side-p))
      (if key1 (setcar (nthcdr k1x l)
		       (tc-image-get-key-0 (1+ k1y) 0 k1side-p)))
      (if key2 (setcar (nthcdr k2x l)
		       (tc-image-get-key-0 0 (1+ k2y) k2side-p))))
    (apply (function concat) l)))

(defun tc-image-stroke-to-string (stroke)
  (let ((lis (list tcode-stroke-to-string-opener)))
    (while stroke
      (setq lis (cons (tc-image-get-key-1 (car stroke) (nth 1 stroke)) lis)
	    lis (cons tcode-stroke-to-string-separator lis)
	    stroke (cdr (cdr stroke))))
    (setq lis (cons tcode-stroke-to-string-closer (cdr lis)))
    (apply (function concat) (nreverse lis))))

(provide 'tc-image)
;;; tc-image.el ends here
