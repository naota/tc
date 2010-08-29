;;; tc-bitmap.el --- tc help display using bitmap-mule.

;; Copyright (C) 2001, 2002 YAGI Tatsuya

;; Author: YAGI Tatsuya <yagi@is.titech.ac.jp>
;; Version: $Id: tc-bitmap.el,v 2.7 2002/08/17 01:52:19 kitajima Exp $
;; Maintainer: KITAJIMA Akira <kitajima@isc.osakac.ac.jp>

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
(require 'tc-help)
(require 'bitmap)

(defvar tc-bitmap-cache-file "tc-bitmap-8x16")
(defvar tc-bitmap-cache
  (let* ((load-path (cons tcode-data-directory 
			  (cons tcode-site-data-directory load-path)))
	 (file (locate-library tc-bitmap-cache-file t)))
    (with-temp-buffer
      (insert-file-contents file)
      (read (current-buffer)))))

(defun tc-bitmap-get-key-0 (key1 key2 &optional side)
  (nth 1 (assoc (format "%s %s%s" (if side "side  " "center")
			(or key1 "/") (or key2 "/"))
		tc-bitmap-cache)))

(defun tc-bitmap-get-key-1 (&optional key1 key2)
  (let* ((l (list (tc-bitmap-get-key-0 nil nil t)
		  (tc-bitmap-get-key-0 nil nil t)
		  (tc-bitmap-get-key-0 nil nil nil)
		  (tc-bitmap-get-key-0 nil nil t)
		  (tc-bitmap-get-key-0 nil nil t)))
	 k1x k1y i k2x k2y j)
    (if key1 (setq k1x (% key1 10) k1y (/ key1 10) i (/ k1x 2)))
    (if key2 (setq k2x (% key2 10) k2y (/ key2 10) j (/ k2x 2)))
    (if (and key1 (eq i j))
	(setcar (nthcdr i l)
		(tc-bitmap-get-key-0 (+ (* 4 (- k1x (* 2 i))) k1y)
				     (+ (* 4 (- k2x (* 2 j))) k2y)
				     (/= i 2)))
      (if key1 (setcar (nthcdr i l)
		       (tc-bitmap-get-key-0 (+ (* 4 (- k1x (* 2 i))) k1y)
					    nil
					    (/= i 2))))
      (if key2 (setcar (nthcdr j l)
		       (tc-bitmap-get-key-0 nil
					    (+ (* 4 (- k2x (* 2 j))) k2y)
					    (/= j 2)))))
    (apply (function concat) l)))

(defun tc-bitmap-stroke-to-string (stroke)
  (let ((lis (list tcode-stroke-to-string-opener)))
    (while stroke
      (setq lis (cons (tc-bitmap-get-key-1 (car stroke) (nth 1 stroke)) lis)
	    lis (cons tcode-stroke-to-string-separator lis)
	    stroke (cdr (cdr stroke))))
    (setq lis (cons tcode-stroke-to-string-closer (cdr lis)))
    (apply (function concat) (nreverse lis))))

(provide 'tc-bitmap)
;;; tc-bitmap.el ends here
