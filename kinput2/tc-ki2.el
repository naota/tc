;;; tc-ki2.el --- make kinput2 data for T-Code

;;; usage:
;;  1.  M-x load-file RET (this file) RET
;;  2.  C-x b rule.tcode RET
;;  3.  C-x C-w (directory including ccdef.tcode) RET
;;
;; see T-Code info for detail.

(require 'tc)

(save-excursion
  (set-buffer (get-buffer-create "rule.tcode"))
  (erase-buffer)
  (goto-char (point-max))
  (let ((i 0) c1 c2
	j
	str)
    (while (< i 40)
      (setq j 0
	    c1 (char-to-string (tcode-key-to-char i)))
      (while (< j 40)
	(setq c2 (char-to-string (tcode-key-to-char j))
	      str (let ((v (tcode-decode (list i j) tcode-table)))
		    (if (integerp (cdr v))
			(char-to-string (cdr v)))))
	(if (and (stringp str)
		 (not (string= str "¢£"))
		 (not (string= str "*")))
	    (insert "\t\"" c1 "\"\t'" c2 "'\t\"" str "\"\n"))
	(if (= (% j 10) 0)
	    (insert "\n"))
	(setq j (1+ j)))
      (setq i (1+ i)))))

;;; tc-ki2.el ends here
