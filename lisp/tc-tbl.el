;;; tc-tbl.el --- T-Code-dependent data

;; Copyright (C) 1989--2001 Kaoru Maeda, Yasushi Saito and KITAJIMA Akira.

;; Author: Kaoru Maeda <maeda@src.ricoh.co.jp>
;;	Yasushi Saito <yasushi@is.s.u-tokyo.ac.jp>
;;      KITAJIMA Akira <kitajima@isc.osakac.ac.jp>

;; $Id: tc-tbl.el,v 1.10 2002/12/18 02:03:31 kitajima Exp $

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

;;; Commentary:
;;
;; set following variables:
;;	tcode-input-method
;;	tcode-transparent-mode-indicator
;;	tcode-tcode-mode-indicator
;;	tcode-alnum-2byte-tcode-mode-indicator
;;	tcode-hiragana-mode-indicator
;;	tcode-katakana-mode-indicator
;;	tcode-tbl
;;	tcode-non-2-stroke-char-list
;;	tcode-another-table
;;	tcode-special-commands-alist
;;	tcode-mode-help-string
;;	tcode-stroke-file-name
;;      eelll-text

;;; Code:

(require 'tc)

(setq tcode-input-method 'tcode)

(setq tcode-transparent-mode-indicator "--"
      tcode-tcode-mode-indicator "TC"
      tcode-alnum-2byte-tcode-mode-indicator "£Ô"
      tcode-hiragana-mode-indicator ""
      tcode-katakana-mode-indicator "")

(setq tcode-tbl [
"¢£¢£¢£¢£¢£¢£¢£¢£¢£¢£¥î¥ð¥ñ¥õ¥öÀÁ¶­·ÏÃµ¾Ý¤î¤ð¤ñ¢£¢£À¹³×ÆÍ²¹Êá¢£¢£¢£¢£¢£°ÍÁ¡¼Ú¿ÜÌõ"
"¢£¢£¢£¢£¢£¢£¢£¢£¢£¢£±¯±±±ã±ï±È¾°²ì´ßÀÕµù±÷±ø²µ²º¢£±×±ç¼þ°è¹Ó¢£¢£¢£¢£¢£¿¥ÉãËçÍð¹á"
"¢£¢£¢£¢£¢£¢£¢£¢£¢£¢£µ´µõ¶¹¶¼¶Ã¼Ë´î´´µÖÅü´ñ´ûµÆµÑµý¹¯ÅÌ·Ê½è¤¼¢£¢£¢£¢£¢£¾ù¥ØÌÏ¹ßÁö"
"¢£¢£¢£¢£¢£¢£¢£¢£¢£¢£¸É¸Ø²«¹¡¹ÌÉÛ¶ì°µ·Ã¸Ç¹ª¹îº©º¤ºªË®Éñ»¨´Á¶Û¢£¢£¢£¢£¢£·ã´³É§¶ÑËô"
"¢£¢£¢£¢£¢£¢£¢£¢£¢£¢£ÊôË¿ËÆËÎ¢£»ÑÀäÌ©Èë²¡¢£¢£¢£¢£¢£½°Àá¿ùÆù½ü¢£¢£¢£¢£¢£Â¬·ì»¶¾ÐÊÛ"
"¢£¢£¢£¢£¢£¢£¢£¢£¢£¢£¸ÐÎéÃø°Ü¶¿¢£¢£¢£¢£¢£½þ²¤ÅØÄì°¡¢£¢£¢£¢£¢£¶Ø¾Ë¼ù¶çÁÃ¢£¢£¢£¢£¢£"
"¢£¢£¢£¢£¢£¢£¢£¢£¢£¢£Ã¼¾þÍ¹±ö·²¢£À±ÀÏÁ«Àë¹È½ý¹ë°ÝÃ¦ÁÍÁâÁÕÂº¢£¸¨Èï¸»´êÎ×¢£¢£¢£¢£¢£"
"¢£¢£¢£¢£¢£¢£¢£¢£¢£¢£ºþ¼÷½ç´íº½½î¾Ñ¾æ¾Î¾øÊÞ½¼µÊÏÓË½¢£¢£¢£¢£¢£Èã·Ä¾ÄÎµÊ»¢£¢£¢£¢£¢£"
"¢£¢£¢£¢£¢£¢£¢£¢£¢£¢£¿Ì°·ÊÒ»¥¸ð¢£ÇµÇ¡ÆôÄ¢ÎØÅÝÁàÊÁµû¢£¢£¢£¢£¢£½¢Ãó´øÃ°Á¯¢£¢£¢£¢£¢£"
"¢£¢£¢£¢£¢£¢£¢£¢£¢£¢£¹°ÄËÉ¼ÁÊ°äÍóÎ¶Î¬Î¸ÎßÂ§Â¸ÇÜµí¼á¢£¢£¢£¢£¢£¹Ë³ãÁÏÇØÈé¢£¢£¢£¢£¢£"
"¥ò°¥²Ë·¼ÇÄ»ÀÃëÃº°ðÅò²Ì¹ðºö¼óÇÀÊâ²óÌ³Åç³«Êó»æ´ÛÌë°Ìµë°÷¤ÉÂå¥ì·ç²ÆÈàºÊÁ±Áê²ÈÅªÂÐÎò"
"¥¥°©²ç·ÇÈ²ËÇÁÜ°ÛÎÙµì³µÇã¾ÜÍ³»à¥­¤»¶èÉ´ÌÚ²»²¦Êü¡¹±þÊ¬¤è¥ëÀé¥¢ºâ¿ËÎ¢µïº¹ÉÕ¥×¤Ð¥åºî"
"¥ô°¸²õ·ÈÈò¹¶¾ÆÆ®ÆàÍ¼Éð»ÄÎ¾ºß! ¤ä½Ð¥¿¼êÊÝ°Æ¶Ê¾ð°ú¿¦7 ¤«( ¥È¤ì½¾¹ü¸ü´éÎÌÆâ¹©È¬¥Æ¸«"
"¥Â°Ï³Ó·àÈÜÈ×ÂÓ°×Â®³ÈÉ÷³¬Ç½ÏÀÁý¥³»³¼ÔÈ¯Î©²£¶½»ÉÂ¦³Ð¤­¤ÃÆü¹ñÆóÅ¬Îà¸æ±§¿ä¶åÌ¾Àîµ¡¥Á"
"¥Å°Ã´¨¸­ÈÍµ¥´¹±äÀã¸ßºÙ¸ÅÍø¥Ú¤ã¥Ê¶â¥ÞÏÂ½÷ºêÇò¤°´±µå¾å¤¯8 ¤¨Ç¯Êì±ü°ø¼ò¿­¥µ·ú¥ÑÂèÆþ"
"´ÊÄ§¿¨½¡¿¢¢£º÷¼ÍÂùËý³²ÄÂÀ°·ÚÉ¾º´Ë¡¿ôÏºÃÌÉþÀ¼Ç¤¸¡Ë­ÈþÂê°æÍÎ¼ÂÇúÃçÃãÎ¨ÈæÀÎÃ»´äµðÇÔ"
"¾µ¾Ï¸õÅÓÊ£¢£ºý¼ûÂÂÌÂ·âÀÞÄÉÂâ³ÑÀÜÈ÷ºÇµÞ¸³ÊÑ¿³²þ¾º·Ý½ÉÀ©½¸°Â²èÍÛ¹½Î¹»ÜÍË±ó¥©¾­¤¾ÄÍ"
"²÷ÈÝ»õÉ®Î¤¢£»®½´ÃßÌáÍá½¨»å½Õ¹¬µ­Ä«ÃÎ¥ïÁ÷¸Â¸¦Ï«ÅýÌò¥»±¿¥ÄÆÃÃ«¥¡Æ³Ç§·òÈø½ø¿¶ÎýÇ°Æ¯"
"ÊñÇ¼ÍêÆ¨¿²¢£»¿½ÖÃùÍÓÀÑÄøÃÇÄã¸º¥â»ñ»ÎÈñ¥£µÕ´ëÀº¤¶°õ¿À¤ÓÂÇ¶Ð¥ã»¦Éé²¿ÍúÈÌ¼ª¼øÈÇ¸ú»ë"
"¾§Êë·ûÊÙºá¢£¢£½âÃî¢£¸Î¹ÛÄó»ùÉßÌµÀÐ²°²òÊçÎá°ãÁõÁ³³ÎÍ¥¸øÉÊ¸ì±é·ô°­½©ÈóÊØ¼¨Â¨ÆñÉáÊÕ"
"¤Ñ°Ö²æ·óÉ©ºùÀ¥Ä»ºÅ¾ã¼ýºÝÂÀ±àÁ¥Ãæ¥¹¤â¤ªÄê¼ï²¬·ë¿Ê¿¿3 ¤È¡»¤Æ¤ë¥Ò¹¾ÊÌ¹Í¸¢¥Ã¿Í»°µþ¤Á"
"¤Ô°Ù³Ý·ùÉ³ÅµÇî¶ÚÃéÆý¼ãÍººº¤Õ¾Þ¤ï¥éÅìÀ¸¤íÂð½ÏÂÔ¼è²Ê¡¼¤·¤¿°ì¤¬µÚµ×Â¢ÁáÂ¤¥í¥¯ËüÊý¥Õ"
"¤×±¢´º¸²ÉÁºÎÍØ´õÊ©»¡»Ø»á´ÝÂ³¥§¤¦4 ) ½½¥êÎÁÅÚ³è¤Í»²¤¤¡¢¤Î5 1 ÅêµÁ»»È¾¸©¤ó¤Þ¥ó¤Ä»Í"
"¤Ú±£´Å¸£Ê°·¯½ãÉûÌÁÉ¸¤®³Ê¼¡½¬²Ð¤¢¤³6 ³Ø·î¼õÍ½ÀÚ°éÃÓ¡£¢¡0 ¡¦2 ¹þÂô·³ÀÄÀ¶¤±¥¤¤¹ÅÅÃÏ"
"¤Ý°ß´µ¸·ÊÀÈÈÍ¾ËÙ¸ªÎÅ»×½Ñ¹­ÌçÊ¹ËÜ¤µ¤é¹â¥·±Ñ¥Ü²Ã¼¼¾¯¤Ç¤Ï¤Ë¤Ê¤òÅ¾¶õÀ­»Èµé¶È»þ¡ÖÄ¹¤ß"
"¼ëÃÙ¹ÃÃ×ÈÆ¢£¿ê¼¢ÄÀ¸ÊÉÂ½ªµ¯Ï©±Û¤àÆî¸¶±ØÊªÀªÉ¬¹Ö°¦´ÉÍ×Àß¿åÆ£Í­ÁÇÊ¼Àì¿ÆÎÀ¥Û¶¦¥ÖÊ¿³Ú"
"¿ØÄá¼¯²ßÍí¢£¿ö¼¾ÅºÖá¾ïÄ¥ÌôËÉÆÀ¥±¼°Àï´ØÃËÍ¢·Á½õ¡þÎ®Ï¢Å´¶µÎÏ¥ÙÌÓ±Ê¿½ÂÞÎÉ»ä¥´Íè¿®¸á"
"´ãÈË»ï¾·µ¨¢£¿â¿ÓÅ°Ì¦»û¼Á¤Å¹Á¾òÏÃºÂÀþ¥À¶¶´ð¹¥Ì£ÊõÁè¥Ç¸½¥¨Â¾ÅÙÅùÀõº¢ÍîÌ¿Â¼¥¬À½¹»¤´"
"¼¹¾ÒÌ´²·°¤¢£¿è¢£ÄÞÇÃÄäÎÎÍÆ¶Ì±¦¤ÙÌ±¥½ÅÀ¶øÂ­ÁðÃÛ´Ñ¸À¼ÖÀ®Å·À¤Ê¸ÈÄµÒ»ÕÀÇÈô¥Î´°½ÅÌó³Æ"
"³Ù·º¼å±ÀÁë¢£À£Æ·Æ«¢£²ÏÃÖ¶¡»îÀÊ´ü¥¾ºÐ¶¯·¸ÉØÃÊ±Ò³Û½Â¼ç±Ç½ñ²Ä¤ØÅÁÄí²ÝÃåºä¶á³°ÊÆ¥ç¸÷"
"¤¡¢£´¤¢£¢£¸ÆÉý´¿¸ùÅðÆÁÅÏ¼éÅÐÂàÅ¹»ýÄ®½ê¤Û·ïÍ§Â´½é´·¹Ô¥É±ß¾®¥¸¥è¸í¾Ú´Þ% ³¤Æ»¤ºÀ¾¤²"
"¤£¢£¢£¢£¢£µªÇË·´¹³È¨³£´©Ë¬Í»±«Á´¤¸¼«µÄÌÀµÜ°Ëµáµ»¼ÌÄÌ¥«¼ÒÌîÆ±È½µ¬´¶ÃÍ¥®ÅöÍý¥á¥¦¥°"
"¤¥¢£¢£¢£¢£Ë¼ÀÓ¼±Â°°áÄë»ÏÎ»¶ËÇ®¥ÐÉôÏ»·ÐÆ°¶ÉÆ¬ÇÛ¹õ±¡¤À¤ê¡½¤áÂçºÑµÈ¤æ´ï¾ÈÉÔ¹çÌÌÀ¯¥ª"
"¤§¢£¢£¢£¢£µîµ¿¤ÂÌÊÎ¥ÆÉÎë¶²ÆÄ¶·¸å´Ö¾ì¥Ë»º¸þÉÜÉÙÄ¾ÁÒ¿·¡×9 »Ò¸ÞÀâ½µ¹æÍÕÇÉ°Ñ²½¥ÓÌÜ»Ô"
"¤©¢£¢£¢£¢£ÉÃÈÏ³Ë±ÆËãÂ²ÃúÌ¤ºÍÊÖÌä¥à¼·½»ËÌ³ä¤ÖÈÖË¾¸µ»öÅÄ²ñÁ°¤½µÙ¾Ê±ûÊ¡Ëèµ¤Çä²¼ÅÔ³ô"
"ÍßÁãÌÐ½ÒÏ¯¢£¢£¢£¢£¢£µ¢Ä£ºòÀ×¥²Àö±©¸Ä°åÀÅ²¯Ï¿ÀÖÁÛ¾Ã»Ù¶¨ÍÑÉ½Àµ¿Þµó¸±¥¼ÇÈ¥ä¿´³¦°Õº£"
"Ç÷ºÒÎøÇ¾Ï·¢£¢£¢£¢£¢£´Æ´óºÛÃ£¼Ç¶ÁËºÆ¤»Ë´Ä¿§ÂßÈÎÊÔ»ÅÀèÂ¿¾¦¥Ï¸òÇ·Ëö¤Ü³¹ÌÈºÆ¥Í¡Á¸ýÂæ"
"Î±Îó¹ïÆ¦´Ç¢£¢£¢£¢£¢£ÃÝÃí²ð¶ñ¼º»Ê·Þ²ÚµöÊäº¸ÂÖ²Ö±É¥¶Ä´º®¥Ý·è¥ß½£Ê§¾è¸Ë¾õÃÄ·×É×¿©Áí"
"ÂØ¾Â? ¼­¸¥¢£¢£¢£¢£¢£¤å½¤µæÅúÍÜÉüÊÂ±º¥æÎä¤ÌÅ¸·Ù·¿Ã¯ÁÈÁªÅÞÂòÂÎÎãËþÄÅ½àÍ·¸Í¤Ò¤ç²ÁÍ¿"
"´Ô¹¹ÀêÈ¢Ìð¢£¢£¢£¢£¢£»ÖÈ´¹ÒÁØ¿¼Ã´Î¦´¬¶¥¸îº¬ÍÍÆÈ»ßÆ²¶ä°Ê¥Ì±Ä¼£»úºà²á½ôÃ±¿È¥Ô¾¡È¿¥º"
])

(setq tcode-non-2-stroke-char-list
      (mapcar (function
	       (lambda (str)
		 (tcode-string-to-char str)))
	      '("¢£" "¢¡" "¡þ")))

(setq tcode-another-table nil)

(setq tcode-special-commands-alist
      '(((0 0) . (lambda () (tcode-show-tables nil nil)))
				       ; 11 : LLÉ½¤ÎÉ½¼¨
	((0 9) . (lambda () (tcode-show-tables nil t)))
				       ; 10 : LRÉ½¤ÎÉ½¼¨
	((9 0) . (lambda () (tcode-show-tables t nil)))
				       ; 01 : RLÉ½¤ÎÉ½¼¨
	((9 9) . (lambda () (tcode-show-tables t t)))
				       ; 00 : RRÉ½¤ÎÉ½¼¨
	((1 1) . tcode-start-jiscode)
				 ; 22 : JIS ¥³¡¼¥ÉÉ½ÆþÎÏ
	((2 2) . tcode-toggle-alnum-mode)
				; 33 : 1-2¥Ð¥¤¥ÈÀÚ¤ê´¹¤¨
	((2 1) . tcode-switch-variable)
				   ; 32 : ¶çÆÉÅÀ¤Î¥È¥°¥ë
	((3 3) . (lambda ()
		   (tcode-display-stroke-sequence tcode-last-help-char-list)))
					; 44 : ¥Ø¥ë¥×
	((4 4) . (lambda () (tcode-query-stroke (point))))
					; 55 : ¥Ø¥ë¥×
	((6 6) . tcode-bushu-begin-alternate-conversion)
			     ; 77 : postfix Éô¼ó¹çÀ®ÊÑ´¹
	((7 7) . (lambda () (tcode-transpose-strokes nil)))
				; 88 : transpose-strokes
	((8 8) . tcode-clear)
		     ; 99 : Éô¼ó¹çÀ®ÊÑ´¹¤Ê¤É¤Î¥­¥ã¥ó¥»¥ë
	((26 23) . tcode-bushu-begin-conversion) ; jf : Éô¼ó¹çÀ®ÊÑ´¹¤Î³«»Ï
	((25 23) . tcode-kuten)
	((26 22) . tcode-touten)
	((23 26) . tcode-mazegaki-begin-conversion)
				      ; fj: ¸ò¤¼½ñ¤­ÊÑ´¹
	((9 8) . tcode-mazegaki-begin-alternate-conversion)
			  ; Á°ÃÖ¡¦¸åÃÖ¤¬µÕ¤Î¸ò¤¼½ñ¤­ÊÑ´¹
	;; ¡Ö18¡×¤ÇÆÉ¤ß1Ê¸»ú¤Î¸åÃÖ·¿¸ò¤¼½ñ¤­ÊÑ´¹
	((0 7) . (lambda ()
		   (tcode-mazegaki-convert 1 current-prefix-arg)))
	;; ¡Ö28¡×¤ÇÆÉ¤ß2Ê¸»ú¤Î¸åÃÖ·¿¸ò¤¼½ñ¤­ÊÑ´¹
	((1 7) . (lambda ()
		   (tcode-mazegaki-convert 2 current-prefix-arg)))

	;; ¡Ö38¡×¤ÇÆÉ¤ß3Ê¸»ú¤Î¸åÃÖ·¿¸ò¤¼½ñ¤­ÊÑ´¹
	((2 7) . (lambda ()
		   (tcode-mazegaki-convert 3 current-prefix-arg)))

	;; ¡Ö48¡×¤ÇÆÉ¤ß4Ê¸»ú¤Î¸åÃÖ·¿¸ò¤¼½ñ¤­ÊÑ´¹
	((3 7) . (lambda ()
		   (tcode-mazegaki-convert 4 current-prefix-arg)))

     ;; ¡Ö58¡×¤Ç³èÍÑ¤¹¤ë¸ì¤òÂÐ¾Ý¤È¤·¤¿¸åÃÖ·¿¸ò¤¼½ñ¤­ÊÑ´¹
	((4 7) . (lambda () (tcode-mazegaki-convert nil t)))

	;; ¡Ö29¡×¤ÇÆÉ¤ß2Ê¸»ú¤Î³èÍÑ¤¹¤ë¸ì¤òÂÐ¾Ý¤È¤·¤¿
	;; ¸åÃÖ·¿¸ò¤¼½ñ¤­ÊÑ´¹
	((1 8) . (lambda () (tcode-mazegaki-convert 2 t)))

	;; ¡Ö39¡×¤ÇÆÉ¤ß3Ê¸»ú¤Î³èÍÑ¤¹¤ë¸ì¤òÂÐ¾Ý¤È¤·¤¿
	;; ¸åÃÖ·¿¸ò¤¼½ñ¤­ÊÑ´¹
	((2 8) . (lambda () (tcode-mazegaki-convert 3 t)))

	;; ¡Ö49¡×¤ÇÆÉ¤ß4Ê¸»ú¤Î³èÍÑ¤¹¤ë¸ì¤òÂÐ¾Ý¤È¤·¤¿
	;; ¸åÃÖ·¿¸ò¤¼½ñ¤­ÊÑ´¹
	((3 8) . (lambda () (tcode-mazegaki-convert 4 t)))

	;; ¡Ö59¡×¤ÇÆÉ¤ß5Ê¸»ú¤Î³èÍÑ¤¹¤ë¸ì¤òÂÐ¾Ý¤È¤·¤¿
	;; ¸åÃÖ·¿¸ò¤¼½ñ¤­ÊÑ´¹
	((4 8) . (lambda () (tcode-mazegaki-convert 5 t)))))

(setq tcode-mode-help-string "\
T¥³¡¼¥É¥â¡¼¥ÉÃæ¤Î¥­¡¼Áàºî¤Ï¼¡¤Î¤È¤ª¤ê¡£
   jf : Éô¼ó¹çÀ®ÊÑ´¹¥â¡¼¥É¤ËÆþ¤ë¡£jf¤òÂÇ¤ÁÂ³¤±¤ë¤ÈºÆµ¢Åª¤ËÉô¼ó¹çÀ®ÊÑ´¹¤ò
	¹Ô¤¦¤³¤È¤¬¤Ç¤­¤ë(see variable `tcode-use-postfix-bushu-as-default')¡£
   fj : ¸ò¤¼½ñ¤­ÊÑ´¹¤ò¹Ô¤¦(see variable `tcode-use-prefix-mazegaki')¡£
   00, 01, 10, 11 : T¥³¡¼¥É¤ÎÉ½¤òÉ½¼¨¤¹¤ë(0¤¬º¸¡¢1¤¬±¦)¡£
   22 : JIS ¥³¡¼¥É°ìÍ÷É½¤Ë¤è¤ëÆþÎÏ¡£
   32 : ¡¢¡£¤È, . ¤òÀÚ¤êÂØ¤¨¤ë¡£(see variable `tcode-switch-table-list')¡£
   33 : T¥³¡¼¥ÉÉ½¤Ë¤¢¤ë±Ñ¿ô»ú¡¦µ­¹æ¤ÎÊ¸»ú¥³¡¼¥É¤Î1¥Ð¥¤¥È¡¦2¥Ð¥¤¥ÈÀÚ¤êÂØ¤¨¡£
   44 : Ä¾Á°¤ËÉ½¼¨¤·¤¿ÂÇ¤ÁÊý¤òºÆÉ½¼¨¤¹¤ë¡£
   55 : ¥Ý¥¤¥ó¥È°ÌÃÖ¤Ë¤¢¤ëÊ¸»ú¤ÎÂÇ¤ÁÊý¤òÉ½¼¨¤¹¤ë¡£
   58 : ³èÍÑ¸ì¤òÍ¥Àè¤·¤Æ¸ò¤¼½ñ¤­ÊÑ´¹¤ò¹Ô¤¦¡£
   77 : ¥Ý¥¤¥ó¥ÈÁ°¤Ë¤¢¤ë2Ê¸»ú¤ÇÉô¼ó¹çÀ®ÊÑ´¹¤ò¹Ô¤¦¡£
   88 : ¥Ý¥¤¥ó¥È°ÌÃÖ¤Ë¤¢¤ëÊ¸»ú¤òµÕ¥¹¥È¥í¡¼¥¯²½¤¹¤ë(Îã: Ì£->¤Î)¡£
        ¹ÔËö¤Ç¤Ï¥Ý¥¤¥ó¥È¤ÎÄ¾Á°¤ÎÊ¸»ú¤òÊÑ´¹¤¹¤ë¡£
   99 : ¸ò¤¼½ñ¤­ÊÑ´¹¥â¡¼¥É¤äÉô¼ó¹çÀ®ÊÑ´¹¥â¡¼¥É¤Ë¤¤¤¿»þ¤Ë¡¢
	¤½¤ì¤é¤òÁ´Éô¥­¥ã¥ó¥»¥ë¤¹¤ë¡£¤Þ¤¿¡¢¥Ø¥ë¥×¤ò¾Ã¤¹¡£
   [1-4]8, [2-5]9: Ê¸»ú¿ô¤ò»ØÄê¤·¤Æ¸ò¤¼½ñ¤­ÊÑ´¹¤ò¹Ô¤¦¡£
   \\[toggle-input-method] : T¥³¡¼¥É¥â¡¼¥É¤òÈ´¤±¤ë¡£

½é¤á¤Æµ¯Æ°¤µ¤ì¤¿»þ¤Ë¤Ï¡¤`tcode-ready-hook' ¤ò¼Â¹Ô¤¹¤ë¡£
¤Þ¤¿¡¢µ¯Æ°¤µ¤ì¤ëÅÙ¤Ë`tcode-toggle-hook'¤ò¼Â¹Ô¤¹¤ë¡£")

(setq tcode-stroke-file-name (concat tcode-data-directory "tcode.st"))

(setq eelll-text "EELLLTXT")

;;; tc-tbl.el ends here
