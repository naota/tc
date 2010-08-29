;;; tutc-tbl.el --- do TUT-Code on T-Code driver tc.el

;; Copyright (C) 1997--2001 KITAJIMA Akira

;; Author: KITAJIMA Akira <kitajima@isc.osakac.ac.jp>
;; Created: 4 Mar 1997
;; Version: $Id: tutc-tbl.el,v 1.11 2003/05/18 08:39:37 kitajima Exp $

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

;;; Code:

(require 'tc)

(setq tcode-input-method 'tutcode)

(setq tcode-transparent-mode-indicator "------"
      tcode-tcode-mode-indicator "TUT"
      tcode-alnum-2byte-tcode-mode-indicator "£Ô "
      tcode-hiragana-mode-indicator " ¤Ò"
      tcode-katakana-mode-indicator " ¥«")

;;  1  2  3  4	5    6	7  8  9	 0
;;  q  w  e  r	t    y	u  i  o	 p
;;  a  s  d  f	g    h	j  k  l	 ;
;;  z  x  c  v	b    n	m  ,  .	 /

;;  0  1  2  3	4    5	6  7  8	 9
;; 10 11 12 13 14   15 16 17 18 19
;; 20 21 22 23 24   25 26 27 28 29
;; 30 31 32 33 34   35 36 37 38 39

;;  ---> °ìÂÇÌÜ¤Î¥­¡¼
;; |
;; v ÆóÂÇÌÜ¤Î¥­¡¼
; 1 2 3 4 5 6 7 8 9 0 q w e r t y u i o p a s d f g h j k l ; z x c v b n m , . /
(setq tcode-tbl [
nil nil nil nil nil nil nil nil nil nil ; 1-0
"¢£¢£¢£¢£¢£¢£¢£¢£¢£¢£ÊÂÂÖÎ¾¾èÀì¶½¸ýÍÎÁ¥µ×°­ÉÂÁá»å»î¾¾°ÂÅÔÍ¿À¶¿­±©ÀÅºâ»Ï²Ï±ÛÅý½¨Ìý" ;q
"¢£¢£¢£¢£¢£¢£¢£¢£¢£¢£ÅÁ·òÂÔÇò¹Á¿å»ºÌ³±ØÆ¬º¬»ÅÌµ°Æ³ÛÆ°ÀîµëÌÚº´Î¢Ã»²¿ÀÑ·Þ¸µÂ¢¸©ÃÛ³Ñ" ;w
"¢£¢£¢£¢£¢£¢£¢£¢£¢£¢£ÀÄ³è·ëÍ¥ºÇ¶âÃÏËü·úÅ´Å·º£³Æ´Ø±¿¹â¹ñ»°È¬ËÌÎàÈóÊä·ÚÎ¨ºê½É²Ä¼¼½Â" ;e
"¢£¢£¢£¢£¢£¢£¢£¢£¢£¢£À°¾¯¿®ÊóÂÀÄ®¼·µþÍý±ÄÎÀ¹­Ê¿²»·×»³ÅÅ¸Þ¾®¸¶ÉüÍ·»Üµ¢ÊØÈ¾²¬ÀÐµÙË­" ;r
"¢£¢£¢£¢£¢£¢£¢£¢£¢£¢£¢¢¢¢¢¢¢¢¢¢ÂèÏÂ¶èÀ¾ÉÙ¢¢¢¢¢¢¢¢¢¢Ì¾Éô¶ÈÌä¶á¢¢¢¢¢¢¢¢¢¢ÎÓÌç³ÊµÈÎð" ;t
"¢£¢£¢£¢£¢£¢£¢£¢£¢£¢£¿©»ý»Ò½Ð¹Ô¢¢¢¢¢¢¢¢¢¢½ñ¾ì²ñ¾åÄÌ¢¢¢¢¢¢¢¢¢¢Íî¾ð»ä¼èÊÌ¢¢¢¢¢¢¢¢¢¢" ;y
"¢£¢£¢£¢£¢£¢£¢£¢£¢£¢£¤æ¤à¤¯¤¦¤Õ³äµÚ³Ú±à½ªÌÜ¤¹¤Ä¤Ì¤ëÀ½¼ïÍÑ³¦ÍÆ¹¥´ÛÄ´ÃÎÀÚÄ£Ç·Î¹Ãç¶Ë" ;u
"¢£¢£¢£¢£¢£¢£¢£¢£¢£¢£Áê¤ß¤­¤¤¤ÒÌ±É½É×²ÃÇ½¤ð¤·¤Á¤Ë¤êÏ¢¼Ö°Ê´°µ»´ï¿ÊÂÐÍ×°ÕÁ³ÃÇ¸±¹½Í¹" ;i
"¢£¢£¢£¢£¢£¢£¢£¢£¢£¢£²òµ¤¼Ô¼êÅªµå±¡ÈÖ¾ïÎÎ´üÈ¯¿Í°÷À¸Í³¶ÉÂÎÌôÃÊÆÉµá±é½Ñ¿È»úÍÜ·â»Êµï" ;o
"¢£¢£¢£¢£¢£¢£¢£¢£¢£¢£ÇÛÍ½µÄÆÃË¡²ÌÅù¼óÌÓ·ôÂ³ÁíÄê½÷À®µé²Ý°é¿§µðÃ£¸Â±þÇã´¶³ÕÃ´¾­³£¸î" ;p
"¢£¢£¢£¢£¢£¢£¢£¢£¢£¢£Ç°±¦»ÄÀÖ·Á¶ø°æÅç»ÎÄÚ¿¼´É¶ÐÁý½¬Æ£ÉÕ·ÐÀµ¾ÞÁÕ¼¨»ù»ÖÌ¤¸¦ÍÕÁÒÉ©Íá" ;a
"¢£¢£¢£¢£¢£¢£¢£¢£¢£¢£ÀÇ¼ÁÃÄ»²´ðµ¡ÌîÁ°É´ÀÜËèÊ¡Â¾ÊÆÍøÂå·îÅÄ¸å¾¦°åÃ±ÂßÄÉµó´ÝÂ´Ä«ÇÀ²¯" ;s
"¢£¢£¢£¢£¢£¢£¢£¢£¢£¢£¹õ»ñ¿ôÉÊºÂ³ØËÜÂçÏ»³ôÌóÍ­Á´¼«½»ÅìÆóÆü»þÉÔÉ¾Äí²þÌò²ÖµÞ³¤»Ô»Ù¿¹" ;d
"¢£¢£¢£¢£¢£¢£¢£¢£¢£¢£·ÙÂ÷¸øºÙÈñ¹©»öÃæ¶å¸áÄ¾Á÷ÊÝÀ¤¸½¿·Ç¯°ìÆ±ÊâÆ¤¶ñËÉÁÇ²ð¸÷ÏºÊ¸ÂôÇÏ" ;f
"¢£¢£¢£¢£¢£¢£¢£¢£¢£¢£¢¢¢¢¢¢¢¢¢¢ÆâÀéÊ¬Î©³°¢¢¢¢¢¢¢¢¢¢±ß»Í½½ÌÌ¶µ¢¢¢¢¢¢¢¢¢¢¼¡ÂðÂ¼¸ÅÂÞ" ;g
"¢£¢£¢£¢£¢£¢£¢£¢£¢£¢£¼ç¤á¤±¤¨¤Ø¢¢¢¢¢¢¢¢¢¢¤ñ¤»¤Æ¤Í¤ì¢¢¢¢¢¢¢¢¢¢È½ÅêÀïÁÈÂ¿¢¢¢¢¢¢¢¢¢¢" ;h
"¢£¢£¢£¢£¢£¢£¢£¢£¢£¢£¤è¤â¤³¤ª¤Û¾¡»á²è¿´ÈÎ¤ò¤½¤È¤Î¤íÎÏ¸³´ÖÀß¹æºöÈ÷ÅöÍè¶¯ÎÉÍÍ¾Ê·¿¸Î" ;j
"¢£¢£¢£¢£¢£¢£¢£¢£¢£¢£¤ä¤Þ¤«¤¢¤ÏºÐÅÙ¹»¶äÅ¾¤ï¤µ¤¿¤Ê¤é²½²¼²óÎò·¸¼ãµ­°ÑÆþ¼ÂÏÀÃÍÏ©ÁõÆ¯" ;k
"¢£¢£¢£¢£¢£¢£¢£¢£¢£¢£¢¢¸«¢¢¢¢¢¢»æ±ÑÂê¹ðÀª¢¢¢¢¢¢¤ó¼ÒÌëÊª½êºÝÂ¦Ç¤°úÅÞ¼õ»×ÈÌÊÔ°õÇÔ¼ø" ;l
"¢£¢£¢£¢£¢£¢£¢£¢£¢£¢£¶õÀþ¹ç²È½¸²¦±Ò·à¿ÆÇú²ÎÌÀÄ¹ÊýÀ¯°Ìºß¼°ºÑµæÃÖÁèµÁººÇÉËöÎÝ¾õÂ¨ÊÕ" ;;
"¢£¢£¢£¢£¢£¢£¢£¢£¢£¢£ÊÖ¶¥ÊìÇ®Âà´ëÉþ¹ÝÄä½£À±µö³¹ÎëÄëÀºÍ§¼ý»ûÍú»¨¿äÅÏ¸ú±§¿¶¹ÛÉßÍ»±º" ;z
"¢£¢£¢£¢£¢£¢£¢£¢£¢£¢£º×Ë¬½ô½¤ÈÇ°Ë½µ³¬Ï«ÈÄÂ²½¾Æ²Ê¼´ÆÉÍ¸ÍÎÁ²Ðºä´©ÆÁÅÐÎä¶ÁÄÅÄÍÈøÃÝÀô" ;x
"¢£¢£¢£¢£¢£¢£¢£¢£¢£¢£ÆÞÃíÌÈÃãºò²Ê²°±Çºå¾ºÄÂÀ²ÆÈ´ÑÊÑÆîÃ«Æ»¶¦¹¾¼ò¸æ¼éÄø»ËÁð¾ò±É¸ËÇÈ" ;c
"¢£¢£¢£¢£¢£¢£¢£¢£¢£¢£¢¢¢¢¢¢¢¢¢¢±ûÃÓÀ©¾ÚÀõ¢¢¢¢¢¢¢¢¢¢²£Âæ¶¶ÉðÊõ¢¢¢¢¢¢¢¢¢¢¼Ç¹¬¶ÌÍË´Ä" ;v
"¢£¢£¢£¢£¢£¢£¢£¢£¢£¢£¢¢¢¢¢¢¢¢¢¢¾¼µÜ¿¦Íº½Õ¢¢¢¢¢¢¢¢¢¢·³ÅÚ¿ÀÂ¤·Ý¢¢¢¢¢¢¢¢¢¢´ä¾ë±Ê½©Î¦" ;b
"¢£¢£¢£¢£¢£¢£¢£¢£¢£¢£É¬´±¼£Àè½é¢¢¢¢¢¢¢¢¢¢½õÀ­ÏÃ¸ìÅÀ¢¢¢¢¢¢¢¢¢¢ÉéÇ§°¦Ãå¾Ã¢¢¢¢¢¢¢¢¢¢" ;n
"¢£¢£¢£¢£¢£¢£¢£¢£¢£¢£¹ÖÉÜÃÌ¶¨È¿¢¢¢¢¢¢¢¢¢¢¼ÌÊçºî¸ò½Å¢¢¢¢¢¢¢¢¢¢¼ºÁÛÎ®¸¡ºÆ¢¢¢¢¢¢¢¢¢¢" ;m
"¢£¢£¢£¢£¢£¢£¢£¢£¢£¢£»È¿¿³«·èÈþÂâÈô¹Ò´óËþ¸¢ÁªÇäÅ¹ÃË´ÞÊ¹·ï»ÕÄãµ¯Ì¿ÀâÉØ¸À½ð³²¸Ä´¬±«" ;,
"¢£¢£¢£¢£¢£¢£¢£¢£¢£¢£º¸»»»Ø¸þÊü¸ºÍÛÊ§´éÉá¿½É÷ÂÇ²Á¹þÈæ»ß¿³»¦ÎãÈ´Æ³ÀÊÏ¿Ä¥²ÚÎÌ»ëÈàºÍ" ;.
"¢£¢£¢£¢£¢£¢£¢£¢£¢£¢£Å¸ÆÀ»àÍ¢Ì£ÃúÁ±ºàº¹Àö³ÎÂ­¹ÍË¾µ¬½àÆñºÛÅ¬¿Þ°ãÀ¼ÄóÅú²á¸üÆÄÎý¶··Ê" ;/
])

(setq tcode-non-2-stroke-char-list (list (tcode-string-to-char "¢£")
					 (tcode-string-to-char "¢¢")))

(setq tcode-another-table
      [ nil  nil  nil  nil  nil	   nil	nil  nil  nil  nil
	"¡È" "¢©" "¡¼" "¡Ú" "¡Ö"   "¡×" "¡Û" "¡Ä" "¡¦" "¡É"
	"¡Æ" "¡ù" tcode-touten tcode-kuten "¡Ø"	  "¡Ù" "¡¹" "¢¨" "¡À" "¡Ç"
	"¡º" "¡ø" "¡û" "¡à" "¢«"   "¢ª" "¡ß" "¡Ô" "¡Õ" "¡¿"])

(defconst tut-over-2-strokes-table
  '((10 ; q
	(14 . ; t
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "ÃÒ" "Ëº" "Ì¥" "²Ç" "Ã²"
	     nil nil nil nil nil   "ÈÏ" "µß" "·Ñ" "µÍ" "À£"
	     nil nil nil nil nil   "ËÛ" "¿±" "ÃÀ" "Ë°" "ÃÞ"])
	(24 . ; g
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "Å¯" "¶Ó" "³÷" "Í¶" "¾¢"
	     nil nil nil nil nil   "¾°" "»í" "²÷" "¹ü" "·¤"
	     nil nil nil nil nil   "¶ô" "Éä" "ÉÝ" "±Â" "È»"])
	(28 ; l
	 (16 . "¤å") ; u
	 (26 . "¤ç") ; j
	 (27 . "¤ã")) ; k
	(33 . ; v
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "»·" "¿Ö" "·ù" "Êå" "ÆÑ"
	     nil nil nil nil nil   "Á¶" "Ê·" "Ë¦" "°É" "¼ö"
	     nil nil nil nil nil   "Ï¾" "±í" "µä" "¹í" "·ñ"])
	(34 . ; b
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "¼¥" "°ª" "Åé" "ËÓ" "³ò"
	     nil nil nil nil nil   "°±" "°ò" "Æé" "°ý" "³±"
	     nil nil nil nil nil   "¼Þ" "Éµ" "°Ð" "Ã¾" "Ï®"]))

    (11 ; w
	(14 . ; t
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "À¿" "³Ð" "½Ò" "Â»" "¼á"
	     nil nil nil nil nil   "¾Ò" "Î×" "º½" "ÁÐ" "¸®"
	     nil nil nil nil nil   "ÃÔ" "Â·" "ÉÏ" "Ï°" "±¤"])
	(24 . ; g
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "ÃÆ" "ºá" "¸õ" "ºÞ" "Ê¥"
	     nil nil nil nil nil   "·û" "²­" "Ëã" "Áñ" "´ñ"
	     nil nil nil nil nil   "Ãâ" "Ï­" "Ëä" "²º" "»±"])
	(33 . ; v
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "º¢" "Éº" "ÈÉ" "Æ¿" "ËØ"
	     nil nil nil nil nil   "»÷" "ÄÔ" "Í÷" "½Á" "Í©"
	     nil nil nil nil nil   "ÊË" "ºñ" "Ç¨" "ºª" "Æç"])
	(34 . ; b
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "Æô" "´÷" "Å©" "³æ" "±ù"
	     nil nil nil nil nil   "Ë¿" "¾±" "»ð" "¸Õ" "¿ó"
	     nil nil nil nil nil   "Ã¡" "Æø" "°ü" "±°" "Æè"]))

    (12 ; e
	(14 . ; t
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "Ìõ" "Èë" "Îá" "ÎÙ" "¸·"
	     nil nil nil nil nil   "Áö" "ºÊ" "³×" "Â°" "Ç¾"
	     nil nil nil nil nil   "°Ò" "´µ" "Î¬" "À§" "ºï"])
	(24 . ; g
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "Ãº" "ÀÓ" "À¹" "ÊÛ" "½¡"
	     nil nil nil nil nil   "Æù" "µÖ" "¿ù" "ÂÓ" "¹È"
	     nil nil nil nil nil   "¿¯" "²«" "¹Ë" "°ß" "±´"])
	(28 ; l
	 (12 (25 . "¥ö")  ; e h
	     (27 . "¥õ"))  ; e k
	 (16 . "¤°") ; u
	 (17 . "¤®") ; i
	 (25 . "¤²") ; h
	 (26 . "¤´") ; j
	 (27 . "¤¬")) ; k
	(33 . ; v
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "ÊÓ" "ÃÜ" "ËÏ" "·é" "»Ã"
	     nil nil nil nil nil   "ÂÙ" "Ä°" "Çµ" "²ü" "ÍÓ"
	     nil nil nil nil nil   "Á­" "Êî" "º²" "°Ô" "°È"])
	(34 . ; b
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "ÇÒ" "ÂÚ" "¶Ý" "ÏÅ" "ÄÒ"
	     nil nil nil nil nil   "ºý" "ÈÕ" "¼Í" "È±" "ÍÉ"
	     nil nil nil nil nil   "ËË" "Êº" "²ö" "·½" "Á×"]))

    (13 ; r
	(14 . ; t
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "¸»" "Àã" "Â¬" "·Ä" "ÏÇ"
	     nil nil nil nil nil   "¹á" "½è" "ÀÁ" "¹³" "À×"
	     nil nil nil nil nil   "ÂÑ" "¶ç" "¹î" "Âò" "¹¦"])
	(24 . ; g
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "°ð" "Ãë" "·ã" "»õ" "µ´"
	     nil nil nil nil nil   "°á" "»¶" "Êá" "ÁÜ" "Áë"
	     nil nil nil nil nil   "Êè" "µÓ" "Â£" "À»" "È³"])
	(28 ; l
	 (13 (16 . "¥ô")) ; r u
	 (16 . "¤¥") ; u
	 (17 . "¤£") ; i
	 (25 . "¤§") ; h
	 (26 . "¤©") ; j
	 (27 . "¤¡")) ; k
	(33 . ; v
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "±î" "Ë×" "Èä" "ËÃ" "¾í"
	     nil nil nil nil nil   "ÈÂ" "»ã" "½Ö" "¾¬" "´½"
	     nil nil nil nil nil   "¸Ñ" "Íô" "¾÷" "º¯" "Æä"])
	(34 . ; b
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "µÝ" "Îõ" "Éú" "·ß" "À¨"
	     nil nil nil nil nil   "Ìæ" "Àø" "Â¡" "¶î" "´¸"
	     nil nil nil nil nil   "Äª" "¶©" "Àå" "Î¼" "Äü"]))

    (14 ; t
	(14 . ; t
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "¶²" "¿Â" "Ï¯" "´û" "·Æ"
	     nil nil nil nil nil   "ÁØ" "Î±" "°¡" "È¼" "¶¸"
	     nil nil nil nil nil   "»ø" "Å«" "¶à" "¶§" "´à"])
	(24 . ; g
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "°Ù" "·§" "Èã" "¾Í" "´·"
	     nil nil nil nil nil   "ÈË" "´ê" "ËÇ" "µÕ" "³Ý"
	     nil nil nil nil nil   "ÁÆ" "°Ö" "³Í" "ËÍ" "²é"])
	(28 ; l
	 (14 ; t
	  (16 . "¤×") ; u
	  (17 . "¤Ô") ; i
	  (25 . "¤Ú") ; h
	  (26 . "¤Ý") ; j
	  (27 . "¤Ñ")) ; k
	 (16 . "¤Ö") ; u
	 (17 . "¤Ó") ; i
	 (25 . "¤Ù") ; h
	 (26 . "¤Ü") ; j
	 (27 . "¤Ð")) ; k
	(33 . ; v
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "ÀÏ" "Îö" "Çõ" "³ç" "±Ö"
	     nil nil nil nil nil   "ÌÂ" "¶×" "Ë¥" "Íë" "³í"
	     nil nil nil nil nil   "Ä±" "¶¬" "°ù" "ÎÊ" "´Ü"])
	(34 . ; b
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "Æ÷" "ÉÐ" "È¦" "²É" "´â"
	     nil nil nil nil nil   "ÅÇ" "Ë³" "½Ú" "À«" "Âþ"
	     nil nil nil nil nil   "Ì½" "Àù" "Çð" "Ç×" "´æ"]))

    ;;
    (15 ; y
	(15 . ; y
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "¸¼" "É¹" "¿¢" "»Þ" "°Å"	nil nil nil nil nil
	     "ÃÕ" "Æ«" "·²" "ÈÝ" "²Ù"	nil nil nil nil nil
	     "µÌ" "µ¼" "ÂÌ" "ÄÃ" "Éæ"	nil nil nil nil nil])
	(25 . ; h
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "È¡" "¼ì" "¹°" "Áã" "Ëà"	nil nil nil nil nil
	     "½ß" "Î¸" "µª" "ÎØ" "Ï·"	nil nil nil nil nil
	     "¿×" "°Î" "³À" "¿ë" "Ï§"	nil nil nil nil nil])
	(35 . ; n
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "ÏÄ" "±Ú" "¶ù" "Èâ" "Á·"	nil nil nil nil nil
	     "Ãä" "Ì»" "ÇÃ" "ÇÙ" "´º"	nil nil nil nil nil
	     "µø" "²å" "½§" "°à" "²ª"	nil nil nil nil nil])
	(36 . ; m
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "¸Ô" "Íç" "´ý" "´Á" "»ç"	nil nil nil nil nil
	     "°Þ" "Â¯" "¾â" "Âë" "Êí"	nil nil nil nil nil
	     "¶Ü" "Îâ" "Ìì" "¶º" "ÄÕ"	nil nil nil nil nil]))

    (16 ; u
	(15 . ; y
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "´Õ" "ÌÐ" "µî" "±×" "ÁÊ"	nil nil nil nil nil
	     "»¿" "Ë½" "¶ì" "»¡" "Â®"	nil nil nil nil nil
	     "Áî" "É±" "±ì" "¿µ" "ÀÎ"	nil nil nil nil nil])
	(25 . ; h
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "¹§" "½þ" "±ä" "É¸" "µì"	nil nil nil nil nil
	     "°¤" "Íð" "²ì" "½ã" "Æà"	nil nil nil nil nil
	     "À·" "Âó" "ÇÓ" "Ä¬" "¶Ã"	nil nil nil nil nil])
	(35 . ; n
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "±½" "½Û" "±ý" "Æ­" "ºé"	nil nil nil nil nil
	     "Åï" "´À" "¼ñ" "µã" "Ãû"	nil nil nil nil nil
	     "ºÀ" "Îç" "¶ó" "¶Æ" "Äð"	nil nil nil nil nil])
	(36 . ; m
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "¿Å" "¹£" "·÷" "¸¸" "²ê"	nil nil nil nil nil
	     "³³" "Ç¥" "Ë»" "Äû" "µ½"	nil nil nil nil nil
	     "¼¶" "Ë±" "¿î" "Íõ" "ÁÉ"	nil nil nil nil nil]))

    (17 ; i
	(15 . ; y
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "Î½" "Ê´" "°×" "Ä»" "Ëç"	nil nil nil nil nil
	     "²ß" "·´" "ÌÏ" "´õ" "Å¡"	nil nil nil nil nil
	     "µº" "Ä²" "¾É" "µò" "·ª"	nil nil nil nil nil])
	(25 . ; h
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "¿Ø" "Í¾" "É§" "²¡" "³Ë"	nil nil nil nil nil
	     "ºþ" "Ë¼" "ÆÍ" "Åü" "´³"	nil nil nil nil nil
	     "ÍÞ" "Äâ" "Æ¨" "º®" "²í"	nil nil nil nil nil])
	(35 . ; n
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "¼´" "¶¹" "½Æ" "Ë¤" "µñ"	nil nil nil nil nil
	     "ÇÚ" "ÄÀ" "Ä§" "Âá" "Ç¦"	nil nil nil nil nil
	     "ËÖ" "·Ô" "»Ç" "º¨" "½¹"	nil nil nil nil nil])
	(36 . ; m
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "¸²" "Áø" "¾×" "Ëå" "Åë"	nil nil nil nil nil
	     "´ö" "°²" "·Ë" "Äï" "´þ"	nil nil nil nil nil
	     "±å" "°ë" "µ°" "Ëú" "ÆÝ"	nil nil nil nil nil]))

    (18 ; o
	(15 . ; y
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "¼ª" "Ãù" "¾Ï" "Äè" "½ä"	nil nil nil nil nil
	     "±Ô" "µ®" "É¼" "ÇØ" "»É"	nil nil nil nil nil
	     "²â" "»Â" "É¡" "ÌÇ" "»ì"	nil nil nil nil nil])
	(25 . ; h
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "´Ë" "Éõ" "¼ù" "Çß" "Î»"	nil nil nil nil nil
	     "ËÀ" "ÅÉ" "¸ß" "Îµ" "Ì´"	nil nil nil nil nil
	     "Ä¤" "³¶" "Â¹" "À¬" "ÁÎ"	nil nil nil nil nil])
	(35 . ; n
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "¹¢" "´ò" "Ë¢" "·¢" "³Ô"	nil nil nil nil nil
	     "ÁÚ" "ÂÄ" "ÍÀ" "ËÆ" "Âú"	nil nil nil nil nil
	     "»¹" "·Ø" "Å£" "ÌÍ" "Çó"	nil nil nil nil nil])
	(36 . ; m
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "Êï" "ÅÒ" "´ô" "±ï" "¼Ø"	nil nil nil nil nil
	     "½³" "¼¾" "Ëá" "½À" "Éå"	nil nil nil nil nil
	     "Éª" "Ï¼" "Êò" "ºû" "Êã"	nil nil nil nil nil]))

    (19 ; p
	(15 . ; y
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "Ëµ" "²æ" "Ä¶" "Áâ" "»ô"	nil nil nil nil nil
	     "½á" "ÍÄ" "ÅÓ" "¾Â" "Â«"	nil nil nil nil nil
	     "ÁÞ" "³´" "Ã¢" "¼À" "²û"	nil nil nil nil nil])
	(25 . ; h
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "ÂÅ" "Â¥" "Á¯" "Åß" "Íµ"	nil nil nil nil nil
	     "¿¨" "Á¼" "Ãø" "¸¨" "Ê£"	nil nil nil nil nil
	     "²ý" "Ê¾" "´×" "Ë¸" "´ù"	nil nil nil nil nil])
	(35 . ; n
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "°»" "·Ö" "ºø" "ÈÁ" "»µ"	nil nil nil nil nil
	     "Íü" "µ²" "´¾" "¼Ù" "Ë¶"	nil nil nil nil nil
	     "ÂÏ" "´»" "Ã®" "ÎÇ" "É¨"	nil nil nil nil nil])
	(36 . ; m
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "¼¦" "Êþ" "Áó" "Æû" "»è"	nil nil nil nil nil
	     "¿Õ" "°í" "²Þ" "ÄÛ" "Á¸"	nil nil nil nil nil
	     "Ëó" "ÃÁ" "µâ" "²ä" "ËÊ"	nil nil nil nil nil]))

    ;;
    (20 ; a
	(14 . ; t
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "Æì" "³§" "Âì" "²ø" "·Ü"
	     nil nil nil nil nil   "Íã" "ÀÞ" "Îø" "¾È" "»Ý"
	     nil nil nil nil nil   "¿°" "Çù" "Áµ" "È·" "½å"])
	(24 . ; g
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "Ç¼" "ÄË" "µí" "Ìï" "Åº"
	     nil nil nil nil nil   "ÅÝ" "°Ý" "³È" "¶Ø" "Ãî"
	     nil nil nil nil nil   "½Ê" "ÀÉ" "Ã¥" "¼¢" "¶ò"])
	(28 (27 . "¤î")) ; l k
	(33 . ; v
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "°Ó" "»ò" "´§" "¼ä" "Æï"
	     nil nil nil nil nil   "³ê" "Ã¯" "Êú" "½¦" "³¸"
	     nil nil nil nil nil   "É¿" "²ç" "Æõ" "Íì" "µÀ"])
	(34 . ; b
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "½â" "Ç¡" "Âµ" "¶Ò" "½í"
	     nil nil nil nil nil   "µ³" "ÌÔ" "ÊÀ" "¿ê" "Ê¯"
	     nil nil nil nil nil   "ºô" "Ìí" "¾¿" "±Ã" "³ß"]))

    (21 ; s
	(14 . ; t
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "Ãµ" "Ìð" "¼¯" "¾Ç" "Éè"
	     nil nil nil nil nil   "Àê" "»ï" "ÎÅ" "ÌÃ" "Åà"
	     nil nil nil nil nil   "¾î" "º¿" "¹Ù" "·°" "º¦"])
	(24 . ; g
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "¶¿" "½ç" "¿¥" "¾µ" "µÑ"
	     nil nil nil nil nil   "¾Ð" "È×" "¹ß" "Äì" "ºÚ"
	     nil nil nil nil nil   "Çþ" "ÇÐ" "Ïª" "ÃÅ" "ÎÑ"])
	(28 ; l
	 (16 . "¤º") ; u
	 (17 . "¤¸") ; i
	 (25 . "¤¼") ; h
	 (26 . "¤¾") ; j
	 (27 . "¤¶")) ; k
	(33 . ; v
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "Æð" "µ¾" "¾Ñ" "Îî" "Âõ"
	     nil nil nil nil nil   "¾Ü" "Ìá" "¹¹" "Á¦" "É¶"
	     nil nil nil nil nil   "Ï³" "´Ù" "²Í" "Æë" "ÏË"])
	(34 . ; b
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "Ç­" "¹Ì" "¹Ú" "¾ê" "¹²"
	     nil nil nil nil nil   "¹Æ" "¾´" "½Ï" "¾ô" "¿É"
	     nil nil nil nil nil   "Æú" "¶Õ" "ÈÒ" "ÂÃ" "±â"]))

    (22 ; d
	(14 . ; t
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "´¿" "Éû" "¸Ç" "ÌÊ" "ÊÙ"
	     nil nil nil nil nil   "·Ã" "´ß" "µÒ" "µù" "Ã¦"
	     nil nil nil nil nil   "ÌÖ" "Èª" "Èé" "½®" "Ãì"])
	(24 . ; g
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "±ü" "¼þ" "²Æ" "°è" "È¨"
	     nil nil nil nil nil   "º§" "´´" "¶¡" "Àá" "°Û"
	     nil nil nil nil nil   "È¢" "Â¸" "Æý" "Ë´" "¹µ"])
	(28 ; l
	 (16 . "¤Ã") ; u
	 (17 . "¤Â") ; i
	 (22 (16 . "¤Å")) ; d u
	 (25 . "¤Ç") ; h
	 (26 . "¤É") ; j
	 (27 . "¤À")) ; k
	(33 . ; v
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "Ã¤" "Î´" "Íê" "·¬" "¸ç"
	     nil nil nil nil nil   "Ãó" "Îé" "ÇÜ" "·»" "ÎÒ"
	     nil nil nil nil nil   "ÀÝ" "°¸" "¸Û" "¹¡" "¶ï"])
	(34 . ; b
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "Ì¡" "ÎÐ" "Ç÷" "Ë§" "Á¾"
	     nil nil nil nil nil   "·º" "´ø" "±Æ" "¹ï" "º÷"
	     nil nil nil nil nil   "°Ø" "µ¶" "¾£" "¾Ì" "±¬"]))

    (23 ; f
	(14 . ; t
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "ºÅ" "Éý" "ËÙ" "Â§" "½¼"
	     nil nil nil nil nil   "¶Ñ" "¾ù" "½ü" "ºù" "¾§"
	     nil nil nil nil nil   "¸×" "Ï²" "ÌÄ" "Ì¯" "¿è"])
	(24 . ; g
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "Ëô" "¹Ó" "±ç" "·¯" "¿Ì"
	     nil nil nil nil nil   "Àä" "¼Ë" "°µ" "¾Æ" "ÅØ"
	     nil nil nil nil nil   "³û" "²Û" "´Ç" "Å¨" "±ô"])
	(33 . ; v
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "ÁÄ" "±è" "Êö" "·®" "¶®"
	     nil nil nil nil nil   "Å¦" "¸¤" "³ù" "±¢" "Åû"
	     nil nil nil nil nil   "ÈÑ" "Å³" "µç" "¹¿" "²²"])
	(34 . ; b
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "·É" "ÏÓ" "Ì¼" "ÃÃ" "Æ¶"
	     nil nil nil nil nil   "¹¨" "·¹" "Íß" "¸í" "Äî"
	     nil nil nil nil nil   "¼Ü" "Î÷" "Åá" "»ó" "Ì÷"]))

    (24 ; g
	(14 . ; t
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "¼û" "ÃÙ" "ÁÏ" "ÍÙ" "½ï"
	     nil nil nil nil nil   "Îó" "µû" "Åµ" "Àë" "ÆÇ"
	     nil nil nil nil nil   "ÀË" "ÌÛ" "Èù" "µ§" "Á«"])
	(24 . ; g
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "É®" "¾û" "ÌÁ" "Ëë" "Åô"
	     nil nil nil nil nil   "¾ã" "À¥" "Á¡" "°Ü" "º¤"
	     nil nil nil nil nil   "Ëý" "¸¹" "¹ø" "ËÞ" "Ä¨"])
	(33 . ; v
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "Ä¦" "Ëâ" "½Ë" "·È" "Èå"
	     nil nil nil nil nil   "Ç»" "ÃÈ" "¼î" "ÇÕ" "¶«"
	     nil nil nil nil nil   "²³" "Îß" "¾½" "ºË" "Ìþ"])
	(34 . ; b
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "Åâ" "ÏÆ" "³­" "ÂÕ" "²§"
	     nil nil nil nil nil   "³Þ" "Æ¼" "¶»" "¶¼" "Âù"
	     nil nil nil nil nil   "Éç" "¹Â" "ÀØ" "¿Û" "²à"]))

    ;;
    (25 ; h
	(15 . ; y
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "É¤" "¹Ä" "¸Ð" "Àç" "ÉÁ"	nil nil nil nil nil
	     "·Ç" "Çö" "Åò" "¸Æ" "Íí"	nil nil nil nil nil
	     "Åõ" "°¥" "¾ö" "Åí" "°½"	nil nil nil nil nil])
	(25 . ; h
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "¿²" "½¢" "¿Ü" "Áà" "¾·"	nil nil nil nil nil
	     "·ó" "ÈÓ" "ÀÕ" "°Í" "Åð"	nil nil nil nil nil
	     "ÂÛ" "³á" "»é" "¶À" "Ãê"	nil nil nil nil nil])
	(35 . ; n
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "¶Ô" "Îæ" "½Ä" "ÎÃ" "Ê³"	nil nil nil nil nil
	     "²¢" "³Ö" "´²" "Ì²" "¾©"	nil nil nil nil nil
	     "º·" "¸û" "¼¸" "º«" "Éù"	nil nil nil nil nil])
	(36 . ; m
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "¼Ð" "±ê" "Ã¸" "²Â" "Ä©"	nil nil nil nil nil
	     "±ñ" "ÍÈ" "´Ô" "·ø" "Çº"	nil nil nil nil nil
	     "ÍÖ" "¹é" "Ïµ" "½¥" "¾ü"	nil nil nil nil nil]))

    (26 ; j
	(15 . ; y
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "½Ü" "¾þ" "ºÎ" "Î¥" "¸ù"	nil nil nil nil nil
	     "³Ù" "ÉÃ" "Æ®" "µ¿" "Ê©"	nil nil nil nil nil
	     "¶Ä" "ÅÜ" "¹à" "¸ã" "´ì"	nil nil nil nil nil])
	(25 . ; h
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "°·" "Éã" "ÉÛ" "²¹" "·ì"	nil nil nil nil nil
	     "°ø" "¾Ý" "¹¯" "ËÂ" "¼±"	nil nil nil nil nil
	     "Âº" "»ê" "µ·" "°û" "Éê"	nil nil nil nil nil])
	(35 . ; n
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "²±" "Íñ" "Áû" "È©" "Ãß"	nil nil nil nil nil
	     "Ä³" "Êô" "Êë" "µµ" "³¨"	nil nil nil nil nil
	     "ÍÚ" "ÈÚ" "»®" "Ì·" "¾é"	nil nil nil nil nil])
	(36 . ; m
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "Ëí" "½î" "¸Ø" "¾Ø" "½Í"	nil nil nil nil nil
	     "Æ¹" "Âî" "Èá" "¸¥" "Æ¸"	nil nil nil nil nil
	     "ºó" "ºÈ" "µÔ" "²¥" "ÂÆ"	nil nil nil nil nil]))

    (27 ; k
	(15 . ; y
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "³ã" "Çî" "¶­" "¼Ú" "Í¼"	nil nil nil nil nil
	     "¼÷" "¹¶" "Ë®" "½°" "»Ñ"	nil nil nil nil nil
	     "³Ó" "À÷" "¾Ë" "¿Ã" "Èè"	nil nil nil nil nil])
	(25 . ; h
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "¹ë" "ÍØ" "Éñ" "Ì©" "±ó"	nil nil nil nil nil
	     "¶Ú" "·Ï" "¿Ë" "ÅÌ" "´î"	nil nil nil nil nil
	     "Îí" "Ãè" "µÊ" "²¤" "Ã×"	nil nil nil nil nil])
	(35 . ; n
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "²Å" "²¾" "¼­" "ËÒ" "´«"	nil nil nil nil nil
	     "¾²" "Éâ" "ÊÒ" "¼å" "Í¦"	nil nil nil nil nil
	     "³¯" "Ê°" "Îå" "»´" "·¨"	nil nil nil nil nil])
	(36 . ; m
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "ÃÂ" "½ø" "Èï" "½Ì" "¿á"	nil nil nil nil nil
	     "¿û" "ºÜ" "´í" "¸ð" "ÂØ"	nil nil nil nil nil
	     "°«" "²¶" "ÎÞ" "°®" "Íö"	nil nil nil nil nil]))

    (28 ; l
	(15 . ; y
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "Àð" "ÆÏ" "Ê»" "¼¹" "µÆ"	nil nil nil nil nil
	     "¹ö" "Áü" "»À" "»¥" "Ã°"	nil nil nil nil nil
	     "µØ" "±÷" "½ë" "Å¥" "±Ù"	nil nil nil nil nil])
	(25 . ; h
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "´ã" "ºÒ" "ÇË" "ÊÞ" "Æ¦"	nil nil nil nil nil
	     "ÏÑ" "½ý" "Äù" "´¹" "ÈÈ"	nil nil nil nil nil
	     "Éë" "ÎÈ" "¼Õ" "·ê" "»Ð"	nil nil nil nil nil])
	(35 . ; n
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "µ¸" "¶þ" "ÊÉ" "¿æ" "Ï¡"	nil nil nil nil nil
	     "ÍÇ" "Í«" "Î§" "½±" "Äà"	nil nil nil nil nil
	     "¸´" "ÍÅ" "Ãï" "¶ß" "ÇÆ"	nil nil nil nil nil])
	(36 . ; m
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "ÎÄ" "Ë¹" "´Ó" "Íò" "Äç"	nil nil nil nil nil
	     "Ç¢" "º©" "Ìö" "·ç" "ÆÊ"	nil nil nil nil nil
	     "Ææ" "¼ô" "¾Ù" "ÁÅ" "½Þ"	nil nil nil nil nil]))

    (29 ; ;
	(15 . ; y
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "¿¬" "ÉÒ" "²·" "¶ð" "Æá"	nil nil nil nil nil
	     "ËÁ" "µ±" "ÊÁ" "¹Ã" "Èò"	nil nil nil nil nil
	     "³º" "Èê" "ÀÆ" "ºÏ" "ÊÐ"	nil nil nil nil nil])
	(25 . ; h
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "ÍÒ" "Áò" "Ãé" "°ä" "ÁÃ"	nil nil nil nil nil
	     "ÍÂ" "Ìø" "ºÄ" "µ¥" "±ö"	nil nil nil nil nil
	     "Á²" "¿Ý" "¹Å" "ÆÚ" "¾æ"	nil nil nil nil nil])
	(35 . ; n
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "³¾" "Çå" "¿ì" "º¶" "Ê¨"	nil nil nil nil nil
	     "¿¡" "ËÄ" "Ê¶" "´¨" "´¤"	nil nil nil nil nil
	     "Á§" "·ä" "³Å" "ÆÒ" "ºæ"	nil nil nil nil nil])
	(36 . ; m
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "Çë" "¼ß" "Îô" "Ê®" "ÌÕ"	nil nil nil nil nil
	     "°¯" "Æ·" "½Ã" "Î¯" "¿»"	nil nil nil nil nil
	     "ºü" "°§" "È¥" "°¶" "³Ç"	nil nil nil nil nil]))
    ;;
    (30 ; z
	(14 . ; t
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "°ç" "¿ñ" "µ¹" "À¦" "Çü"
	     nil nil nil nil nil   "Ä¼" "Áú" "Áå" "Ç´" "³ó"
	     nil nil nil nil nil   "³õ" "¶æ" "Àí" "³ý" "Áæ"])
	(24 . ; g
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "ÇÝ" "¹×" "¾ú" "¾ç" "¾Û"
	     nil nil nil nil nil   "¿ý" "´ª" "µ÷" "¼§" "Çæ"
	     nil nil nil nil nil   "³©" "Í¤" "¾á" "»¢" "Èõ"])
	(33 . ; v
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "ºÖ" "ÅÎ" "·å" "É¦" "ÌØ"
	     nil nil nil nil nil   "º­" "Ì­" "¼®" "³®" "´Ã"
	     nil nil nil nil nil   "ÁÌ" "Âû" "Ìå" "Éó" "¸Ó"])
	(34 . ; b
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "¸¿" "ÎÆ" "Ëê" "½Ý" "ÄÇ"
	     nil nil nil nil nil   "¶ú" "Ä÷" "±²" "ÇÊ" "Ãð"
	     nil nil nil nil nil   "±á" "ÅË" "½¶" "ÀÃ" "ÊÜ"]))

    (31 ; x
	(14 . ; t
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "µà" "³¼" "ÀÀ" "·ð" "·¦"
	     nil nil nil nil nil   "¹ó" "±£" "¸­" "ÃÑ" "ÀÂ"
	     nil nil nil nil nil   "µÂ" "Á¢" "ÄÞ" "ÀÔ" "µ©"])
	(24 . ; g
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "´®" "´Î" "Ãò" "ÉÓ" "Ç«"
	     nil nil nil nil nil   "ÄÆ" "Íâ" "Ë·" "¾¤" "¾Ó"
	     nil nil nil nil nil   "ÇÄ" "¶Ö" "Äæ" "¶ü" "Áä"])
	(33 . ; v
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "°Ç" "ÄÖ" "¸ä" "°ê" "È¤"
	     nil nil nil nil nil   "±Ü" "Éí" "·Â" "´­" "Êð"
	     nil nil nil nil nil   "Ê÷" "´ç" "Á®" "ÂÜ" "¿Ñ"])
	(34 . ; b
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "³Á" "Åã" "±Í" "°À" "ÁÍ"
	     nil nil nil nil nil   "¼Ó" "º»" "¶Ï" "Ëò" "ÂÁ"
	     nil nil nil nil nil   "¾Ö" "Ë©" "Á¹" "´£" "¶ý"]))

    (32 ; c
	(14 . ; t
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "·ü" "·¡" "Î²" "·æ" "¿â"
	     nil nil nil nil nil   "ÊÄ" "´ú" "ÇÑ" "·õ" "Ê¤"
	     nil nil nil nil nil   "É³" "»¤" "Íï" "·Å" "¸Ò"])
	(24 . ; g
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "¹À" "½Ó" "±À" "·¼" "ÅÍ"
	     nil nil nil nil nil   "Á¬" "Î¤" "¾Ä" "Å°" "´Ï"
	     nil nil nil nil nil   "Æ¥" "Æ©" "Îï" "Ì×" "±ú"])
	(33 . ; v
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "¸°" "¹Ø" "Çì" "°©" "¼×"
	     nil nil nil nil nil   "³ø" "¸É" "¾Î" "ÍÏ" "¼ð"
	     nil nil nil nil nil   "ºç" "ÌÑ" "¼Û" "¾Å" "²ë"])
	(34 . ; b
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "Æß" "Æ½" "Å½" "´Ì" "Àò"
	     nil nil nil nil nil   "µõ" "Å±" "ÊÇ" "¿ç" "·Î"
	     nil nil nil nil nil   "±Þ" "ÎË" "Áº" "Íä" "Çâ"]))

    (33 ; v
	(14 . ; t
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "½­" "ºÌ" "¸¬" "Ï£" "½Ç"
	     nil nil nil nil nil   "¼Ñ" "Çñ" "´Å" "À¡" "¿Ó"
	     nil nil nil nil nil   "Ä¿" "¶´" "¹·" "ÎÜ" "Çô"])
	(24 . ; g
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "±Õ" "·À" "Ìé" "¹±" "ÇÞ"
	     nil nil nil nil nil   "Ä¢" "ÁÝ" "´Ê" "±ø" "³µ"
	     nil nil nil nil nil   "¼¿" "¼ü" "·ý" "Éî" "Í´"])
	(33 . ; v
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "·Ì" "¿þ" "Ê×" "Èì" "ÂÉ"
	     nil nil nil nil nil   "¶Å" "Ìñ" "Áþ" "°°" "°Ã"
	     nil nil nil nil nil   "Éï" "ÇÎ" "³¡" "¶ª" "Á½"])
	(34 . ; b
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "¾ß" "¹ä" "ÇÅ" "±³" "ÉÚ"
	     nil nil nil nil nil   "³Ï" "½ù" "Íû" "¿«" "Å÷"
	     nil nil nil nil nil   "ºÓ" "Á¿" "Ï´" "ÄÐ" "´È"]))

    (34 ; b
	(14 . ; t
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "Èý" "Èç" "´¢" "ÀÍ" "Æò"
	     nil nil nil nil nil   "µê" "»Ì" "¼ë" "¿ò" "Ã³"
	     nil nil nil nil nil   "Éà" "ÄØ" "ÇÖ" "ÆÅ" "¿à"])
	(24 . ; g
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "ÅÛ" "²¸" "Äñ" "¿Ô" "±Ó"
	     nil nil nil nil nil   "¶Í" "Í£" "Åþ" "ÁÓ" "ÁÂ"
	     nil nil nil nil nil   "Éö" "¹É" "¿ã" "ËÚ" "ÎÕ"])
	(33 . ; v
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "¾¶" "ÈÍ" "µý" "Ìã" "Åè"
	     nil nil nil nil nil   "³¥" "½·" "¼í" "Îì" "ÆÌ"
	     nil nil nil nil nil   "ÍÝ" "ºë" "Ìª" "ÄÊ" "Äõ"])
	(34 . ; b
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "¼Æ" "Èí" "µ£" "±Ý" "¸Ã"
	     nil nil nil nil nil   "¿¸" "°´" "ÇÌ" "Ëû" "Ì¶"
	     nil nil nil nil nil   "Ç¿" "²À" "Ìà" "Ã¿" "¶Â"]))

    ;;
    (35 ; n
	(15 . ; y
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "±±" "±Ì" "Í±" "¸Ý" "¿õ"	nil nil nil nil nil
	     "Ãö" "¶ã" "Äú" "¶Ç" "Â±"	nil nil nil nil nil
	     "ºÉ" "Æ´" "ÄÑ" "±È" "³ª"	nil nil nil nil nil])
	(25 . ; h
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "ºµ" "ÍÊ" "»£" "ÊÅ" "Â¶"	nil nil nil nil nil
	     "¶ö" "ÁË" "±Ë" "ÀÒ" "ËÐ"	nil nil nil nil nil
	     "·µ" "´á" "¹Ê" "ºã" "Ê½"	nil nil nil nil nil])
	(35 . ; n
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "ÌÞ" "ÆÜ" "µü" "ÉË" "°Ú"	nil nil nil nil nil
	     "·Ó" "Ì¨" "¶Þ" "°¼" "¿ð"	nil nil nil nil nil
	     "Ë²" "Ã¨" "°î" "Á£" "ÏÉ"	nil nil nil nil nil])
	(36 . ; m
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "¾ª" "Äß" "Áç" "»½" "½«"	nil nil nil nil nil
	     "ÆÓ" "Í¯" "º¾" "Ì®" "ÁÀ"	nil nil nil nil nil
	     "ÄÙ" "Ã·" "±ª" "ºú" "¾ó"	nil nil nil nil nil]))

    (36 ; m
	(15 . ; y
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "Ä·" "¹´" "¸¯" "Íó" "»§"	nil nil nil nil nil
	     "Ï¶" "ÁÔ" "´¥" "Ãá" "Äº"	nil nil nil nil nil
	     "º¡" "Îè" "Ã¶" "¶Ê" "Äò"	nil nil nil nil nil])
	(25 . ; h
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "¾¸" "¼Î" "¾»" "·±" "Êæ"	nil nil nil nil nil
	     "¿Ï" "¸â" "Êñ" "Â©" "Æ§"	nil nil nil nil nil
	     "Íå" "È²" "Î¡" "ËÝ" "ÎÍ"	nil nil nil nil nil])
	(35 . ; n
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "Éì" "Ì°" "Á©" "¶é" "¼Ï"	nil nil nil nil nil
	     "ÆÖ" "Ëì" "ÉÄ" "Ãª" "±ð"	nil nil nil nil nil
	     "´¼" "Âü" "Èü" "³¿" "Êµ"	nil nil nil nil nil])
	(36 . ; m
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "Äö" "ÉÑ" "ÊÊ" "³Ã" "¸ñ"	nil nil nil nil nil
	     "¼Ä" "°¾" "ËÑ" "Çû" "Â×"	nil nil nil nil nil
	     "¸§" "ÃÉ" "Ìß" "Çè" "»Ó"	nil nil nil nil nil]))

    (37 ; ,
	(15 . ; y
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "Êé" "²µ" "Ì¸" "¸ª" "¸Ü"	nil nil nil nil nil
	     "¿Ò" "¿ï" "¶Û" "¿Î" "µÛ"	nil nil nil nil nil
	     "ÏÍ" "¸ô" "ºÕ" "¸à" "¹Î"	nil nil nil nil nil])
	(25 . ; h
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "Ï¤" "ÅÂ" "Äá" "Ç³" "¿Ç"	nil nil nil nil nil
	     "ÉÞ" "°Ï" "Ã¼" "µ¨" "²®"	nil nil nil nil nil
	     "³Ì" "·ö" "²õ" "Çï" "ÈÊ"	nil nil nil nil nil])
	(35 . ; n
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "Áï" "²Ô" "²Ë" "¶³" "Êø"	nil nil nil nil nil
	     "³å" "ºÁ" "Í¡" "¿£" "Î³"	nil nil nil nil nil
	     "»¾" "°ñ" "Áô" "Ï½" "´¯"	nil nil nil nil nil])
	(36 . ; m
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "¼à" "²ã" "Ê¢" "¹Õ" "ÀÛ"	nil nil nil nil nil
	     "Ëß" "È¶" "¿ø" "·«" "¸ë"	nil nil nil nil nil
	     "¼µ" "¿Á" "½Î" "ÌÒ" "ÊÎ"	nil nil nil nil nil]))

    (38 ; .
	(15 . ; y
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "Ëª" "²ù" "º°" "Íª" "Àà"	nil nil nil nil nil
	     "Âä" "²Ò" "±ã" "Ä¯" "Ãà"	nil nil nil nil nil
	     "ÇÁ" "É°" "´Ú" "·Û" "¿¤"	nil nil nil nil nil])
	(25 . ; h
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "ÃÚ" "Îº" "ÄÁ" "Îñ" "¾ø"	nil nil nil nil nil
	     "Íù" "¹ò" "Èî" "ºØ" "ÄÄ"	nil nil nil nil nil
	     "Á¨" "¿Ä" "´è" "¶ë" "È­"	nil nil nil nil nil])
	(35 . ; n
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "Ë£" "ÂÝ" "³é" "Æ×" "ÈÆ"	nil nil nil nil nil
	     "¸Á" "Ëõ" "ÎÔ" "±¯" "½Ô"	nil nil nil nil nil
	     "ÅÆ" "ÃÐ" "¶û" "Ëø" "°¹"	nil nil nil nil nil])
	(36 . ; m
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "²´" "°ô" "±¾" "²Õ" "ËÕ"	nil nil nil nil nil
	     "Ë¨" "°þ" "½ö" "ÈÞ" "Ìû"	nil nil nil nil nil
	     "Äµ" "²×" "Ä¡" "ÀÌ" "Ìù"	nil nil nil nil nil]))

    (39 ; /
	(15 . ; y
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "½ì" "¹¸" "Äþ" "¸Ì" "ÈÜ"	nil nil nil nil nil
	     "Íþ" "²ô" "°ï" "»ü" "¸Ï"	nil nil nil nil nil
	     "½ú" "ÈÃ" "¿ª" "È«" "Ã¬"	nil nil nil nil nil])
	(25 . ; h
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "³ì" "Äé" "Àû" "Äå" "ÆÆ"	nil nil nil nil nil
	     "°¿" "ËÅ" "¸Ê" "Ì¦" "¹ª"	nil nil nil nil nil
	     "Âé" "Ä¸" "³î" "ÆØ" "°ö"	nil nil nil nil nil])
	(35 . ; n
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "¼Ê" "¾«" "Âö" "ÏÈ" "ÌÆ"	nil nil nil nil nil
	     "±¨" "Î¶" "²î" "¼¤" "Ë«"	nil nil nil nil nil
	     "³ð" "³ë" "ÏÐ" "³·" "¹å"	nil nil nil nil nil])
	(36 . ; m
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "»â" "Å®" "µ¦" "¿Ð" "±»"	nil nil nil nil nil
	     "»Æ" "ÏÁ" "½Ù" "Ìè" "¸È"	nil nil nil nil nil
	     "ÀÈ" "Ì§" "Çí" "¸ï" "Î¿"	nil nil nil nil nil]))))

(setq tcode-special-commands-alist
  '(((0 0) . (lambda () (tcode-show-tables nil nil))) ; 11 : LLÉ½¤ÎÉ½¼¨
    ((0 9) . (lambda () (tcode-show-tables nil t))) ; 10 : LRÉ½¤ÎÉ½¼¨
    ((9 0) . (lambda () (tcode-show-tables t nil))) ; 01 : RLÉ½¤ÎÉ½¼¨
    ((9 9) . (lambda () (tcode-show-tables t t))) ; 00 : RRÉ½¤ÎÉ½¼¨
    ((1 1) . tcode-start-jiscode)	; 22 : JIS ¥³¡¼¥ÉÉ½ÆþÎÏ
    ((2 2) . tcode-toggle-alnum-mode) ; 33 : 1-2¥Ð¥¤¥ÈÀÚ¤ê´¹¤¨
    ((2 1) . tcode-switch-variable) ; 32 : ¶çÆÉÅÀ¤Î¥È¥°¥ë
    ((3 3) . (lambda ()
	       (tcode-display-stroke-sequence tcode-last-help-char-list)))
					; 44 : ¥Ø¥ë¥×
    ((4 4) . (lambda () (tcode-query-stroke (point))))
					; 55 : ¥Ø¥ë¥×
    ((6 6) . tcode-bushu-begin-alternate-conversion)
					; 77 : postfix Éô¼óÊÑ´¹
    ((7 7) . (lambda () (tcode-transpose-strokes nil)))
					; 88 : transpose-strokes
    ((8 8) . tcode-clear)
					; 99 : Éô¼ó¹çÀ®ÊÑ´¹¡¦¸ò¤¼½ñ¤­ÊÑ´¹¤Ê¤É¤Î
					; ¥­¥ã¥ó¥»¥ë
    ((20 28 20) . tcode-bushu-begin-conversion))) ; ala : Éô¼ó¹çÀ®ÊÑ´¹¤Î³«»Ï


(setq tcode-mode-help-string "\
TUT¥³¡¼¥É¥â¡¼¥ÉÃæ¤Î¥­¡¼Áàºî¤Ï¼¡¤Î¤È¤ª¤ê¡£
   ala : Éô¼ó¹çÀ®ÊÑ´¹¥â¡¼¥É¤ËÆþ¤ë¡£ala¤òÂÇ¤ÁÂ³¤±¤ë¤ÈºÆµ¢Åª¤ËÉô¼ó¹çÀ®ÊÑ´¹¤ò
	¹Ô¤¦¤³¤È¤¬¤Ç¤­¤ë¡£
   alj : ¸ò¤¼½ñ¤­ÊÑ´¹¤ò¹Ô¤¦(see variable `tcode-use-prefix-mazegaki')¡£
   00, 01, 10, 11 : TUT¥³¡¼¥É¤Î2¥¹¥È¥í¡¼¥¯¤Î¥¹¥È¥í¡¼¥¯É½¤òÉ½¼¨¤¹¤ë¡£
			(0¤¬¡Ö±¦¡×¡¢1¤¬¡Öº¸¡×¤ò°ÕÌ£¤·¤Æ¤¤¤ë)
   22 : JIS ¥³¡¼¥É°ìÍ÷É½¤Ë¤è¤ëÆþÎÏ¡£
   32 : ¡¢¡£¤È, . ¤òÀÚ¤êÂØ¤¨¤ë¡£(see variable `tcode-switch-table-list')¡£
   33 : TUT¥³¡¼¥ÉÉ½¤Ë¤¢¤ë±Ñ¿ô»ú¡¦µ­¹æ¤ÎÊ¸»ú¥³¡¼¥É¤Î1¥Ð¥¤¥È¡¦2¥Ð¥¤¥ÈÀÚ¤êÂØ¤¨¡£
   44 : Ä¾Á°¤ËÉ½¼¨¤·¤¿ÂÇ¤ÁÊý¤òºÆÉ½¼¨¤¹¤ë¡£
   55 : ¥Ý¥¤¥ó¥È°ÌÃÖ¤Ë¤¢¤ëÊ¸»ú¤ÎÂÇ¤ÁÊý¤òÉ½¼¨¤¹¤ë¡£
   58 : ³èÍÑ¸ì¤òÍ¥Àè¤·¤Æ¸ò¤¼½ñ¤­ÊÑ´¹¤ò¹Ô¤¦¡£
   77 : ¥Ý¥¤¥ó¥ÈÁ°¤Ë¤¢¤ë2Ê¸»ú¤ÇÉô¼ó¹çÀ®ÊÑ´¹¤ò¹Ô¤¦¡£
   88 : ¥Ý¥¤¥ó¥È°ÌÃÖ¤Ë¤¢¤ëÊ¸»ú¤òµÕ¥¹¥È¥í¡¼¥¯²½¤¹¤ë(Îã: Ç¯->¤Î)¡£
	¹ÔËö¤Ç¤Ï¥Ý¥¤¥ó¥È¤ÎÄ¾Á°¤ÎÊ¸»ú¤òÊÑ´¹¤¹¤ë¡£
   99 : ¸ò¤¼½ñ¤­ÊÑ´¹¥â¡¼¥É¤äÉô¼óÊÑ´¹¥â¡¼¥É¤Ë¤¤¤¿»þ¤Ë¡¢
	¤½¤ì¤é¤òÁ´Éô¥­¥ã¥ó¥»¥ë¤¹¤ë¡£¤Þ¤¿¡¢¥Ø¥ë¥×¤ò¾Ã¤¹¡£
   [1-4]8, [2-5]9: Ê¸»ú¿ô¤ò»ØÄê¤·¤Æ¸ò¤¼½ñ¤­ÊÑ´¹¤ò¹Ô¤¦¡£
   \\[toggle-input-method] : TUT¥³¡¼¥É¥â¡¼¥É¤òÈ´¤±¤ë¡£

½é¤á¤Æµ¯Æ°¤µ¤ì¤¿»þ¤Ë¤Ï¡¤`tcode-ready-hook' ¤ò¼Â¹Ô¤¹¤ë¡£
¤Þ¤¿¡¢µ¯Æ°¤µ¤ì¤ëÅÙ¤Ë`tcode-toggle-hook'¤ò¼Â¹Ô¤¹¤ë¡£")

(defun tcode-make-special-for-tut (seq table)
  "TABLE ¤ò SEQ ¤Ë´ð¤Å¤­ `tcode-special-commands-alist' ÍÑ¤ËÊÑ´¹¤¹¤ë¡£"
  (cond ((consp table)
	 (if (integerp (car table))
	     (tcode-make-special-for-tut (append seq (car table)) (cdr table))
	   (let (list)
	     (while table
	       (let* ((elm (car table))
		      (ret (tcode-make-special-for-tut
			    (append seq (list (car elm)))
			    (cdr elm))))
		 (and ret
		      (setq list (append ret list)))
		 (setq table (cdr table))))
	     list)))
	((vectorp table)
	 (list (cons seq table)))
	((stringp table)
	 (list (cons seq table)))))

(setq tcode-special-commands-alist
      (nconc (tcode-make-special-for-tut nil tut-over-2-strokes-table)
	     tcode-special-commands-alist
	     '(((20 28 26) . tcode-mazegaki-begin-conversion)
				     ; alj: ¸ò¤¼½ñ¤­ÊÑ´¹
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
	       ((4 8) . (lambda () (tcode-mazegaki-convert 5 t))))))

(setq tcode-stroke-file-name (concat tcode-data-directory "tutcode.st"))

(setq eelll-text "EELLLTXT.tut")

;;; tutc-tbl.el ends here
