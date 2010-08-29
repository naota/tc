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
      tcode-alnum-2byte-tcode-mode-indicator "�� "
      tcode-hiragana-mode-indicator " ��"
      tcode-katakana-mode-indicator " ��")

;;  1  2  3  4	5    6	7  8  9	 0
;;  q  w  e  r	t    y	u  i  o	 p
;;  a  s  d  f	g    h	j  k  l	 ;
;;  z  x  c  v	b    n	m  ,  .	 /

;;  0  1  2  3	4    5	6  7  8	 9
;; 10 11 12 13 14   15 16 17 18 19
;; 20 21 22 23 24   25 26 27 28 29
;; 30 31 32 33 34   35 36 37 38 39

;;  ---> �����ܤΥ���
;; |
;; v �����ܤΥ���
; 1 2 3 4 5 6 7 8 9 0 q w e r t y u i o p a s d f g h j k l ; z x c v b n m , . /
(setq tcode-tbl [
nil nil nil nil nil nil nil nil nil nil ; 1-0
"������������������������ξ���춽�������װ����������Ϳ�������ź�ϲϱ�������" ;q
"������������������������������建̳��Ƭ����̵�Ƴ�ư����ں�΢û���ѷ޸�¢���۳�" ;w
"���������������������ĳ��ͥ�Ƕ�������Ŵŷ���ƴر����Ȭ���������Ψ��ɲļ���" ;e
"������������������������������Į������������ʿ���׻��Ÿ޾�����ͷ�ܵ���Ⱦ���е�˭" ;r
"���������������������������������¶����٢���������̾������ᢢ������������ʵ���" ;t
"�������������������������ҽйԢ���������������̢���������������̢���������" ;y
"����������������������य���ճ�ڳڱཪ�ܤ��Ĥ̤������ѳ��ƹ���Ĵ����ģǷι���" ;u
"����������������������ߤ�����̱ɽ�ײ�ǽ�𤷤��ˤ�Ϣ�ְʴ���������װ����Ǹ���͹" ;i
"���������������������򵤼Լ�Ū�屡�־��δ�ȯ�Ͱ���ͳ���������ɵ��ѿȻ��ܷ�ʵ�" ;o
"����������������������ͽ����ˡ�������ӷ�³���������ݰ鿧��ã�±��㴶��ô������" ;p
"��������������������ǰ�����ַ���������ڿ��ɶ�����ƣ�շ������ռ�����̤������ɩ��" ;a
"���������������������Ǽ��Ļ�������ɴ����ʡ¾��������ĸ徦��ñ���ɵ��´ī����" ;s
"��������������������������ʺ³�����ϻ����ͭ����������������ɾ�����ֵ޳��Իٿ�" ;d
"�����������������������������񹩻�����ľ����������ǯ��Ʊ��Ƥ�����ǲ��Ϻʸ����" ;f
"����������������������������������ʬΩ�������������߻ͽ��̶���������������¼����" ;g
"����������������������ᤱ���آ����������񤻤Ƥͤ좢��������Ƚ������¿����������" ;h
"����������������������⤳���۾���迴�Τ򤽤ȤΤ��ϸ����߹�������趯���;ʷ���" ;j
"����������������������ޤ����Ϻ��ٹ���ž�蘆���ʤ鲽�����򷸼㵭����������ϩ��Ư" ;k
"�������������������������������������������������ʪ���¦Ǥ���޼������԰��Լ�" ;l
"��������������������������Ƚ����ҷ��������Ĺ�����̺߼��ѵ�������������ݾ�¨��" ;;
"���������������������ֶ���Ǯ��������佣������������ͧ�����������ϸ���������ͻ��" ;z
"����������������������ˬ�����ǰ˽���ϫ��²��Ʋʼ���͸����к䴩�����������������" ;x
"�����������������������������ʲ��Ǻ徺�����ȴ�����ëƻ���������������ɸ���" ;c
"�����������������������������������������������������涶���������������ǹ����˴�" ;v
"���������������������������������ܿ�ͺ�բ������������ڿ�¤�ݢ������������ʽ�Φ" ;b
"��������������������ɬ������颢�������������ø���������������ǧ����â���������" ;n
"�������������������������̶�ȿ����������������Ţ�������������ή���Ƣ���������" ;m
"���������������������ȿ������������Ҵ���������Ź�˴�ʹ����㵯̿���ظ��𳲸Ĵ���" ;,
"�������������������������ظ�������ʧ���´���ǲ�����߿�����ȴƳ��Ͽĥ���̻����" ;.
"��������������������Ÿ����̣͢�����ູ����­��˾�������Ŭ�ް������������������" ;/
])

(setq tcode-non-2-stroke-char-list (list (tcode-string-to-char "��")
					 (tcode-string-to-char "��")))

(setq tcode-another-table
      [ nil  nil  nil  nil  nil	   nil	nil  nil  nil  nil
	"��" "��" "��" "��" "��"   "��" "��" "��" "��" "��"
	"��" "��" tcode-touten tcode-kuten "��"	  "��" "��" "��" "��" "��"
	"��" "��" "��" "��" "��"   "��" "��" "��" "��" "��"])

(defconst tut-over-2-strokes-table
  '((10 ; q
	(14 . ; t
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "��" "˺" "̥" "��" "ò"
	     nil nil nil nil nil   "��" "��" "��" "��" "��"
	     nil nil nil nil nil   "��" "��" "��" "˰" "��"])
	(24 . ; g
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "ů" "��" "��" "Ͷ" "��"
	     nil nil nil nil nil   "��" "��" "��" "��" "��"
	     nil nil nil nil nil   "��" "��" "��" "��" "Ȼ"])
	(28 ; l
	 (16 . "��") ; u
	 (26 . "��") ; j
	 (27 . "��")) ; k
	(33 . ; v
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "��" "��" "��" "��" "��"
	     nil nil nil nil nil   "��" "ʷ" "˦" "��" "��"
	     nil nil nil nil nil   "Ͼ" "��" "��" "��" "��"])
	(34 . ; b
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "��" "��" "��" "��" "��"
	     nil nil nil nil nil   "��" "��" "��" "��" "��"
	     nil nil nil nil nil   "��" "ɵ" "��" "þ" "Ϯ"]))

    (11 ; w
	(14 . ; t
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "��" "��" "��" "»" "��"
	     nil nil nil nil nil   "��" "��" "��" "��" "��"
	     nil nil nil nil nil   "��" "·" "��" "ϰ" "��"])
	(24 . ; g
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "��" "��" "��" "��" "ʥ"
	     nil nil nil nil nil   "��" "��" "��" "��" "��"
	     nil nil nil nil nil   "��" "ϭ" "��" "��" "��"])
	(33 . ; v
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "��" "ɺ" "��" "ƿ" "��"
	     nil nil nil nil nil   "��" "��" "��" "��" "ͩ"
	     nil nil nil nil nil   "��" "��" "Ǩ" "��" "��"])
	(34 . ; b
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "��" "��" "ũ" "��" "��"
	     nil nil nil nil nil   "˿" "��" "��" "��" "��"
	     nil nil nil nil nil   "á" "��" "��" "��" "��"]))

    (12 ; e
	(14 . ; t
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "��" "��" "��" "��" "��"
	     nil nil nil nil nil   "��" "��" "��" "°" "Ǿ"
	     nil nil nil nil nil   "��" "��" "ά" "��" "��"])
	(24 . ; g
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "ú" "��" "��" "��" "��"
	     nil nil nil nil nil   "��" "��" "��" "��" "��"
	     nil nil nil nil nil   "��" "��" "��" "��" "��"])
	(28 ; l
	 (12 (25 . "��")  ; e h
	     (27 . "��"))  ; e k
	 (16 . "��") ; u
	 (17 . "��") ; i
	 (25 . "��") ; h
	 (26 . "��") ; j
	 (27 . "��")) ; k
	(33 . ; v
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "��" "��" "��" "��" "��"
	     nil nil nil nil nil   "��" "İ" "ǵ" "��" "��"
	     nil nil nil nil nil   "��" "��" "��" "��" "��"])
	(34 . ; b
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "��" "��" "��" "��" "��"
	     nil nil nil nil nil   "��" "��" "��" "ȱ" "��"
	     nil nil nil nil nil   "��" "ʺ" "��" "��" "��"]))

    (13 ; r
	(14 . ; t
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "��" "��" "¬" "��" "��"
	     nil nil nil nil nil   "��" "��" "��" "��" "��"
	     nil nil nil nil nil   "��" "��" "��" "��" "��"])
	(24 . ; g
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "��" "��" "��" "��" "��"
	     nil nil nil nil nil   "��" "��" "��" "��" "��"
	     nil nil nil nil nil   "��" "��" "£" "��" "ȳ"])
	(28 ; l
	 (13 (16 . "��")) ; r u
	 (16 . "��") ; u
	 (17 . "��") ; i
	 (25 . "��") ; h
	 (26 . "��") ; j
	 (27 . "��")) ; k
	(33 . ; v
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "��" "��" "��" "��" "��"
	     nil nil nil nil nil   "��" "��" "��" "��" "��"
	     nil nil nil nil nil   "��" "��" "��" "��" "��"])
	(34 . ; b
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "��" "��" "��" "��" "��"
	     nil nil nil nil nil   "��" "��" "¡" "��" "��"
	     nil nil nil nil nil   "Ī" "��" "��" "μ" "��"]))

    (14 ; t
	(14 . ; t
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "��" "��" "ϯ" "��" "��"
	     nil nil nil nil nil   "��" "α" "��" "ȼ" "��"
	     nil nil nil nil nil   "��" "ū" "��" "��" "��"])
	(24 . ; g
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "��" "��" "��" "��" "��"
	     nil nil nil nil nil   "��" "��" "��" "��" "��"
	     nil nil nil nil nil   "��" "��" "��" "��" "��"])
	(28 ; l
	 (14 ; t
	  (16 . "��") ; u
	  (17 . "��") ; i
	  (25 . "��") ; h
	  (26 . "��") ; j
	  (27 . "��")) ; k
	 (16 . "��") ; u
	 (17 . "��") ; i
	 (25 . "��") ; h
	 (26 . "��") ; j
	 (27 . "��")) ; k
	(33 . ; v
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "��" "��" "��" "��" "��"
	     nil nil nil nil nil   "��" "��" "˥" "��" "��"
	     nil nil nil nil nil   "ı" "��" "��" "��" "��"])
	(34 . ; b
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "��" "��" "Ȧ" "��" "��"
	     nil nil nil nil nil   "��" "˳" "��" "��" "��"
	     nil nil nil nil nil   "̽" "��" "��" "��" "��"]))

    ;;
    (15 ; y
	(15 . ; y
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "��" "ɹ" "��" "��" "��"	nil nil nil nil nil
	     "��" "ƫ" "��" "��" "��"	nil nil nil nil nil
	     "��" "��" "��" "��" "��"	nil nil nil nil nil])
	(25 . ; h
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "ȡ" "��" "��" "��" "��"	nil nil nil nil nil
	     "��" "θ" "��" "��" "Ϸ"	nil nil nil nil nil
	     "��" "��" "��" "��" "ϧ"	nil nil nil nil nil])
	(35 . ; n
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "��" "��" "��" "��" "��"	nil nil nil nil nil
	     "��" "̻" "��" "��" "��"	nil nil nil nil nil
	     "��" "��" "��" "��" "��"	nil nil nil nil nil])
	(36 . ; m
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "��" "��" "��" "��" "��"	nil nil nil nil nil
	     "��" "¯" "��" "��" "��"	nil nil nil nil nil
	     "��" "��" "��" "��" "��"	nil nil nil nil nil]))

    (16 ; u
	(15 . ; y
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "��" "��" "��" "��" "��"	nil nil nil nil nil
	     "��" "˽" "��" "��" "®"	nil nil nil nil nil
	     "��" "ɱ" "��" "��" "��"	nil nil nil nil nil])
	(25 . ; h
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "��" "��" "��" "ɸ" "��"	nil nil nil nil nil
	     "��" "��" "��" "��" "��"	nil nil nil nil nil
	     "��" "��" "��" "Ĭ" "��"	nil nil nil nil nil])
	(35 . ; n
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "��" "��" "��" "ƭ" "��"	nil nil nil nil nil
	     "��" "��" "��" "��" "��"	nil nil nil nil nil
	     "��" "��" "��" "��" "��"	nil nil nil nil nil])
	(36 . ; m
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "��" "��" "��" "��" "��"	nil nil nil nil nil
	     "��" "ǥ" "˻" "��" "��"	nil nil nil nil nil
	     "��" "˱" "��" "��" "��"	nil nil nil nil nil]))

    (17 ; i
	(15 . ; y
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "ν" "ʴ" "��" "Ļ" "��"	nil nil nil nil nil
	     "��" "��" "��" "��" "š"	nil nil nil nil nil
	     "��" "Ĳ" "��" "��" "��"	nil nil nil nil nil])
	(25 . ; h
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "��" ";" "ɧ" "��" "��"	nil nil nil nil nil
	     "��" "˼" "��" "��" "��"	nil nil nil nil nil
	     "��" "��" "ƨ" "��" "��"	nil nil nil nil nil])
	(35 . ; n
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "��" "��" "��" "ˤ" "��"	nil nil nil nil nil
	     "��" "��" "ħ" "��" "Ǧ"	nil nil nil nil nil
	     "��" "��" "��" "��" "��"	nil nil nil nil nil])
	(36 . ; m
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "��" "��" "��" "��" "��"	nil nil nil nil nil
	     "��" "��" "��" "��" "��"	nil nil nil nil nil
	     "��" "��" "��" "��" "��"	nil nil nil nil nil]))

    (18 ; o
	(15 . ; y
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "��" "��" "��" "��" "��"	nil nil nil nil nil
	     "��" "��" "ɼ" "��" "��"	nil nil nil nil nil
	     "��" "��" "ɡ" "��" "��"	nil nil nil nil nil])
	(25 . ; h
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "��" "��" "��" "��" "λ"	nil nil nil nil nil
	     "��" "��" "��" "ε" "̴"	nil nil nil nil nil
	     "Ĥ" "��" "¹" "��" "��"	nil nil nil nil nil])
	(35 . ; n
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "��" "��" "ˢ" "��" "��"	nil nil nil nil nil
	     "��" "��" "��" "��" "��"	nil nil nil nil nil
	     "��" "��" "ţ" "��" "��"	nil nil nil nil nil])
	(36 . ; m
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "��" "��" "��" "��" "��"	nil nil nil nil nil
	     "��" "��" "��" "��" "��"	nil nil nil nil nil
	     "ɪ" "ϼ" "��" "��" "��"	nil nil nil nil nil]))

    (19 ; p
	(15 . ; y
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "˵" "��" "Ķ" "��" "��"	nil nil nil nil nil
	     "��" "��" "��" "��" "«"	nil nil nil nil nil
	     "��" "��" "â" "��" "��"	nil nil nil nil nil])
	(25 . ; h
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "��" "¥" "��" "��" "͵"	nil nil nil nil nil
	     "��" "��" "��" "��" "ʣ"	nil nil nil nil nil
	     "��" "ʾ" "��" "˸" "��"	nil nil nil nil nil])
	(35 . ; n
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "��" "��" "��" "��" "��"	nil nil nil nil nil
	     "��" "��" "��" "��" "˶"	nil nil nil nil nil
	     "��" "��" "î" "��" "ɨ"	nil nil nil nil nil])
	(36 . ; m
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "��" "��" "��" "��" "��"	nil nil nil nil nil
	     "��" "��" "��" "��" "��"	nil nil nil nil nil
	     "��" "��" "��" "��" "��"	nil nil nil nil nil]))

    ;;
    (20 ; a
	(14 . ; t
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "��" "��" "��" "��" "��"
	     nil nil nil nil nil   "��" "��" "��" "��" "��"
	     nil nil nil nil nil   "��" "��" "��" "ȷ" "��"])
	(24 . ; g
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "Ǽ" "��" "��" "��" "ź"
	     nil nil nil nil nil   "��" "��" "��" "��" "��"
	     nil nil nil nil nil   "��" "��" "å" "��" "��"])
	(28 (27 . "��")) ; l k
	(33 . ; v
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "��" "��" "��" "��" "��"
	     nil nil nil nil nil   "��" "ï" "��" "��" "��"
	     nil nil nil nil nil   "ɿ" "��" "��" "��" "��"])
	(34 . ; b
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "��" "ǡ" "µ" "��" "��"
	     nil nil nil nil nil   "��" "��" "��" "��" "ʯ"
	     nil nil nil nil nil   "��" "��" "��" "��" "��"]))

    (21 ; s
	(14 . ; t
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "õ" "��" "��" "��" "��"
	     nil nil nil nil nil   "��" "��" "��" "��" "��"
	     nil nil nil nil nil   "��" "��" "��" "��" "��"])
	(24 . ; g
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "��" "��" "��" "��" "��"
	     nil nil nil nil nil   "��" "��" "��" "��" "��"
	     nil nil nil nil nil   "��" "��" "Ϫ" "��" "��"])
	(28 ; l
	 (16 . "��") ; u
	 (17 . "��") ; i
	 (25 . "��") ; h
	 (26 . "��") ; j
	 (27 . "��")) ; k
	(33 . ; v
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "��" "��" "��" "��" "��"
	     nil nil nil nil nil   "��" "��" "��" "��" "ɶ"
	     nil nil nil nil nil   "ϳ" "��" "��" "��" "��"])
	(34 . ; b
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "ǭ" "��" "��" "��" "��"
	     nil nil nil nil nil   "��" "��" "��" "��" "��"
	     nil nil nil nil nil   "��" "��" "��" "��" "��"]))

    (22 ; d
	(14 . ; t
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "��" "��" "��" "��" "��"
	     nil nil nil nil nil   "��" "��" "��" "��" "æ"
	     nil nil nil nil nil   "��" "Ȫ" "��" "��" "��"])
	(24 . ; g
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "��" "��" "��" "��" "Ȩ"
	     nil nil nil nil nil   "��" "��" "��" "��" "��"
	     nil nil nil nil nil   "Ȣ" "¸" "��" "˴" "��"])
	(28 ; l
	 (16 . "��") ; u
	 (17 . "��") ; i
	 (22 (16 . "��")) ; d u
	 (25 . "��") ; h
	 (26 . "��") ; j
	 (27 . "��")) ; k
	(33 . ; v
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "ä" "δ" "��" "��" "��"
	     nil nil nil nil nil   "��" "��" "��" "��" "��"
	     nil nil nil nil nil   "��" "��" "��" "��" "��"])
	(34 . ; b
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "̡" "��" "��" "˧" "��"
	     nil nil nil nil nil   "��" "��" "��" "��" "��"
	     nil nil nil nil nil   "��" "��" "��" "��" "��"]))

    (23 ; f
	(14 . ; t
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "��" "��" "��" "§" "��"
	     nil nil nil nil nil   "��" "��" "��" "��" "��"
	     nil nil nil nil nil   "��" "ϲ" "��" "̯" "��"])
	(24 . ; g
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "��" "��" "��" "��" "��"
	     nil nil nil nil nil   "��" "��" "��" "��" "��"
	     nil nil nil nil nil   "��" "��" "��" "Ũ" "��"])
	(33 . ; v
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "��" "��" "��" "��" "��"
	     nil nil nil nil nil   "Ŧ" "��" "��" "��" "��"
	     nil nil nil nil nil   "��" "ų" "��" "��" "��"])
	(34 . ; b
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "��" "��" "̼" "��" "ƶ"
	     nil nil nil nil nil   "��" "��" "��" "��" "��"
	     nil nil nil nil nil   "��" "��" "��" "��" "��"]))

    (24 ; g
	(14 . ; t
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "��" "��" "��" "��" "��"
	     nil nil nil nil nil   "��" "��" "ŵ" "��" "��"
	     nil nil nil nil nil   "��" "��" "��" "��" "��"])
	(24 . ; g
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "ɮ" "��" "��" "��" "��"
	     nil nil nil nil nil   "��" "��" "��" "��" "��"
	     nil nil nil nil nil   "��" "��" "��" "��" "Ĩ"])
	(33 . ; v
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "Ħ" "��" "��" "��" "��"
	     nil nil nil nil nil   "ǻ" "��" "��" "��" "��"
	     nil nil nil nil nil   "��" "��" "��" "��" "��"])
	(34 . ; b
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "��" "��" "��" "��" "��"
	     nil nil nil nil nil   "��" "Ƽ" "��" "��" "��"
	     nil nil nil nil nil   "��" "��" "��" "��" "��"]))

    ;;
    (25 ; h
	(15 . ; y
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "ɤ" "��" "��" "��" "��"	nil nil nil nil nil
	     "��" "��" "��" "��" "��"	nil nil nil nil nil
	     "��" "��" "��" "��" "��"	nil nil nil nil nil])
	(25 . ; h
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "��" "��" "��" "��" "��"	nil nil nil nil nil
	     "��" "��" "��" "��" "��"	nil nil nil nil nil
	     "��" "��" "��" "��" "��"	nil nil nil nil nil])
	(35 . ; n
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "��" "��" "��" "��" "ʳ"	nil nil nil nil nil
	     "��" "��" "��" "̲" "��"	nil nil nil nil nil
	     "��" "��" "��" "��" "��"	nil nil nil nil nil])
	(36 . ; m
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "��" "��" "ø" "��" "ĩ"	nil nil nil nil nil
	     "��" "��" "��" "��" "Ǻ"	nil nil nil nil nil
	     "��" "��" "ϵ" "��" "��"	nil nil nil nil nil]))

    (26 ; j
	(15 . ; y
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "��" "��" "��" "Υ" "��"	nil nil nil nil nil
	     "��" "��" "Ʈ" "��" "ʩ"	nil nil nil nil nil
	     "��" "��" "��" "��" "��"	nil nil nil nil nil])
	(25 . ; h
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "��" "��" "��" "��" "��"	nil nil nil nil nil
	     "��" "��" "��" "��" "��"	nil nil nil nil nil
	     "º" "��" "��" "��" "��"	nil nil nil nil nil])
	(35 . ; n
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "��" "��" "��" "ȩ" "��"	nil nil nil nil nil
	     "ĳ" "��" "��" "��" "��"	nil nil nil nil nil
	     "��" "��" "��" "̷" "��"	nil nil nil nil nil])
	(36 . ; m
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "��" "��" "��" "��" "��"	nil nil nil nil nil
	     "ƹ" "��" "��" "��" "Ƹ"	nil nil nil nil nil
	     "��" "��" "��" "��" "��"	nil nil nil nil nil]))

    (27 ; k
	(15 . ; y
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "��" "��" "��" "��" "ͼ"	nil nil nil nil nil
	     "��" "��" "ˮ" "��" "��"	nil nil nil nil nil
	     "��" "��" "��" "��" "��"	nil nil nil nil nil])
	(25 . ; h
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "��" "��" "��" "̩" "��"	nil nil nil nil nil
	     "��" "��" "��" "��" "��"	nil nil nil nil nil
	     "��" "��" "��" "��" "��"	nil nil nil nil nil])
	(35 . ; n
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "��" "��" "��" "��" "��"	nil nil nil nil nil
	     "��" "��" "��" "��" "ͦ"	nil nil nil nil nil
	     "��" "ʰ" "��" "��" "��"	nil nil nil nil nil])
	(36 . ; m
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "��" "��" "��" "��" "��"	nil nil nil nil nil
	     "��" "��" "��" "��" "��"	nil nil nil nil nil
	     "��" "��" "��" "��" "��"	nil nil nil nil nil]))

    (28 ; l
	(15 . ; y
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "��" "��" "ʻ" "��" "��"	nil nil nil nil nil
	     "��" "��" "��" "��" "ð"	nil nil nil nil nil
	     "��" "��" "��" "ť" "��"	nil nil nil nil nil])
	(25 . ; h
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "��" "��" "��" "��" "Ʀ"	nil nil nil nil nil
	     "��" "��" "��" "��" "��"	nil nil nil nil nil
	     "��" "��" "��" "��" "��"	nil nil nil nil nil])
	(35 . ; n
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "��" "��" "��" "��" "ϡ"	nil nil nil nil nil
	     "��" "ͫ" "Χ" "��" "��"	nil nil nil nil nil
	     "��" "��" "��" "��" "��"	nil nil nil nil nil])
	(36 . ; m
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "��" "˹" "��" "��" "��"	nil nil nil nil nil
	     "Ǣ" "��" "��" "��" "��"	nil nil nil nil nil
	     "��" "��" "��" "��" "��"	nil nil nil nil nil]))

    (29 ; ;
	(15 . ; y
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "��" "��" "��" "��" "��"	nil nil nil nil nil
	     "��" "��" "��" "��" "��"	nil nil nil nil nil
	     "��" "��" "��" "��" "��"	nil nil nil nil nil])
	(25 . ; h
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "��" "��" "��" "��" "��"	nil nil nil nil nil
	     "��" "��" "��" "��" "��"	nil nil nil nil nil
	     "��" "��" "��" "��" "��"	nil nil nil nil nil])
	(35 . ; n
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "��" "��" "��" "��" "ʨ"	nil nil nil nil nil
	     "��" "��" "ʶ" "��" "��"	nil nil nil nil nil
	     "��" "��" "��" "��" "��"	nil nil nil nil nil])
	(36 . ; m
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "��" "��" "��" "ʮ" "��"	nil nil nil nil nil
	     "��" "Ʒ" "��" "ί" "��"	nil nil nil nil nil
	     "��" "��" "ȥ" "��" "��"	nil nil nil nil nil]))
    ;;
    (30 ; z
	(14 . ; t
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "��" "��" "��" "��" "��"
	     nil nil nil nil nil   "ļ" "��" "��" "Ǵ" "��"
	     nil nil nil nil nil   "��" "��" "��" "��" "��"])
	(24 . ; g
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "��" "��" "��" "��" "��"
	     nil nil nil nil nil   "��" "��" "��" "��" "��"
	     nil nil nil nil nil   "��" "ͤ" "��" "��" "��"])
	(33 . ; v
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "��" "��" "��" "ɦ" "��"
	     nil nil nil nil nil   "��" "̭" "��" "��" "��"
	     nil nil nil nil nil   "��" "��" "��" "��" "��"])
	(34 . ; b
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "��" "��" "��" "��" "��"
	     nil nil nil nil nil   "��" "��" "��" "��" "��"
	     nil nil nil nil nil   "��" "��" "��" "��" "��"]))

    (31 ; x
	(14 . ; t
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "��" "��" "��" "��" "��"
	     nil nil nil nil nil   "��" "��" "��" "��" "��"
	     nil nil nil nil nil   "��" "��" "��" "��" "��"])
	(24 . ; g
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "��" "��" "��" "��" "ǫ"
	     nil nil nil nil nil   "��" "��" "˷" "��" "��"
	     nil nil nil nil nil   "��" "��" "��" "��" "��"])
	(33 . ; v
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "��" "��" "��" "��" "Ȥ"
	     nil nil nil nil nil   "��" "��" "��" "��" "��"
	     nil nil nil nil nil   "��" "��" "��" "��" "��"])
	(34 . ; b
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "��" "��" "��" "��" "��"
	     nil nil nil nil nil   "��" "��" "��" "��" "��"
	     nil nil nil nil nil   "��" "˩" "��" "��" "��"]))

    (32 ; c
	(14 . ; t
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "��" "��" "β" "��" "��"
	     nil nil nil nil nil   "��" "��" "��" "��" "ʤ"
	     nil nil nil nil nil   "ɳ" "��" "��" "��" "��"])
	(24 . ; g
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "��" "��" "��" "��" "��"
	     nil nil nil nil nil   "��" "Τ" "��" "Ű" "��"
	     nil nil nil nil nil   "ƥ" "Ʃ" "��" "��" "��"])
	(33 . ; v
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "��" "��" "��" "��" "��"
	     nil nil nil nil nil   "��" "��" "��" "��" "��"
	     nil nil nil nil nil   "��" "��" "��" "��" "��"])
	(34 . ; b
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "��" "ƽ" "Ž" "��" "��"
	     nil nil nil nil nil   "��" "ű" "��" "��" "��"
	     nil nil nil nil nil   "��" "��" "��" "��" "��"]))

    (33 ; v
	(14 . ; t
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "��" "��" "��" "ϣ" "��"
	     nil nil nil nil nil   "��" "��" "��" "��" "��"
	     nil nil nil nil nil   "Ŀ" "��" "��" "��" "��"])
	(24 . ; g
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "��" "��" "��" "��" "��"
	     nil nil nil nil nil   "Ģ" "��" "��" "��" "��"
	     nil nil nil nil nil   "��" "��" "��" "��" "ʹ"])
	(33 . ; v
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "��" "��" "��" "��" "��"
	     nil nil nil nil nil   "��" "��" "��" "��" "��"
	     nil nil nil nil nil   "��" "��" "��" "��" "��"])
	(34 . ; b
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "��" "��" "��" "��" "��"
	     nil nil nil nil nil   "��" "��" "��" "��" "��"
	     nil nil nil nil nil   "��" "��" "ϴ" "��" "��"]))

    (34 ; b
	(14 . ; t
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "��" "��" "��" "��" "��"
	     nil nil nil nil nil   "��" "��" "��" "��" "ó"
	     nil nil nil nil nil   "��" "��" "��" "��" "��"])
	(24 . ; g
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "��" "��" "��" "��" "��"
	     nil nil nil nil nil   "��" "ͣ" "��" "��" "��"
	     nil nil nil nil nil   "��" "��" "��" "��" "��"])
	(33 . ; v
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "��" "��" "��" "��" "��"
	     nil nil nil nil nil   "��" "��" "��" "��" "��"
	     nil nil nil nil nil   "��" "��" "̪" "��" "��"])
	(34 . ; b
	    [nil nil nil nil nil    nil	 nil  nil  nil	nil
	     nil nil nil nil nil   "��" "��" "��" "��" "��"
	     nil nil nil nil nil   "��" "��" "��" "��" "̶"
	     nil nil nil nil nil   "ǿ" "��" "��" "ÿ" "��"]))

    ;;
    (35 ; n
	(15 . ; y
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "��" "��" "ͱ" "��" "��"	nil nil nil nil nil
	     "��" "��" "��" "��" "±"	nil nil nil nil nil
	     "��" "ƴ" "��" "��" "��"	nil nil nil nil nil])
	(25 . ; h
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "��" "��" "��" "��" "¶"	nil nil nil nil nil
	     "��" "��" "��" "��" "��"	nil nil nil nil nil
	     "��" "��" "��" "��" "ʽ"	nil nil nil nil nil])
	(35 . ; n
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "��" "��" "��" "��" "��"	nil nil nil nil nil
	     "��" "̨" "��" "��" "��"	nil nil nil nil nil
	     "˲" "è" "��" "��" "��"	nil nil nil nil nil])
	(36 . ; m
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "��" "��" "��" "��" "��"	nil nil nil nil nil
	     "��" "ͯ" "��" "̮" "��"	nil nil nil nil nil
	     "��" "÷" "��" "��" "��"	nil nil nil nil nil]))

    (36 ; m
	(15 . ; y
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "ķ" "��" "��" "��" "��"	nil nil nil nil nil
	     "϶" "��" "��" "��" "ĺ"	nil nil nil nil nil
	     "��" "��" "ö" "��" "��"	nil nil nil nil nil])
	(25 . ; h
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "��" "��" "��" "��" "��"	nil nil nil nil nil
	     "��" "��" "��" "©" "Ƨ"	nil nil nil nil nil
	     "��" "Ȳ" "Ρ" "��" "��"	nil nil nil nil nil])
	(35 . ; n
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "��" "̰" "��" "��" "��"	nil nil nil nil nil
	     "��" "��" "��" "ê" "��"	nil nil nil nil nil
	     "��" "��" "��" "��" "ʵ"	nil nil nil nil nil])
	(36 . ; m
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "��" "��" "��" "��" "��"	nil nil nil nil nil
	     "��" "��" "��" "��" "��"	nil nil nil nil nil
	     "��" "��" "��" "��" "��"	nil nil nil nil nil]))

    (37 ; ,
	(15 . ; y
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "��" "��" "̸" "��" "��"	nil nil nil nil nil
	     "��" "��" "��" "��" "��"	nil nil nil nil nil
	     "��" "��" "��" "��" "��"	nil nil nil nil nil])
	(25 . ; h
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "Ϥ" "��" "��" "ǳ" "��"	nil nil nil nil nil
	     "��" "��" "ü" "��" "��"	nil nil nil nil nil
	     "��" "��" "��" "��" "��"	nil nil nil nil nil])
	(35 . ; n
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "��" "��" "��" "��" "��"	nil nil nil nil nil
	     "��" "��" "͡" "��" "γ"	nil nil nil nil nil
	     "��" "��" "��" "Ͻ" "��"	nil nil nil nil nil])
	(36 . ; m
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "��" "��" "ʢ" "��" "��"	nil nil nil nil nil
	     "��" "ȶ" "��" "��" "��"	nil nil nil nil nil
	     "��" "��" "��" "��" "��"	nil nil nil nil nil]))

    (38 ; .
	(15 . ; y
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "˪" "��" "��" "ͪ" "��"	nil nil nil nil nil
	     "��" "��" "��" "į" "��"	nil nil nil nil nil
	     "��" "ɰ" "��" "��" "��"	nil nil nil nil nil])
	(25 . ; h
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "��" "κ" "��" "��" "��"	nil nil nil nil nil
	     "��" "��" "��" "��" "��"	nil nil nil nil nil
	     "��" "��" "��" "��" "ȭ"	nil nil nil nil nil])
	(35 . ; n
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "ˣ" "��" "��" "��" "��"	nil nil nil nil nil
	     "��" "��" "��" "��" "��"	nil nil nil nil nil
	     "��" "��" "��" "��" "��"	nil nil nil nil nil])
	(36 . ; m
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "��" "��" "��" "��" "��"	nil nil nil nil nil
	     "˨" "��" "��" "��" "��"	nil nil nil nil nil
	     "ĵ" "��" "ġ" "��" "��"	nil nil nil nil nil]))

    (39 ; /
	(15 . ; y
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "��" "��" "��" "��" "��"	nil nil nil nil nil
	     "��" "��" "��" "��" "��"	nil nil nil nil nil
	     "��" "��" "��" "ȫ" "ì"	nil nil nil nil nil])
	(25 . ; h
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "��" "��" "��" "��" "��"	nil nil nil nil nil
	     "��" "��" "��" "̦" "��"	nil nil nil nil nil
	     "��" "ĸ" "��" "��" "��"	nil nil nil nil nil])
	(35 . ; n
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "��" "��" "��" "��" "��"	nil nil nil nil nil
	     "��" "ζ" "��" "��" "˫"	nil nil nil nil nil
	     "��" "��" "��" "��" "��"	nil nil nil nil nil])
	(36 . ; m
	    [ nil  nil	nil  nil  nil	nil nil nil nil nil
	     "��" "Ů" "��" "��" "��"	nil nil nil nil nil
	     "��" "��" "��" "��" "��"	nil nil nil nil nil
	     "��" "̧" "��" "��" "ο"	nil nil nil nil nil]))))

(setq tcode-special-commands-alist
  '(((0 0) . (lambda () (tcode-show-tables nil nil))) ; 11 : LLɽ��ɽ��
    ((0 9) . (lambda () (tcode-show-tables nil t))) ; 10 : LRɽ��ɽ��
    ((9 0) . (lambda () (tcode-show-tables t nil))) ; 01 : RLɽ��ɽ��
    ((9 9) . (lambda () (tcode-show-tables t t))) ; 00 : RRɽ��ɽ��
    ((1 1) . tcode-start-jiscode)	; 22 : JIS ������ɽ����
    ((2 2) . tcode-toggle-alnum-mode) ; 33 : 1-2�Х����ڤ괹��
    ((2 1) . tcode-switch-variable) ; 32 : �������Υȥ���
    ((3 3) . (lambda ()
	       (tcode-display-stroke-sequence tcode-last-help-char-list)))
					; 44 : �إ��
    ((4 4) . (lambda () (tcode-query-stroke (point))))
					; 55 : �إ��
    ((6 6) . tcode-bushu-begin-alternate-conversion)
					; 77 : postfix �����Ѵ�
    ((7 7) . (lambda () (tcode-transpose-strokes nil)))
					; 88 : transpose-strokes
    ((8 8) . tcode-clear)
					; 99 : ��������Ѵ����򤼽��Ѵ��ʤɤ�
					; ����󥻥�
    ((20 28 20) . tcode-bushu-begin-conversion))) ; ala : ��������Ѵ��γ���


(setq tcode-mode-help-string "\
TUT�����ɥ⡼����Υ������ϼ��ΤȤ��ꡣ
   ala : ��������Ѵ��⡼�ɤ����롣ala���Ǥ�³����ȺƵ�Ū����������Ѵ���
	�Ԥ����Ȥ��Ǥ��롣
   alj : �򤼽��Ѵ���Ԥ�(see variable `tcode-use-prefix-mazegaki')��
   00, 01, 10, 11 : TUT�����ɤ�2���ȥ����Υ��ȥ���ɽ��ɽ�����롣
			(0���ֱ��ס�1���ֺ��פ��̣���Ƥ���)
   22 : JIS �����ɰ���ɽ�ˤ�����ϡ�
   32 : ������, . ���ڤ��ؤ��롣(see variable `tcode-switch-table-list')��
   33 : TUT������ɽ�ˤ���ѿ����������ʸ�������ɤ�1�Х��ȡ�2�Х����ڤ��ؤ���
   44 : ľ����ɽ�������Ǥ������ɽ�����롣
   55 : �ݥ���Ȱ��֤ˤ���ʸ�����Ǥ�����ɽ�����롣
   58 : ���Ѹ��ͥ�褷�Ƹ򤼽��Ѵ���Ԥ���
   77 : �ݥ�������ˤ���2ʸ������������Ѵ���Ԥ���
   88 : �ݥ���Ȱ��֤ˤ���ʸ����ե��ȥ���������(��: ǯ->��)��
	�����Ǥϥݥ���Ȥ�ľ����ʸ�����Ѵ����롣
   99 : �򤼽��Ѵ��⡼�ɤ������Ѵ��⡼�ɤˤ������ˡ�
	��������������󥻥뤹�롣�ޤ����إ�פ�ä���
   [1-4]8, [2-5]9: ʸ��������ꤷ�Ƹ򤼽��Ѵ���Ԥ���
   \\[toggle-input-method] : TUT�����ɥ⡼�ɤ�ȴ���롣

���Ƶ�ư���줿���ˤϡ�`tcode-ready-hook' ��¹Ԥ��롣
�ޤ�����ư������٤�`tcode-toggle-hook'��¹Ԥ��롣")

(defun tcode-make-special-for-tut (seq table)
  "TABLE �� SEQ �˴�Ť� `tcode-special-commands-alist' �Ѥ��Ѵ����롣"
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
				     ; alj: �򤼽��Ѵ�
	       ((9 8) . tcode-mazegaki-begin-alternate-conversion)
			  ; ���֡����֤��դθ򤼽��Ѵ�
	       ;; ��18�פ��ɤ�1ʸ���θ��ַ��򤼽��Ѵ�
	       ((0 7) . (lambda ()
			  (tcode-mazegaki-convert 1 current-prefix-arg)))
	       ;; ��28�פ��ɤ�2ʸ���θ��ַ��򤼽��Ѵ�
	       ((1 7) . (lambda ()
			  (tcode-mazegaki-convert 2 current-prefix-arg)))

	       ;; ��38�פ��ɤ�3ʸ���θ��ַ��򤼽��Ѵ�
	       ((2 7) . (lambda ()
			  (tcode-mazegaki-convert 3 current-prefix-arg)))

	       ;; ��48�פ��ɤ�4ʸ���θ��ַ��򤼽��Ѵ�
	       ((3 7) . (lambda ()
			  (tcode-mazegaki-convert 4 current-prefix-arg)))

     ;; ��58�פǳ��Ѥ������оݤȤ������ַ��򤼽��Ѵ�
	       ((4 7) . (lambda () (tcode-mazegaki-convert nil t)))

	    ;; ��29�פ��ɤ�2ʸ���γ��Ѥ������оݤȤ���
	       ;; ���ַ��򤼽��Ѵ�
	       ((1 8) . (lambda () (tcode-mazegaki-convert 2 t)))

	    ;; ��39�פ��ɤ�3ʸ���γ��Ѥ������оݤȤ���
	       ;; ���ַ��򤼽��Ѵ�
	       ((2 8) . (lambda () (tcode-mazegaki-convert 3 t)))

	    ;; ��49�פ��ɤ�4ʸ���γ��Ѥ������оݤȤ���
	       ;; ���ַ��򤼽��Ѵ�
	       ((3 8) . (lambda () (tcode-mazegaki-convert 4 t)))

	    ;; ��59�פ��ɤ�5ʸ���γ��Ѥ������оݤȤ���
	       ;; ���ַ��򤼽��Ѵ�
	       ((4 8) . (lambda () (tcode-mazegaki-convert 5 t))))))

(setq tcode-stroke-file-name (concat tcode-data-directory "tutcode.st"))

(setq eelll-text "EELLLTXT.tut")

;;; tutc-tbl.el ends here
