;;;; Street Organ Music Book Creator

;;;; converts xml sheet-music file of MuseScore into html file containing svg chart
;;;; very specially for the street organs built and performed by the owing master
;;;; of a tiny hotel "Andante" in Hachimantai, Iwate prefecture

;;;; runs on xyzzy lisp 0.2.2.253
(defvar *SOBC-version-id* "SOBC v2.31")
;;;; as of 19 July 2018

;;;; (c) ISHIZAKI Masato
;;;; mass.ishiz@gmail.com

;;;; 手回しオルガン用ブックの孔切抜きパターンを楽譜データから生成するライブラリ
;;;; 岩手県八幡平のペンション「安暖庭」ご主人手作りオルガン向けに作成

;;;; 機能：
;;;;
;;;; 1) create-svg
;;;; 開かれたファイルの音符データをブックパターンのsvg画像に変換する
;;;; この画像を、A4用紙に１ページずつ印刷するJavaScriptを仕込んだhtmlに包含させる
;;;;
;;;; 2) ask-to-create-a-book
;;;; 開かれたファイルがMuseScoreで書かれたxml(またはmusicxml)ファイルかどうかを判定し、
;;;; create-svgに渡す
;;;; *find-file-hooks*へのadd-hookで、ファイルを開くと自動起動する
;;;;
;;;; 3) create-pseudo-index-html
;;;; ツールバーから起動し、開かれるファイラで指定したファイル名(拡張子、"_book"は無視)から
;;;; 当該楽曲の関連ファイルへのリンク集であるpseuto-indexファイルを生成する
;;;;
;;;; 4) *after-save-buffer-hook*へのadd-hook
;;;; 上記3)で生成したpseudo-indexの曲目がSOBC-index.htmlになければ自動で1行生成して追加
;;;;
;;;; 5) *chrome-path*
;;;; chromeへのパスを探して格納しておく
;;;; 現時点ではsvg画像を正確に原寸で印刷可能なのはchromeのみのようなので、
;;;; このライブラリでは標準ブラウザをchromeとする
;;;;
;;;; 6) show-missing-files
;;;; 上記3)で、pseudo-indexに載せるべきリンク先のファイルが不足していたら
;;;; これを*SOBC-missing-files-for-pseudo-index*に保存して再確認する
;;;; ツールバーから起動
;;;;
;;;; 7) *init-app-menus-hook*へのadd-hook
;;;; ツールバーにメニューを追加

;;;; これらの使い方は下記urlを参照のこと
;;;; http://andante8mantai.ga/_rev3/_lisp/usage.html
;;;; より詳しくはこのソースをお読み取りください

;;;; 1.43 -> 2.00 改正点（2017/05/06 安暖庭にて実際のオルガンを試奏して）
;;;; model IIIのブック送り速度(feed-rate)を1.5倍（35 -> 53 mm/s）に
;;;; 同じく同音連続の最小切れ目(wipe-out-length)を0.5mm大きく（2.5 -> 3.0）
;;;; 印刷画像の曲名とページ番号の間にファイル名、このプログラムの版、変換日時を追加
;;;; 自動で吐き出すインデックス的htmlファイルを必要情報のみのシンプルな形に変更

;;;; 2.00 -> 2.1x- 改正点（2018/05/03 安暖庭訪問時の現場に即して、他）
;;;; 1) 速度指示が小節の頭にないとおかしいことになるバグ取り
;;;; 2) MuseScore 2.2.1の拡張子"musicxml"に対応
;;;; 3) ブック送りが速すぎてしんどいとの指摘により 45 mm/s を中心に可変に(2018/05/28)
;;;;    wipe-out-length はとりあえず変えずにおく(2018/05/28)
;;;; 4) yes-or-no-pなどをmessage-boxに統一し、体裁を整えた(2018/05/29-30)
;;;; 5) 7連符に対応、measure-div-with-marginにて端数が小節をはみ出すことを許容(2018/06/02)
;;;; 6) ブック（とindexっぽい）のhtmlがすでにあるならrenameをオプションで可能に(2018/06/03)
;;;; 7) 音域外の音が出現したらエラーに(2018/06/04)
;;;; 8) 入力ファイルに(goto-char (point-min)、pseudo-indexファイルに(search-buffer "<p>")
;;;; 9) 曲目リストのファイルに "<li><a pseudo-indexファイル名>曲名</a></li>/n" をhookで追加
;;;; 10) muxicxml仕様によりfocus関連でエラーが出る可能性にトラップを仕込む(v2.28b)
;;;; 11) get-integerで数が得られなければエラーを出してquitする(v2.28b)

;;;; To do for 2.1x - 2.2x(issues found upon visiting "Andante" on 2018/05/03, etc.):
;;;; 1) bug fix about tempo change amid a measure <= seems done, 2018/05/27
;;;; 2) treat extension "musicxml" properly for MusseScore 2.2.1 <done 2018/05/28
;;;; 3) book feed rate to be set before converting xml to html <= done, not elegant though 2018/05/28
;;;;    along with calculating wipe-out-lenght properly <= not implemented yet
;;;; 4) all dialog to be unified to message-box(2018/05/29-30)
;;;; 5) compatible with complex tuplets (e.g. septuplets),
;;;;    set measure-div-with-margin to allow fractional divs stepping out of measure(20218/06/02)
;;;; 6) if _book.html (as well as .html) exists, optional renaming allowed(2018/06/03)
;;;; 7) error traps for out-of-range pitch note(2018/06/04)
;;;; 8) (goto-char (point-min) for input file, (search-buffer "<p>") for pseudo-index file
;;;; 9) add a line "<li><a \"pseudo-index-file-name\">music title</a></li>\n" to music list file on hook
;;;; 10) on spec of musicxml, error might occur in focus-related functions, to be trapped (v2.28b)
;;;; 11) if a number is not obtained in get-integer, issue an error to quit(v2.28b)

;;; function create-svg
;;; main function of this library, to create an html file including an svg chart
;;; of Music Book for street organ
;;; the html file also includes some javascript functionalities to help printing

(defun create-svg ()
  (interactive)

  ;; function focus (a-tag), to restrict elements inside a pair of tags
  ;; <a-tag>[make restriction from here]anything[to here]</a-tag>
  ;; use inside (save-restriction)

  (defun focus (a-tag)
    (if (scan-buffer (concat "<" a-tag "\\([ \\t\\n]+[^>]*\\)*>") :regexp t)
	(narrow-to-region (match-end 0) (point-max))
      (return-from focus nil)) ; if opning tag not found, return nil
    (if (scan-buffer (concat "</" a-tag ">")) ; if closing tag not found, return error to quit
	(narrow-to-region (point-min) (match-beginning 0))
      (progn
	(message-box (concat "*** error in \"focus\" ***/n/n"
			     (format nil "tag seems not closed properly: ~A\n" a-tag)
			     (format nil "in line ~A\n" (current-line-number)))
		     *SOBC-verrsion-id*
		     '(:ok :exclamation))
	(widen)
	(quit)))
    (goto-char (point-min)))

  (defun focus-if (a-tag)
    (unless (focus a-tag) ; if a-tag not found, return error
      (message-box (concat "*** error in \"focus-if\" ***\n\n"
			   (format nil "in line ~A\ntag: ~A" (current-line-number) a-tag))
		   *SOBC-version-id*
		   '(:ok :exclamation))
      (widen)
      (quit)))

  ;; function (get-integer)
  ;; in the region focused by the above function, get the value if strings suggesting integer
  ;; 上記focusしたregionに「整数を表す文字列」があれば取って来る

  (defun get-integer ()
    (if (scan-buffer "[-\+]?[0-9]+" :regexp t)
	(parse-integer (match-string 0))
      (progn
	(message-box (concat "*** error in \"get-integer\" ***\n\n"
			     (format nil "in line ~A\n" (current-line-number)))
		     *SOBC-version-id*
		     '(:ok :exclamation))
	(widen)
	(quit))))

;;; main part of function

  (let ((page-height 190) ; mm, assuming A4 paper sheet of 210 x 297 mm
	(page-width 280) ; mm, ditto
	(offset-x 5) ; mm
	(offset-y 10) ; mm
	(note-stroke-width 0.08) ; mm
	(line-stroke-width 0.06) ; mm
	(key-line-stroke-width 0.04) ; mm
	(thick-line-stroke-width 0.1) ; mm
	(line-color "#000")
	(note-fill-color "#bbb")
	(black-key-color "#ddd")
	(wipe-out-color "#fff")
	(page-border-color "#000")
	(page-border-width 0.05) ;mm
	(page-border-dasharray "2 2")
	(measure-origin 0) ; mm
	(measure-number-margin-left 10) ; default unit
	(measure-number-margin-bottom 1) ; default unit
	(measure-number-font-size "0.3pc") ; given in a string width unit
	(measure-number-font-family "serif")
	(measure-number-font-style "italic")
	(file-name-font-size "0.3pc")
	(file-name-font-family "serif")
	(file-name-font-style "italic")
	(exceptional-tone-name-font-size "0.2pc")
	(exceptional-tone-name-font-family "sans-serif")
	(exceptional-tone-name-font-style "normal")
	(vertical-padding "0.1em")
	(line-height "1.2em")
	wipe-width
	margin-y
	page-border-x
	wipe-x
	wipe-out-stroke-width
	articulation
	(tenuto 0.87) ; default was 1, geometric sequence down to 0.5,...
	(detached-legato 0.76) ; was 0.67, ...as in "instrument.xml"
	(staccato 0.66) ; was 0.5
	(staccatissimo 0.57) ; was 0.33
	(normal 1)
	input-file-name
	input-buffer
	output-file-name
	output-buffer
	model
	book-height
	feed-rate
	top-tone-pitch-number
	bottom-tone-pitch-number
	top-tone-hole-offset
	tone-hole-pitch
	rect-height
	wipe-out-length
	black-keys
	keys
	thick-keys
	exceptionals
	bottom-y
	measure-number-y
	max-measure ; maximum measure number (= the length of this music)
	div-count ; current position in a measure, 1 div = 1/480 quater note(typ.) 
	measure-top
	measure-div ; number of divs in a measure
	(measure-margin 120) ; margin for tuplets(septuplets, etc.)
	measure-div-with-margin
	(regular-measure-div nil)
	div-time
	temp-div-time
	notes
	auftakt
	(mit-auftakt nil)
	a-tag
	(tempo nil)
	a-div-time
	duration
	slave
	pitch-number
	octave
	alter
	tied
	div-beginning-time
	duration-sec
	slit
	measure-width
	measure-end
	a-string
	a-key-line
	max-page-number
	music-title)

    (case (message-box "select organ model"
		       *SOBC-version-id*
		       '(:button3 :exclamation)
		       :button1 "model I"
		       :button2 "model II"
		       :button3 "model III")
      (:button3
       (setq model 3 ; for model III
	     book-height 170 ; mm
	     top-tone-pitch-number 80 ; highest G
	     keys 37
	     bottom-tone-pitch-number (1+ (- top-tone-pitch-number keys))
	     top-tone-hole-offset 6.8 ; mm, was 6.6
	     tone-hole-pitch 4.226 ; mm, was (* 3 (expt 2 0.5))
	     rect-height 2.5 ; mm
	     wipe-out-length 3.0 ; mm
	     black-keys '(45 47 50 52 55 57 59 62 64 67 69 71 74 76 79)
	     thick-keys '(8 20 32)))
      (:button2
       (setq model 2 ; for model II
	     book-height 150 ; mm
	     top-tone-pitch-number 80 ; highest G
	     keys 31
	     bottom-tone-pitch-number 41 ; with exceptionals
	     top-tone-hole-offset 14.6 ; mm
	     tone-hole-pitch 4.226 ; mm, was (* 3 (expt 2 0.5))
	     rect-height 2.5 ; mm
	     wipe-out-length 2.8 ; mm
	     black-keys '(55 57 59 62 64 67 69 71 74 76 79)
	     thick-keys '(8 20 27 28 29 30 31)
	     exceptionals '((50 "F") (51 "G") (52 "A") (53 "C"))))
      (:button1
       (setq model 1 ; for model I
	     book-height 114.3 ; mm
	     top-tone-pitch-number 80 ; highest G
	     keys 25
	     bottom-tone-pitch-number (1+ (- top-tone-pitch-number keys))
	     top-tone-hole-offset 3.5 ; mm
	     tone-hole-pitch 4.233 ; mm, 1/6 inch
	     rect-height 2.5 ; mm
	     wipe-out-length 2.8 ; mm
	     black-keys '(57 59 62 64 67 69 71 74 76 79)
	     thick-keys '(8 20))))

    (setq feed-rate 45)
    (let ((feed-rate-set nil)
	  (min-feed-rate 35)
	  (max-feed-rate 55))
      (while (not feed-rate-set)
	(case (message-box (format nil "set feed rate (~A - ~A mm/s)"
				   min-feed-rate max-feed-rate)
			   *SOBC-version-id*
			   '(:button2 :exclamation)
			   :button1 "-"
			   :button2 (format nil "~A mm/s" feed-rate)
			   :button3 "+")
	  (:button1
	   (decf feed-rate)
	   (if (< feed-rate min-feed-rate)
	       (setq feed-rate min-feed-rate)))
	  (:button3
	   (incf feed-rate)
	   (if (> feed-rate max-feed-rate)
	       (setq feed-rate max-feed-rate)))
	  (:button2
	   (if (eq (message-box (format nil "feed rate set as ~A mm/s" feed-rate)
				*SOBC-version-id*
				'(:ok-cancel :information :button1))
		   :ok)
	       (setq feed-rate-set t))))))

    (setq bottom-y (+ offset-y book-height)
	  measure-number-y (- (+ offset-y book-height) measure-number-margin-bottom)
	  wipe-width (- wipe-out-length note-stroke-width)
	  margin-y (+ top-tone-hole-offset (/ (- rect-height tone-hole-pitch) 2) offset-y)
	  wipe-out-stroke-width (+ note-stroke-width 0.08) ; mm
	  input-file-name (get-buffer-file-name)
	  input-buffer (buffer-name (selected-buffer))
	  output-file-name (concat (directory-namestring input-file-name)
				   (setq SOBC-simple-file-name
					 (pathname-name input-file-name))
				   "_book.html")
	  output-buffer (file-namestring output-file-name))

    ;; chech if file already exists and decide renew/new name/quit (2018/06/03)

    (while (file-exist-p output-file-name)
      (case (message-box (format nil "\"~A\" already exists." output-buffer)
			 *SOBC-version-id*
			 '(:question :button2)
			 :button1 "renew"
			 :button2 "give new name"
			 :button3 "quit")
	(:button1
	 (delete-file output-file-name))
	(:button2
	 (setq output-buffer (read-string "new name: "
					  :default output-buffer)
	       output-file-name (concat (directory-namestring input-file-name)
					output-buffer)))
	(:button3
	 (if (eq (message-box "quit procedure"
			      *SOBC-version-id*
			      '(:ok-cancel :exclamation :button2))
		 :ok)
	     (quit)))))

;;; first of all: get the title and length (measures) of music
;;; 最初にやること： 曲名とmeasureの最大数を得る

    (message "pre-processing")
    (goto-char (point-min))
    (save-restriction
      (setq music-title
	    (read-string "set music title: "
			 :default
			 (if (focus "work-title")
			     (buffer-substring (point-min) (point-max))
			   SOBC-simple-file-name))))
    (goto-char (point-max))
    (scan-buffer "<measure[ \\t\\n]*\\([^>]*\\)*>" :regexp t :reverse t)
    (save-restriction
      (narrow-to-region (match-beginning 0) (match-end 0))
      (scan-buffer "number[ \\t\\n]*=[ \\t\\n]*")
      (setq max-measure (get-integer)))

;;; get started to fill output buffer with html strings

    (switch-to-buffer output-buffer)
    (set-buffer-file-name output-file-name)
    (insert
     (concat
      "<!DOCTYPE html>\n"
      "<html>\n"
      "<!--\n"
      "********************************************************************************\n"
      (format nil "\tGenerated by Street Organ Book Creator ~A\n" *SOBC-version-id*)
      (format-date-string "\tat %Y/%m/%d %H:%M:%S %#Z UTC\n")
      (format nil "\tbook feed rate set as ~A mm/s\n\n" feed-rate)
      "\tThis music book is converted for the street organ\n"
      "\tbuild and performed by the owing master of a tiny hotel \"Andante\"\n"
      "\tin Hachimantai, Iwate prefecture\n\n"
      "\thtml structure: (c) ISHIZAKI Masato\n"
      "\tmass.ishiz@gmail.com\n"
      "********************************************************************************\n"
      "-->\n"
      "<!-- music title: "
      music-title
      " -->\n"
      "<head>\n"
      "<meta charset=\"Shift-JIS\">\n"
      "<style type=\"text/css\">\n"
      "p {\n"
      "\tline-height: " line-height "; \n"
      "\tmargin: 0; \n"
      "\tpadding: " vertical-padding " 0;\n"
      "}\n\n"
      "rect.note {\n"
      "\tstroke: " line-color "; \n"
      (format nil "\tstroke-width: ~Fmm; \n" note-stroke-width)
      "\tfill: " note-fill-color ";\n"
      "}\n\n"
      "rect.wipeOut {\n"
      "\tstroke: " wipe-out-color "; \n"
      (format nil "\tstroke-width: ~Fmm; \n" wipe-out-stroke-width)
      "\tfill: " wipe-out-color ";\n"
      "}\n\n"
      "rect.key {\n"
      "\tstroke: " black-key-color "; \n"
      (format nil "\tstroke-width: ~Fmm; \n" note-stroke-width)
      "\tfill: " black-key-color ";\n"
      "}\n\n"
      "rect.wipeOutBlack {\n"
      "\tstroke: " black-key-color "; \n"
      (format nil "\tstroke-width: ~Fmm; \n" wipe-out-stroke-width)
      "\tfill: " black-key-color ";\n"
      "}\n\n"
      "line.bar,.top,.bottom {\n"
      "\tstroke: " line-color "; \n"
      (format nil "\tstroke-width: ~Fmm;\n" line-stroke-width)
      "}\n\n"
      "line.key {\n"
      "\tstroke: " line-color "; \n"
      (format nil "\tstroke-width: ~Fmm;\n" key-line-stroke-width)
      "}\n\n"
      "line.thick {\n"
      (format nil "\tstroke-width: ~Fmm;\n" thick-line-stroke-width)
      "}\n\n"
      "text.key {\n"
      "\tfont-style: " exceptional-tone-name-font-style "; \n"
      "\tfont-family: " exceptional-tone-name-font-family "; \n"
      "\tfont-size: " exceptional-tone-name-font-size "; \n"
      "}\n\n"
      "text.measure-number {\n"
      "\tmargin: 0mm; \n"
      "\tpadding: 0.5mm; \n"
      "\tfont-size: " measure-number-font-size "; \n"
      "\tfont-family: " measure-number-font-family "; \n"
      "\tfont-style: " measure-number-font-style "; \n"
      "}\n\n"
      ".page-border {\n"
      "\tstroke: " page-border-color "; \n"
      (format nil "\tstroke-width: ~Fmm; \n" page-border-width)
      "\tstroke-dasharray: " page-border-dasharray "; \n"
      "}\n\n"
      "text.file-name {\n"
      "\tmargin: 0mm; \n"
      "\tpadding: 0.5mm; \n"
      "\talignment-baseline: text-before-edge; \n"
      "\tfont-size: " measure-number-font-size "; \n"
      "\tfont-family: " measure-number-font-family "; \n"
      "\tfont-style: " measure-number-font-style "; \n"
      "}\n\n"
      "#footer p{\n"
      "\ttext-align: center;\n"
      "}\n\n"
      "@media print {\n"
      "\t.noprint {\n"
      "\t\tdisplay: none; \n"
      "\t}\n"
      "}\n"
      "</style>\n\n"
      "<script type=\"text/javascript\">\n"
      "var pageNumber = 1;\n"
      "var minPageNumber = 1;\n"
      "var maxPageNumber = ;\n\n" ; figure inserted in the end of this program
      "function nextPage() {\n"
      "\tpageNumber = pageNumber + 1;\n"
      "\tif (pageNumber > maxPageNumber) {\n"
      "\t\tpageNumber = maxPageNumber;\n"
      "\t}\n"
      (format nil "\tvar x = (pageNumber - 1) * ~F;\n" page-width)
      (format nil "\tdocument.getElementById(\"book\").setAttribute(\"viewBox\", \"\" + x + \" 0 ~F ~F\");\n"
	      page-width page-height)
      "}\n\n"
      "function previousPage() {\n"
      "\tpageNumber = pageNumber - 1;\n"
      "\tif (pageNumber < minPageNumber) {\n"
      "\t\tpageNumber = minPageNumber;\n"
      "\t}\n"
      (format nil "\tvar x = (pageNumber - 1) * ~F;\n" page-width)
      (format nil "\tdocument.getElementById(\"book\").setAttribute(\"viewBox\", \"\" + x + \" 0 ~F ~F\");\n"
	      page-width page-height)
      "}\n\n"
      "function firstPage() {\n"
      "\tpageNumber = minPageNumber;\n"
      (format nil "\tvar x = (pageNumber - 1) * ~F;\n" page-width)
      (format nil "\tdocument.getElementById(\"book\").setAttribute(\"viewBox\", \"\" + x + \" 0 ~F ~F\");\n"
	      page-width page-height)
      "}\n\n"
      "function lastPage() {\n"
      "\tpageNumber = maxPageNumber;\n"
      (format nil "\tvar x = (pageNumber - 1) * ~F;\n" page-width)
      (format nil "\tdocument.getElementById(\"book\").setAttribute(\"viewBox\", \"\" + x + \" 0 ~F ~F\");\n"
	      page-width page-height)
      "}\n\n"
      "function printUntilEnd() {\n"
      "\tfor (var i = pageNumber; i <= maxPageNumber; i++) {\n"
      "\t\twindow.print();\n"
      "\t\tnextPage();\n"
      "\t}\n"
      "}\n\n"
      "function printAll() {\n"
      "\tfirstPage();\n"
      "\tprintUntilEnd();\n"
      "}\n\n"
      "var key = document.getElementsByClassName(\"key\");\n\n"
      "function hideKey() {\n"
      "\tfor (var i = 0; i < key.length; i++) {\n"
      "\t\tkey[i].style.visibility = \"hidden\";\n"
      "\t}\n"
      "}\n\n"
      "function showKey() {\n"
      "\tfor (var i = 0; i < key.length; i++) {\n"
      "\t\tkey[i].style.visibility = \"visible\";\n"
      "\t}\n"
      "}\n\n"
      "var keyVisible = 1;\n\n"
      "function toggleKey() {\n"
      "\tif (keyVisible == 1) {\n"
      "\t\thideKey();\n"
      "\t\tkeyVisible = 0;\n"
      "\t} else {\n"
      "\t\tshowKey();\n"
      "\t\tkeyVisible = 1;\n"
      "\t}\n"
      "}\n"
      "</script>\n\n"
      "<title>" SOBC-simple-file-name "_book</title>\n"
      "</head>\n"
      "<body>\n"
      (format nil "<svg width=\"~Fmm\" height=\"~Fmm\" id=\"book\" viewBox=\"0 0 ~F ~F\">\n"
	      page-width page-height page-width page-height)
      "<!-- keyboard mimics -->\n")) ; svg elements inserted in the end of this program

;;; main loop: iterate for each measure
;;; make a list of tone-hole information from each note (ex rest) in the measure
;;; get divisions from attributes (just once at the beginning?)
;;; clasify to handle direction, note, backup and forward
;;; add tone information to the list if note but not rest
;;; handle div-count properly

;;; main loop: 各measureで回す
;;; measureの中身をnote（rest以外）の孔情報のリストに落とす
;;; attributesからdivisionsを取得（曲の頭で1度だけ？）
;;; direction, note, backup, forwardを仕分け処理
;;; note(でrestでない)ならlistにnote情報を追加
;;; 適宜div-countを操作

    (switch-to-buffer input-buffer)
    (goto-char (point-min))
    (setq notes (make-array (1+ max-measure)) ; auftaktのときのみ measure = 0 に意味がある
	  a-div-time 1) ; setting dummy parameters initially
    (while (scan-buffer "<measure[ \\t\\n]*\\([^>]*\\)*>" :regexp t) ; input-bufferに<measure>がある限りループ
      (save-excursion
	(save-restriction
	  (narrow-to-region (match-beginning 0) (match-end 0)) ; <measure ...>にregionを設定
	  (scan-buffer "number[ \\t\\n]*=[ \\t\\n]*" :regexp t)
	  (setq measure (get-integer)
		auftakt (zerop measure)) ; 現在地がauftaktかどうかを判定
	  (if auftakt (setq mit-auftakt t)))) ; この曲にauftaktがあるかどうかをセット　上の行とまとめてはならない
      (message (format nil "processing measure ~D / ~D" measure max-measure))
      (setq div-count 0 measure-top t) ; div-count: measureの中で現在位置を特定する変数
      (if (and mit-auftakt (= measure 1))
	  (setq measure-div regular-measure-div
		meassure-div-with-margin (+ measure-div measure-margin)))
      (save-restriction
	(focus-if "measure")
	(while (scan-buffer "<\\([^ />]+\\)" :regexp t)
	  (setq a-tag (match-string 1))

	  (cond
	   ((string= a-tag "attributes")
	    (when measure-top
	      (dolist (var-name '(divisions beats beat-type)) ; "fifths" deleted, '18/05/03
		(save-restriction
		  (goto-char (point-min))
		  (if (focus (string var-name))
		      (set var-name (get-integer)))))
	      (setq measure-div (/ (* 4 divisions beats) beat-type)
		    regular-measure-div measure-div
		    measure-div-with-margin (+ measure-div measure-margin) ; add margin for tuplet
		    div-time (make-array measure-div-with-margin :initial-element 0)
		    temp-div-time 0)

	      (dotimes (i measure-div-with-margin)
		(setf (aref div-time i)
		      (incf temp-div-time a-div-time))))
	    (scan-buffer "</attributes>" :tail t))

	   ((string= a-tag "direction")
	    (save-restriction
	      (focus "direction")
	      (when (scan-buffer "sound tempo[ \\t\\n]*=[ \\t\\n]*\"\\([0-9]+[\.0-9]*\\)\"/>" :regexp t)
		(setq tempo (read-from-string (match-string 1))
		      a-div-time (/ 60 tempo divisions)
		      temp-div-time (if (zerop div-count) 0
				      (aref div-time (1- div-count))))
		(dotimes (i (- measure-div-with-margin div-count))  ;; このループを"note"で"measure-top"の場合も
		  (setf (aref div-time (+ div-count i)) ;; 実行さすべし (for v2.10)
			(incf temp-div-time a-div-time)))))
	    (scan-buffer "</direction>" :tail t))

	   ((string= a-tag "note")
	    (save-restriction
	      (focus "note")

	      ;テンポの指定がなければ怒る
	      ; alart if tempo not set
	      (unless tempo
		(message-box "Tempo not set\n\nquit procedure"
			     *SOBC-version-id*
			     '(:ok :exclamation))
		(delete-buffer output-buffer)
		(quit))

	      (when measure-top
		(setq temp-div-time 0)
		(dotimes (i measure-div-with-margin)  ;; このループを"note"で"measure-top"の場合も
		  (setf (aref div-time i) ;; 実行させてみる (for v2.10)
			  (incf temp-div-time a-div-time)))
		(setq measure-top nil))

	      (save-restriction
		(focus-if "duration")
		(setq duration (get-integer)))
	      (goto-char (point-min))
	      (save-restriction
		(unless (scan-buffer "<rest[ \\t\\n/>]" :regexp t)
		  (if (setq slave (scan-buffer "<chord[ \\t\\n/>]" :regexp t))
		      (decf div-count duration))
		  (goto-char (point-min))
		  (save-restriction
		    (focus-if "step")
		    (looking-at ".")
		    (setq pitch-number
			  (case (elt (match-string 0) 0)
			    (#\C 1) (#\D 3) (#\E 5) (#\F 6) (#\G 8) (#\A 10) (#\B 12)
			    (otherwise
			     (message-box
			      (concat
			       (format nil
				      "*** FATAL ERROR *** \n\npitch-name unidentified\nin measure ~A\,n" measure)
			       (format nil "line ~A" (current-line-number)))
			      *SOBC-version-id*
			      '(:ok :exclamation))
			     (quit)))))
		  (goto-char (point-min))
		  (save-restriction
		    (focus-if "octave")
		    (setq octave (get-integer)))
		  (goto-char (point-min))
		  (save-restriction
		    (setq alter (if (focus "alter") (get-integer) 0)))
		  (goto-char (point-min))
		  (save-restriction
		    (setq tied (scan-buffer "<tie[ \\t\\n]+[^>]*type[ \\t\\n]*=[ \\t\\n]*\"stop\"[^/]*/>"
					    :regexp t)))
		  (unless slave
		    (goto-char (point-min))
		    (save-restriction
		      (setq articulation
			    (if (focus "articulations")
				(cond ((scan-buffer "<staccato[ \\t\\n/]" :regexp t) staccato)
				      ((scan-buffer "<staccatissimo[\\t\\n/]" :regexp t) staccatissimo)
				      ((scan-buffer "<detached-legato[ \\t\\n/]" :regexp t) detached-legato)
				      ((scan-buffer "<tenuto[ \\t\\n/]" :regexp t) tenuto)
				      (t normal))
			      normal))))
		  (incf pitch-number (+ (* 12 octave) alter))

		  ;; out of range 判定するならここで
		  ;; confirm if out-of-range tone
		  (case (if (< pitch-number bottom-tone-pitch-number)
			    :too-low
			  (if (> pitch-number top-tone-pitch-number)
			      :too-high))
		    (:too-low
		     (message-box (format nil "low tone pitch out of range \nin measure ~A" measure)
				  *SOBC-version-id*
				  '(:ok :exclamation))
		     (delete-buffer output-buffer)
		     (quit))
		    (:too-high
		     (message-box (format nil "high tone pitch out of range \nin measure ~A" measure)
				  *SOBC-version-id*
				  '(:ok :exclamation))
		     (delete-buffer output-buffer)
		     (quit)))

		  (case model
		    (2 ; 低音部の「鳴らせない音」への例外処理
		     (if (<= pitch-number 53)
			 (setq pitch-number
			       (case pitch-number
				 ((43 45 47 48 50 51 52 53)
				  (message-box
				   (format nil "missing pipe for model II \nin measure ~A" measure)
				   *SOBC-version-id*
				   '(:ok :exclamation))
				  (delete-buffer output-buffer)
				  (quit))
				 (49 53) (46 52) (44 51) (42 50))))))
		  (setq div-beginning-time (if (zerop div-count) 0 (aref div-time (1- div-count)))
			duration-sec (* articulation
					(- (aref div-time (1- (+ div-count duration))) div-beginning-time))
			slit (list div-beginning-time
				   duration-sec
				   pitch-number
				   tied))
		  (setf (aref notes measure)
			(append (aref notes measure) (list slit))))
		(incf div-count duration)))
	    (scan-buffer "</note>" :tail t))

	   ((string= a-tag "backup")
	    (if auftakt (setq measure-div div-count auftakt nil))
	    (save-restriction
	      (focus "backup")
	      (focus-if "duration")
	      (decf div-count (get-integer)))
	    (scan-buffer "</backup>" :tail t))

	   ((string= a-tag "forward")
	    (save-restriction
	      (focus "forward")
	      (focus-if "duration")
	      (incf div-count (get-integer)))
	    (scan-buffer "</forward>" :tail t))

	   (t (scan-buffer ">")
	      (if (looking-back "/")
		  (forward-char)
		(scan-buffer (concat "</" a-tag ">") :tail t)))))

	(if auftakt (setq measure-div div-count auftakt nil)))
      (scan-buffer "</measure>" :tail t)

      ;; list (of list of information) of notes is ready up to here
      ;; ここまでで、measure毎に(note(の要素情報list)のlist)ができる

      ;; sort all notes in the measure by beginning time (= car) first, to treat tied tones properly
      (setf (aref notes measure) (sort (aref notes measure) #'< :key #'car))

;;; objectives: make a large (long) svg image (virtual)
;;; move ViewBox from page to page by JavaScript in outer html
;;; to obtain printing image on A4 sheet papers
;;; (or print to pdf image with real dimensions for paper-printing)

;;; 方針：大きい１つのsvg画像を作成しておいて、
;;; 外側のhtmlに仕込んだJavaScriptでViewBoxを次々と動かして
;;; A4複数ページの印刷イメージを得る
;;; （印刷時にpdf化して原寸画像を得ることもできる）

      (switch-to-buffer output-buffer)
      (setq measure-width (* (aref div-time (1- measure-div)) feed-rate)
	    measure-end (+ measure-origin measure-width))

      (insert
       (concat
	(setq a-string nil)
	(dolist (a-note (aref notes measure) a-string) ; draw cut-out rectangles of this measure
	  (setq a-string
		(concat
		 a-string
		 (let* ((beginning-time (first a-note))
			(duration-sec (second a-note))
			(tone-pitch-number (third a-note))
			(tiedp (fourth a-note))
			(x (+ (* beginning-time feed-rate) measure-origin))
			(y (+ offset-y top-tone-hole-offset
			      (* (- top-tone-pitch-number tone-pitch-number) tone-hole-pitch)))
			(dx (* duration-sec feed-rate)))
		   (concat
		    (unless tiedp ; wipe out preceding space for the note first, unless tied
		      (concat
		       (format nil "<rect class=\"wipeOut\" x=\"~F\" y=\"~F\" width=\"~F\" height=\"~F\"/>\n"
			       (setq wipe-x (- x wipe-out-length)) y wipe-width rect-height)
		       (if (find tone-pitch-number black-keys) ; stain wiped area if black key
			   (format nil "<rect class=\"wipeOutBlack key\" x=\"~F\" y=\"~F\" width=\"~F\" height=\"~F\"/>\n"
				   wipe-x y wipe-width rect-height))))
		    (format nil "<rect class=\"note\" x=\"~F\" y=\"~F\" width=\"~F\" height=\"~F\"/>\n"
			    x y dx rect-height))))))
	(format nil "<line class=\"bar\" x1=\"~F\" y1=\"~F\" x2=\"~F\" y2=\"~F\"/>\n"
		measure-origin offset-y measure-origin bottom-y)
	(format nil "<text class=\"measure-number\" x=\"~F\" y=\"~F\" text-anchor=\"end\">~D</text>\n\n"
		(+ measure-origin measure-number-margin-left) measure-number-y measure)))
      (setq measure-origin measure-end)
      (switch-to-buffer input-buffer)) ; end of outmost "while"

    (message "post-processing")
    (switch-to-buffer output-buffer)
    (setq max-page-number (ceiling (/ measure-end page-width)))

    (insert
     (concat
      (format nil "<line class=\"bar\" x1=\"~F\" y1=\"~F\" x2=\"~F\" y2=\"~F\"/>\n"
	      measure-end offset-y measure-end bottom-y)
      (format nil "<line class=\"top\" x1=\"0\" y1=\"~F\" x2=\"~F\" y2=\"~F\"/>\n"
	      offset-y measure-end offset-y)
      (format nil "<line class=\"bottom\" x1=\"0\" y1=\"~F\" x2=\"~F\" y2=\"~F\"/>\n\n"
	      bottom-y measure-end bottom-y)
      (setq a-string nil)
      (dotimes (i max-page-number a-string)
	(setq a-string
	      (concat
	       a-string
	       (unless (zerop i)
		 (format nil "<line class=\"page-border\" x1=\"~F\" y1=\"0\" x2=\"~F\" y2=\"~F\"/>\n"
			 (setq page-border-x (* i page-width)) page-border-x page-height))
	       (format nil "<text class=\"file-name\" x=\"~F\" y=\"~F\">~A [~A, "
		       (+ offset-x (* i page-width)) (- offset-y 4.5)
		       music-title SOBC-simple-file-name)
	       (format-date-string "%Y/%m/%d %H:%M:%S")
	       (format nil "; ~A mm/s, ~A] page ~D</text>\n"
		       feed-rate *SOBC-version-id* (1+ i)))))
      "\n</svg>\n\n"
      "<div class=\"noprint\" id=\"footer\">\n"
      "<p><a href=\"#\" onClick=\"javascript: firstPage()\">最初のページ</a> | \n"
      "<a href=\"#\" onClick=\"javascript: previousPage()\">前のページ</a> | \n"
      "<a href=\"#\" onClick=\"javascript: nextPage()\">次のページ</a> | \n"
      "<a href=\"#\" onClick=\"javascript: lastPage()\">最後のページ</a></p>\n"
      "<p><a href=\"#\" onClick=\"javascritp: window.print()\">このページを印刷</a> | \n"
      "<a href=\"#\" onClick=\"javascript: window.print();nextPage()\">このページを印刷して次のページへ</a> | \n"
      "<a href=\"#\" onClick=\"javascript: printUntilEnd()\">このページから最後までを次々と印刷</a> | \n"
      "<a href=\"#\" onClick=\"javascript: printAll()\">全ページを次々と印刷</a></p>\n"
      "<p><a href=\"#\" onClick=\"javascript: toggleKey()\">鍵盤表示切替</a></p>\n"
      "</div>\n\n"
      "<script type=\"text/javascript\">toggleKey()</script>\n\n"
      "</body>\n"
      "</html>\n"))

    (goto-char (point-min))
    (scan-buffer "var maxPageNumber = " :tail t)
    (insert (format nil "~D" max-page-number)) ; give maxPageNumber in header script

    (scan-buffer "<!-- keyboard mimics -->\n" :tail t) ; draw keyboard mimics
    (insert
     (concat
      (setq a-string nil)
      (dolist (a-black-key black-keys a-string)
	(setq a-string
	      (concat
	       a-string
	       (format nil "<rect class=\"key\" x=\"0\" y=\"~F\" width=\"~F\" height=\"~F\"/>\n"
		       (+ margin-y (* tone-hole-pitch (- top-tone-pitch-number a-black-key)))
		       measure-end tone-hole-pitch))))
      (setq a-string nil)
      (dotimes (i (1+ keys) a-string)
	(setq a-string
	      (concat
	       a-string
	       (format nil "<line class=\"key~[ thick~;~]\" x1=\"0\" y1=\"~F\" x2=\"~F\" y2=\"~F\"/>\n"
		       (if (find i thick-keys) 0 1)
		       (setq a-key-line (+ margin-y (* tone-hole-pitch i)))
		       measure-end a-key-line))))
      (case model
	(2 ; exceptional handling for model II
	 (setq a-string nil)
	 (let (y-adjustment string-offset)
	   (dolist (an-exceptional exceptionals a-string)
	     (let* ((tone-number (car an-exceptional))
		    (tone-name (cadr an-exceptional))
		    (number-of-exceptionals (length exceptionals))
		    (y-adjustment -1.5)
		    (y (+ offset-y top-tone-hole-offset y-adjustment
			  (* (1+ (- top-tone-pitch-number tone-number)) tone-hole-pitch)))
		    (string-offset 5)
		    (string-pitch (* string-offset number-of-exceptionals))
		    (order-place (mod tone-number number-of-exceptionals))
		    (x (- (* order-place string-offset) string-pitch)))
	       (while (< (incf x string-pitch) measure-end)
		 (setq a-string
		       (concat
			a-string
			(format nil "<text class=\"key\" x=\"~F\" y=\"~F\">~A</text>"
				x y tone-name)))))))))
      "<!-- end of keyborad mimics -->\n\n"))

    (save-buffer) ; save output file
    (message "done")

    (if *chrome-path*
	(call-process (concat *chrome-path* " " output-file-name)) ; open with chrome
      (progn
	(message-box "*** chrome.exe not found ***\n\nopen with default browser instead"
		     *SOBC-version-id*
		     '(:ok :exclamation))
	(shell-execute output-file-name))) ; if chrome not found, open with default browser

    (switch-to-buffer input-buffer)
    (goto-char (point-min))
    ;(delete-buffer (selected-buffer)) ; this line causes an unexpected dialog: "削除されたバッファです"
    (delete-buffer output-buffer)))

;;; main program ends here

;;; chrome path to open an xml file, set as a special variable

(defvar *chrome-path*
  (let ((chrome-path-candidate
	 `("C:\\Program Files (x86)\\Google\\Chrome\\Application\\chrome.exe"
	   "C:\\Program Files\\Google\\Chrome\\Application\\chrome.exe"
	   ,(concat "C:\\Users\\"
		    (user-name)
		    "\\AppData\\Local\\Google\\Chrome\\Application\\chrome.exe")
	   ,(concat "C:\\Documents and Settings\\"
		    (user-name)
		    "\\Local Settings\\Application Data\\Google\\Chrome\\Application\\chrome.exe"))))
    (dolist (a-path chrome-path-candidate)
      (when (file-exist-p a-path)
	(return a-path)))))

;;; function to activate when .xml (or .musicxml) file is loaded

(defun ask-to-create-a-book ()
  (let (file-name ext)
    (setq file-name (file-namestring (get-buffer-file-name))
	  ext (pathname-type file-name))
    (if (or (string= ext "musicxml")
	    (string= ext "xml"))
	(if (scan-buffer "<software>MuseScore") ; give verrsion number if necessary
	    (if (eq (message-box
		     (concat "Street Organ Book Creator " *SOBC-version-id* "\n\n\""
			     file-name "\"\n\nCreate a chart pattern of the book from this file?\n")
		     *SOBC-version-id*
		     '(:yes-no :question :button1))
		    :yes)
		(create-svg)
	      (goto-char (point-min)))))))

(add-hook '*find-file-hooks* #'ask-to-create-a-book)

;;; function to create an pseudo-index html file to put on the web
;;; select a file (extension ignored) with filer to create html with links to related files
;;; add a line to music list file on save by hook
;;; ファイラでファイルを指定（拡張子は無視される）すると、
;;; その名前の一連のファイル群へのリンクのリストを有するpseudo-indexのhtmlファイルを吐き出す
;;; save時にhookで曲目リストのファイルに1行追加

(defun create-pseudo-index-html () (interactive)
  (let (full-file-name
	full-bufferr-name
	a-buffer
	directory-name
	file-name
	file-name-length
	file-set
	file-name-with-ext
	music-title)
    (when (setq full-file-name ; was (when (setq full-file-name
		(file-name-dialog
		 :title (concat *SOBC-version-id* ": select any related file")
		 :multiple nil))
      (setq file-name (pathname-name full-file-name)
	    file-name-length (length file-name)
	    directory-name (directory-namestring full-file-name)
	    file-set '(("_book" "html" "ブックパターン" "(html)　← Chromeでの印刷はこちらをどうぞ")
		       (nil "pdf" "譜面" "(pdf)")
		       ;("_book" "pdf" "ブックパターン" "(pdf)")
		       ;(nil "mp3" "参考音源" "(mp3)")
		       ;(nil "xml" "譜面の元ファイルのテキストデータ" "(xml)")
		       (nil "mscz" "譜面の元ファイル" "（要<a href=\"https://musescore.org/ja\">MuseScore</a>）")))
      (if (string-looking-at "_book" file-name :start (decf file-name-length 5))
	  (setq file-name (subseq file-name 0 file-name-length)))
      (setq a-buffer (concat file-name ".html"))
      (while (file-exist-p
	      (setq full-buffer-name
		    (concat directory-name a-buffer)))
	(case (message-box (format nil "~A already exists." a-buffer)
			   *SOBC-version-id*
			   '(:exclamation :button2)
			   :button1 "renew"
			   :button2 "give new name"
			   :button3 "quit")
	  (:button1
	   (delete-file full-buffer-name))
	  (:button2
	   (setq a-buffer (read-string "new name: " :default a-buffer)))
	  (:button3
	   (if (eq (message-box "quit procedure"
				*SOBC-version-id*
				'(:ok-cancel :button2))
		   :ok)
	       (quit)))))

      (find-file (concat directory-name file-name "_book.html"))
      (goto-char (point-min))
      (scan-buffer "<!-- music title: ")
      (narrow-to-region (match-end 0) (point-max))
      (scan-buffer " -->")
      (narrow-to-region (point-min) (match-beginning 0))
      (setq music-title (buffer-substring (point-min) (point-max)))
      (delete-buffer (selected-buffer))

;      (setq music-title (read-string "music title: " :default music-title))

      (switch-to-buffer a-buffer)
      (set-buffer-file-name full-buffer-name)
      (let ((a-string nil))
	(insert (concat
		 "<!DOCTYPE html>\n"
		 "<html>\n"
		 "<!-- SOBC pseudo-index file -->\n"
		 "<!--\n"
		 "******************************************************************************\n"
		 "\tthis file is semi-automatically generated \n"
		 "\tby Street Organ Music Book Creator library \n"
		 "\t" *SOBC-version-id* "\n"
		 "\twords in paragraph \"about_this_music\" to be inserted manually\n"
		 "******************************************************************************\n"
		 "-->\n"
		 "<head>\n"
		 "<meta name=\"ROBOTS\" content=\"NOINDEX\">\n"
		 "<link rel=\"stylesheet\" type=\"text/css\" href=\"../css.css\">\n"
		 "<title>" music-title "</title>\n"
		 "</head>\n"
		 "<body>\n"
		 "<h1 id=\"music-title\">" music-title "</h1>\n"
		 "<p id=\"about_this_music\">\n\n</p>\n"
		 "<audio controls src=\"" file-name ".mp3\">\n"
		 "<p>音を鳴らそうとしたけれど、うまくできなかったみたい…</p>\n"
		 "</audio>\n"
		 "<ul>\n"
		 (dolist (file-elements file-set a-string)
		   (setq a-string
			 (concat a-string
				 "<li><a href=\"" file-name
				 (first file-elements) "."
				 (second file-elements) "\">"
				 (third file-elements) "</a>"
				 (fourth file-elements) "</li>\n")))
		 "</ul>\n"
		 "<p><a href=\"../SOBC-index.html\">曲目一覧に戻る</a></p>\n"
		 "</body>\n"
		 "</html>\n")))
      (save-buffer)
      (goto-char (point-min))
      (scan-buffer "<p id=\"about_this_music\">\n" :regexp t :tail t)
      (let ((a-file-list nil)
	    a-file-name)
	(dolist (file-elements file-set)
	  (unless (file-exist-p
		   (concat directory-name
			   (setq a-file-name
				 (concat file-name
					 (first file-elements) "."
					 (second file-elements)))))
	    (push a-file-name a-file-list)))
	(unless (file-exist-p
		 (concat directory-name
			 (setq a-file-name (concat file-name ".mp3"))))
	  (push a-file-name a-file-list))
	(setq *SOBC-missing-files-for-pseudo-index* (reverse a-file-list))
	(if *SOBC-missing-files-for-pseudo-index* (show-missing-files)))
      (if *chrome-path*
	  (call-process (concat *chrome-path* " " directory-name a-buffer)) ; open with chrome
	(progn
	  (message-box "*** chrome.exe not found ***\n\nopen with default browser instead\n"
		       *SOBC-version-id*
		       '(:ok :exclamation))
	  (shell-execute a-buffer)))))) ; if chrome not found, open with default browser

;;; when pseudo-index file of a music piece is saved, add a line to music list if not yet

(add-hook
 '*after-save-buffer-hook*
 #'(lambda ()
     (let ((file-name (file-namestring (get-buffer-file-name))))
       (save-excursion
	 (save-restriction
	   (goto-char (point-min))
	   (if (and (string-equal (pathname-type (get-buffer-file-name)) "html")
		    (scan-buffer "^<!-- SOBC pseudo-index file -->\n" :regexp t))
	       (let* ((full-file-name (get-buffer-file-name))
		      (directory-name (car (last (pathname-directory full-file-name))))
		      music-title
		      opening-tag-a-to-pseudo-index)

		 (find-file "../SOBC-index.html")
		 (setq opening-tag-a-to-pseudo-index
		       (format nil "<a href=\"~A/~A\">" directory-name file-name))
		 (goto-char (point-min))
		 (if (not (scan-buffer opening-tag-a-to-pseudo-index))
		     (when (eq (message-box "add a line to music list file?"
					    *SOBC-version-id*
					    '(:yes-no :button1 :exclamation))
			       :yes)
		       (switch-to-buffer file-name)
		       (goto-char (point-min))
		       (scan-buffer "<title>")
		       (narrow-to-region (match-end 0) (point-max))
		       (scan-buffer "</title>")
		       (narrow-to-region (point-min) (match-beginning 0))
		       (setq music-title (buffer-substring (point-min) (point-max)))
		       (switch-to-buffer "SOBC-index.html")
		       (goto-char (point-min))
		       (scan-buffer "<ul id=\"music_list\">")
		       (scan-buffer "</ul>")
		       (insert (concat
				"<li>"
				opening-tag-a-to-pseudo-index
				music-title
				"</a></li>\n"))
		       (scan-buffer "<li>" :reverse t) ; compatible whether <a> line exists
		       (save-buffer)
		       (if *chrome-path*
			   (call-process (concat *chrome-path* " " (get-buffer-file-name))) ; open with chrome
			 (shell-execute (get-buffer-file-name))))) ; if chrome not found, open with default browser
		 (scan-buffer "</a>" :tail t)))
	   (switch-to-buffer file-name))))))

(setq *SOBC-missing-files-for-pseudo-index* nil)

(defun show-missing-files () (interactive) ; 後掛かりで再表示できるように独立させておく
  (let ((a-file-list *SOBC-missing-files-for-pseudo-index*)
	(a-file-string nil))
    (while a-file-list
      (setq a-file-string (concat a-file-string "\n" (pop a-file-list))))
    (message-box (concat "files below not found:\n\n" a-file-string)
		 *SOBC-version-id*
		 '(:ok :exclamation))))

;;; modifying menu bar

(add-hook '*init-app-menus-hook*
	  #'(lambda ()
	      (insert-popup-menu
	       *app-menu*
	       (get-menu-position *app-menu* 'ed::help)
	       (define-popup-menu
		 (:item nil "xmlからsvg入りhtmlのbookを作成(&C)"
		  #'(lambda () (interactive)
		      (find-file
		       (file-name-dialog
			:title (concat *SOBC-version-id* ": open a file")
			:multiple nil
			:filter '(("XML(*.xml)" . "*.xml")
				  ("musicXml(*.musicxml)" . "*.musicxml")
				  ("全てのファイル(*.*)" . "*.*"))))))
		 (:item nil "book等のファイル名からpseudo-indexのhtmlを作成(&H)" 'create-pseudo-index-html)
		 (:item nil "足りなかったファイル名を再表示(&R)" 'show-missing-files
		  #'(lambda () (if (not *SOBC-missing-files-for-pseudo-index*) :disable))))
	       "SOBC(&B)")))

; end of "sobc.l"
