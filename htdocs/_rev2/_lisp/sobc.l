;;;; Street Organ Music Book Creator

;;;; converts xml sheet-music file of MuseScore into html file containing svg chart
;;;; for the street organs built and performed by the owing master
;;;; of a tiny hotel "Andante" in Hachimantai, Iwate prefecture

;;;; runs on xyzzy lisp 0.2.2.253
(setq *SOBC-version-id* "2.00b")
;;;; as of 05 August 2017

;;;; (c) ISHIZAKI Masato
;;;; mass.ishiz@gmail.com

;;;; ��񂵃I���K���p�u�b�N�̍E�ؔ����p�^�[�������y���f�[�^���琶������v���O����
;;;; ��茧�������̃y���V�����u���g��v����l����I���K�������ɍ쐬

;;;; MuseScore�ŏ����ꂽMusicXML�t�@�C�����A�u�b�N�p�^�[����svg�摜�ɕϊ����A
;;;; ���̉摜��A4�p���ɂP�y�[�W���������JavaScript���d����html�ɕ�܂�����
;;;; �g�����͉��Lurl���Q��
;;;; http://andante8mantai.ga/lisp/usage.html
;;;; ���ڂ����͂��̃\�[�X�����ǂݎ�肭������

;;;; 1.43 -> 2.00 �����_�i2017/05/06 ���g��ɂĎ��ۂ̃I���K�������t���āj
;;;; model III�̃u�b�N���葬�x(feed-rate)��1.5�{�i35 -> 53 mm/s�j��
;;;; �����������A���̍�  ���؂��(wipe-out-length)��0.5mm�傫���i2.5 -> 3.0�j
;;;; ����摜�̋Ȗ��ƃy�[�W�ԍ��̊ԂɃt�@�C�����A���̃v���O�����̔ŁA�ϊ�������ǉ�
;;;; �����œf���o���C���f�b�N�X�Ihtml�t�@�C����K�v���݂̂̃V���v���Ȍ`�ɕύX

;;; function create-svg
;;; main program of this library, to create an html file including an svg chart
;;; of Music Book for street organ
;;; the html file also includes some javascript functionalities to help printing

(defun create-svg ()
  
  ;; function focus (a-tag), to restrict elements inside a pair of tags
  ;; <a-tag>[make restriction from here]anything[to here]</a-tag>
  ;; should be used inside (save-restriction)

  (defun focus (a-tag)
    (if (scan-buffer (concat "<" a-tag "\\([ \\t\\n]*>\\|[ \\t\\n]+[^>]*>\\)") :regexp t)
	(narrow-to-region (match-end 0) (point-max))
      (return-from focus nil))
    (if (scan-buffer (concat "</" a-tag ">"))
	(narrow-to-region (point-min) (match-beginning 0))
      (return-from focus nil))
    (goto-char (point-min)))

  ;; function (get-integer)
  ;; in the region focused by the above function, get the value if strings suggesting integer
  ;; ��Lfocus����region�Ɂu������\��������v������Ύ���ė���

  (defun get-integer ()
    (scan-buffer "[-\+]?[0-9]+" :regexp t)
    (parse-integer (match-string 0)))

;;; main function

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
	(tenuto 0.87) ; geometric sequence down to 0.5, should correspond with "instrument.xml", default was 1
	(detached-legato 0.76) ; was 0.67
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
	top-tone-hole-offset
	tone-hole-pitch
	rect-height
	wipe-out-length
	black-keys
	keys
	thick
	exceptionals
	bottom-y
	measure-number-y
	max-measure
	div-count
	virgin
	measure-div
	(regular-measure-div nil)
	div-time
	temp-div-time
	notes
	auftakt
	(mit-auftakt nil)
	a-tag
	tempo
	a-div-time
	temp-div-time
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
	work-title)

    (case (message-box "select model" nil '(:button3)
		       :button1 "model I"
		       :button2 "model II"
		       :button3 "model III")
      (:button3
       (setq model 3 ; for model III
	     book-height 170 ; mm
	     feed-rate 53 ; mm/s
	     top-tone-pitch-number 80 ; highest G
	     top-tone-hole-offset 6.8 ; mm, was 6.6
	     tone-hole-pitch 4.226 ; mm, was (* 3 (expt 2 0.5))
	     rect-height 2.5 ; mm
	     wipe-out-length 3.0 ; mm
	     black-keys '(45 47 50 52 55 57 59 62 64 67 69 71 74 76 79)
	     keys 37
	     thick '(8 20 32)))
      (:button2
       (setq model 2 ; for model II
	     book-height 150 ; mm
	     feed-rate 40 ; mm/s
	     top-tone-pitch-number 80 ; highest G
	     top-tone-hole-offset 14.6 ; mm
	     tone-hole-pitch 4.226 ; mm, was (* 3 (expt 2 0.5))
	     rect-height 2.5 ; mm
	     wipe-out-length 2.8 ; mm
	     black-keys '(55 57 59 62 64 67 69 71 74 76 79)
	     keys 31
	     thick '(8 20 27 28 29 30 31)
	     exceptionals '((50 "F") (51 "G") (52 "A") (53 "C"))))
      (:button1
       (setq model 1 ; for model I
	     book-height 114.3 ; mm
	     feed-rate 40 ; mm/s
	     top-tone-pitch-number 80 ; highest G
	     top-tone-hole-offset 3.5 ; mm
	     tone-hole-pitch 4.233 ; mm, 1/6 inch
	     rect-height 2.5 ; mm
	     wipe-out-length 2.8 ; mm
	     black-keys '(57 59 62 64 67 69 71 74 76 79)
	     keys 25
	     thick '(8 20))))

    (setq bottom-y (+ offset-y book-height)
	  measure-number-y (- (+ offset-y book-height) measure-number-margin-bottom)
	  wipe-width (- wipe-out-length note-stroke-width)
	  margin-y (+ top-tone-hole-offset (/ (- rect-height tone-hole-pitch) 2) offset-y)
	  wipe-out-stroke-width (+ note-stroke-width 0.08) ; mm
	  input-file-name (get-buffer-file-name)
	  input-buffer (file-namestring input-file-name)
	  output-file-name (concat (directory-namestring input-file-name)
				   (setq SOBC-simple-file-name
					 (pathname-name input-file-name))
				   "_book.html")
	  output-buffer (file-namestring output-file-name))

    (if (file-exist-p output-file-name)
	(if (yes-or-no-p (concat "\"" output-buffer "\" already exists. \n"
				 "Wanna renew?"))
	    (delete-file output-file-name)
	  (error "quit procedure")))
    (create-file-buffer output-file-name)

;;; first of all: get the title and length (measures) of music
;;; �ŏ��ɂ�邱�ƁF �Ȗ���measure�̍ő吔�𓾂�

    (message "pre-processing")
    (switch-to-buffer input-buffer)
    (goto-char (point-min))
    (save-restriction
      (if (focus "work-title")
	  (setq work-title (buffer-substring (point-min) (point-max)))
	(setq work-title SOBC-simple-file-name)))
    (goto-char (point-max))
    (scan-buffer "<measure[ \\t\\n]*\\([^>]*\\)*>" :regexp t :reverse t)
    (save-restriction
      (narrow-to-region (match-beginning 0) (match-end 0))
      (scan-buffer "number[ \\t\\n]*=[ \\t\\n]*")
      (setq max-measure (get-integer)))

;;; get started to fill output buffer with html strings
    (switch-to-buffer output-buffer)
    (insert
     (concat
      "<!DOCTYPE html>\n"
      "<html>\n"
      "<!-- generated by \"Street Organ Music Book Creator\" ver. " *SOBC-version-id*
      (format-date-string " at %Y/%m/%d %H:%M:%S %#Z UTC -->\n")
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
      "}\n\n"
      "</style>\n"
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
      "}\n\n"
      "</script>\n"
      "<title>" SOBC-simple-file-name "_book</title>\n"
      "</head>\n"
      "<body>\n"
      (format nil "<svg width=\"~Fmm\" height=\"~Fmm\" id=\"book\" viewBox=\"0 0 ~F ~F\">\n\n"
	      page-width page-height page-width page-height)
      "<!-- keyboard mimics -->\n")) ; svg elements inserted in the end of this program


;;; main loop: iterate for each measure
;;; make a list of tone-hole information from each note (ex rest) in the measure
;;; get divisions from attributes (just once at the beginning?)
;;; clasify to handle direction, note, backup and forward
;;; add tone information to the list if note but not rest
;;; handle div-count properly

;;; main loop: �emeasure�ŉ�
;;; measure�̒��g��note�irest�ȊO�j�̍E���̃��X�g�ɗ��Ƃ�
;;; attributes����divisions���擾�i�Ȃ̓���1�x�����H�j
;;; direction, note, backup, forward���d��������
;;; note(��rest�łȂ�)�Ȃ�list��note����ǉ�
;;; �K�Xdiv-count�𑀍�

    (switch-to-buffer input-buffer)
    (goto-char (point-min))
    (setq notes (make-array (1+ max-measure)) ; auftakt�̂Ƃ��̂�0������
	  a-div-time 1) ; setting dummy parameters initially
    (while (scan-buffer "<measure[ \\t\\n]*\\([^>]*\\)*>" :regexp t) ; ����measure�̔ԍ����E��
      (save-excursion
	(save-restriction
	  (narrow-to-region (match-beginning 0) (match-end 0))
	  (scan-buffer "number[ \\t\\n]*=[ \\t\\n]*" :regexp t)
	  (setq measure (get-integer)
		auftakt (zerop measure)) ; ���ݒn��auftakt���ǂ����𔻒�
	  (if auftakt (setq mit-auftakt t)))) ; ���̋Ȃ�auftakt�����邩�ǂ������Z�b�g�@��̍s�Ƃ܂Ƃ߂Ă͂Ȃ�Ȃ�
      (message (format nil "processing measure ~D / ~D" measure max-measure))
      (setq div-count 0 virgin t) ; div-count: measure�̒��Ō��݈ʒu����肷��ϐ�
      (if (and mit-auftakt (= measure 1))
	  (setq measure-div regular-measure-div))
      (save-restriction
	(focus "measure")
	(while (scan-buffer "<\\([^ />]+\\)" :regexp t)
	  (setq a-tag (match-string 1))
	  (cond
	   ((string= a-tag "attributes")
	    (when virgin
	      (dolist (var-name '(divisions fifths beats beat-type))
		(save-restriction
		  (goto-char (point-min))
		  (when (focus (string var-name))
		    (set var-name (get-integer)))))
	      (setq measure-div (/ (* 4 divisions beats) beat-type)
		    regular-measure-div measure-div
		    div-time (make-array measure-div :initial-element 0)
		    temp-div-time 0)
	      (dotimes (i measure-div)
		(setf (aref div-time i)
		      (incf temp-div-time a-div-time)))
	      (setq virgin nil))
	    (scan-buffer "</attributes>" :tail t))

	   ((string= a-tag "direction")
	    (save-restriction
	      (focus "direction")
	      (when (scan-buffer "sound tempo[ \\t\\n]*=[ \\t\\n]*\"\\([0-9]+[\.0-9]*\\)\"/>" :regexp t)
		(setq tempo (read-from-string (match-string 1))
		      a-div-time (/ 60 tempo divisions)
		      temp-div-time (if (zerop div-count) 0
				      (aref div-time (1- div-count))))
		(dotimes (i (- measure-div div-count))
		  (setf (aref div-time (+ div-count i))
			(incf temp-div-time a-div-time)))))
	    (scan-buffer "</direction>" :tail t))

	   ((string= a-tag "note")
	    (save-restriction
	      (focus "note")
	      (setq virgin nil)
	      (save-restriction
		(focus "duration")
		(setq duration (get-integer)))
	      (goto-char (point-min))
	      (save-restriction
		(unless (scan-buffer "<rest[ \\t\\n/>]" :regexp t)
		  (if (setq slave (scan-buffer "<chord[ \\t\\n/>]" :regexp t))
		      (decf div-count duration))
		  (goto-char (point-min))
		  (save-restriction
		    (focus "step")
		    (looking-at ".")
		    (setq pitch-number
			  (case (elt (match-string 0) 0)
			    (#\C 1) (#\D 3) (#\E 5) (#\F 6) (#\G 8) (#\A 10) (#\B 12)
			    (otherwise (error "pitch-name unidentified")))))
		  (goto-char (point-min))
		  (save-restriction
		    (focus "octave")
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

		  (case model
		    (2 ; �ቹ���́u�点�Ȃ����v�ւ̗�O����
		     (if (<= pitch-number 53)
			 (setq pitch-number
			       (case pitch-number
				 ((43 45 47 48 50 51 52 53) (error "missing pipe \nfor model II"))
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
	      (focus "duration")
	      (decf div-count (get-integer)))
	    (scan-buffer "</backup>" :tail t))

	   ((string= a-tag "forward")
	    (save-restriction
	      (focus "forward")
	      (focus "duration")
	      (incf div-count (get-integer)))
	    (scan-buffer "</forward>" :tail t))

	   (t (scan-buffer ">")
	      (if (looking-back "/")
		  (forward-char)
		(scan-buffer (concat "</" a-tag ">") :tail t)))))

	(if auftakt (setq measure-div div-count auftakt nil)))
      (scan-buffer "</measure>" :tail t)

      ;; list (of list of information) of notes is ready up to here
      ;; �����܂łŁAmeasure����(note(�̗v�f���list)��list)���ł���

      ;; sort all notes in the measure by beginning time (= car) first, to treat tied tones properly
      (setf (aref notes measure) (sort (aref notes measure) #'< :key #'car))

;;; objectives: make a large (long) svg image (virtual)
;;; move ViewBox from page to page by JavaScript in outer html
;;; to obtain printing image on A4 sheet papers
;;; (or print to pdf image with real dimensions for paper-printing)
      
;;; ���j�F�傫���P��svg�摜���쐬���Ă����āA
;;; �O����html�Ɏd����JavaScript��ViewBox�����X�Ɠ�������
;;; A4�����y�[�W�̈���C���[�W�𓾂�
;;; �i�������pdf�����Č����摜�𓾂邱�Ƃ��ł���j

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
	       (format nil "<text class=\"file-name\" x=\"~F\" y=\"~F\">~A [~A, v~A "
		       (+ offset-x (* i page-width)) (- offset-y 4.5)
		       work-title SOBC-simple-file-name *SOBC-version-id*)
	       (format-date-string "%Y/%m/%d %H:%M:%S")
	       (format nil "] page ~D</text>\n"
		       (1+ i)))))
      "\n</svg>\n\n"
      "<div class=\"noprint\" id=\"footer\">\n"
      "<p><a href=\"#\" onClick=\"javascript: firstPage()\">�ŏ��̃y�[�W</a> | \n"
      "<a href=\"#\" onClick=\"javascript: previousPage()\">�O�̃y�[�W</a> | \n"
      "<a href=\"#\" onClick=\"javascript: nextPage()\">���̃y�[�W</a> | \n"
      "<a href=\"#\" onClick=\"javascript: lastPage()\">�Ō�̃y�[�W</a></p>\n"
      "<p><a href=\"#\" onClick=\"javascritp: window.print()\">���̃y�[�W�����</a> | \n"
      "<a href=\"#\" onClick=\"javascript: window.print();nextPage()\">���̃y�[�W��������Ď��̃y�[�W��</a> | \n"
      "<a href=\"#\" onClick=\"javascript: printUntilEnd()\">���̃y�[�W����Ō�܂ł����X�ƈ��</a> | \n"
      "<a href=\"#\" onClick=\"javascript: printAll()\">�S�y�[�W�����X�ƈ��</a></p>\n"
      "<p><a href=\"#\" onClick=\"javascript: toggleKey()\">���Օ\���ؑ�</a></p>\n"
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
		       (if (find i thick) 0 1)
		       (setq a-key-line (+ margin-y (* tone-hole-pitch i)))
		       measure-end a-key-line))))
      (case model
	(2 ; exceptional handling for model II
	 (setq a-string nil)
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
			      x y tone-name))))))))
      "<!-- end of keyborad mimics -->\n\n"))

    (save-buffer) ; save output file
    (message "done")

    (if *chrome-path*
	(call-process (concat *chrome-path* " " output-file-name)) ; open with chrome
      (progn
	(msgbox "*** chrome.exe not found ***\nopen with default browser instead")
	(shell-execute output-file-name))) ; if chrome not found, open with default browser

;    (delete-buffer input-buffer) ; this line causes an error: "�폜���ꂽ�o�b�t�@�ł�"
    (delete-buffer output-buffer)
    ;(setq *last-created-work-title* work-title) ; handling work title to html creator and close '(defun create-svg ()

    ))
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

;;; function to activate when .xml file is loaded

(defun xmlp ()
  (if (string= (pathname-type (get-buffer-file-name)) "xml")
      (if (scan-buffer "<software>MuseScore 2.")
	  (if (yes-or-no-p "Create a chart pattern of the book \nfrom this .xml file?")
	      (create-svg)
	    (goto-char (point-min))))))
(setq *find-file-hooks* '(xmlp))

;;; function to create an html index-like file to put on the web
;;; �t�@�C���Ńt�@�C�����w��
;;; ���̖��O�̈�A�̃t�@�C���Q�ւ̃����N�̃��X�g��L����html��f���o��

(defun create-organbook-html () (interactive)
  (let (full-file-name)
    (when (setq full-file-name (file-name-dialog
				:title "select any related file"
				:multiple nil))
      (let* ((file-name (pathname-name full-file-name))
	     (directory-name (directory-namestring full-file-name))
	     (file-name-length (length file-name))
	     (file-set '(
			 ("_book" "html" "�u�b�N�p�^�[��" "(html)�@��Chrome�ł̈���͂�������ǂ���")
			 (nil "pdf" "����" "(pdf)")
			 ;("_book" "pdf" "�u�b�N�p�^�[��" "(pdf)")
			 ;(nil "mp3" "�Q�l����" "(mp3)")
			 ;(nil "xml" "���ʂ̌��t�@�C���̃e�L�X�g�f�[�^" "(xml)")
			 (nil "mscz" "���ʂ̌��t�@�C��" "�i�v<a href=\"https://musescore.org/ja\">MuseScore</a>�j"))))
	(if (string-looking-at "_book" file-name :start (decf file-name-length 5))
	    (setq file-name (subseq file-name 0 file-name-length)))
	(if (file-exist-p
	     (setq full-buffer-name
		   (concat directory-name
			   (setq a-buffer (concat file-name ".html")))))
	  (if (yes-or-no-p ".html already exists. \nWanna renew?")
	      (delete-file full-buffer-name)
	    (error "quit procedure")))
      (create-file-buffer full-buffer-name)
      (switch-to-buffer a-buffer)
      (let ((title (read-string "title: " :default file-name))
	    (a-string nil))
	(insert (concat
		 "<!DOCTYPE html>\n"
		 "<html>\n"
		 "<head>\n"
		 "<meta name=\"ROBOTS\" content=\"NOINDEX\">\n"
		 "<link rel=\"stylesheet\" type=\"text/css\" href=\"../css.css\">\n"
		 "<title>" title "</title>\n"
		 "</head>\n"
		 "<body>\n"
		 "<h3>" title "</h3>\n"
		 "<audio controls src=\"" file-name ".mp3\">\n"
		 "<p>����炻���Ƃ�������ǁA���܂��ł��Ȃ������݂����c</p>\n"
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
		 "<p><a href=\"../index0.html\">�Ȗڈꗗ�ɖ߂�</a></p>\n"
		 "</body>\n"
		 "</html>\n")))
      (save-buffer)
;      (delete-buffer a-buffer)
      (let ((a-file-list nil))
	(dolist (file-elements file-set)
	  (unless (file-exist-p
		   (concat directory-name
			   (setq a-file-name
				 (concat file-name
					 (first file-elements) "."
					 (second file-elements)))))
	    (push a-file-name a-file-list)))
	(setq *missing-files-for-street-organ-html-index* (reverse a-file-list))
	(if *missing-files-for-street-organ-html-index* (show-missing-files)))
      (if *chrome-path*
	  (call-process (concat *chrome-path* " " directory-name a-buffer)) ; open with chrome
	(progn
	  (msgbox "*** chrome.exe not found ***\nopen with default browser instead")
	  (shell-execute a-buffer))))))) ; if chrome not found, open with default browser

(setq *missing-files-for-street-organ-html-index* nil)

(defun show-missing-files () (interactive) ; ��|����ōĕ\���ł���悤�ɓƗ������Ă���
  (let ((a-file-list *missing-files-for-street-organ-html-index*)
	(a-file-string nil))
    (while a-file-list
      (setq a-file-string (concat a-file-string "\n" (pop a-file-list))))
    (msgbox (concat "�ȉ��̃t�@�C��������܂���ł����F\n" a-file-string))))

;;; modifying menu bar

(add-hook '*init-app-menus-hook*
	  #'(lambda ()
	      (insert-popup-menu
	       *app-menu*
	       (get-menu-position *app-menu* 'ed::help)
	       (define-popup-menu
		 (:item nil "xml����svg����html��book���쐬(&C)"
		  #'(lambda () (interactive)
		      (find-file
		       (file-name-dialog :title "select a file"
					 :multiple nil
					 :filter '(("XML(*.xml)" . "*.xml"))))))
		 (:item nil "book������web�pindex��html���쐬(&H)" 'create-organbook-html)
		 (:item nil "����Ȃ������t�@�C�������ĕ\��(&R)" 'show-missing-files
		  #'(lambda () (if (not *missing-files-for-street-organ-html-index*) :disable))))
	       "SOBC(&B)")))

; end of "sobc.l"