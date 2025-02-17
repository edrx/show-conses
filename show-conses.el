;; This file:
;;   http://anggtwu.net/show-conses/show-conses-2.el.html
;;   http://anggtwu.net/show-conses/show-conses-2.el
;;          (find-angg "show-conses/show-conses-2.el")
;; Author: Eduardo Ochs <eduardoochs@gmail.com>
;;
;; (defun sc1 () (interactive) (find-angg "show-conses/show-conses.el"))
;; (defun sc2 () (interactive) (find-angg "show-conses/show-conses-2.el"))
;; (load (buffer-file-name))
;;
;; «.sc-:do1»			(to "sc-:do1")
;; «.makeconcat»		(to "makeconcat")
;; «.colors»			(to "colors")
;; «.sc-show»			(to "sc-show")
;; «.markers»			(to "markers")
;; «.overlay»			(to "overlay")
;; «.:m1»			(to ":m1")
;; «.insert»			(to "insert")
;; «.show-regions»		(to "show-regions")
;; «.rect-objects»		(to "rect-objects")
;; «.:toplain»			(to ":toplain")
;; «.insertrects»		(to "insertrects")
;; «.hpad-and-vpad»		(to "hpad-and-vpad")
;; «.glueing-rects»		(to "glueing-rects")
;;
;; «.:expand»			(to ":expand")
;; «.:m»			(to ":m")
;; «.:sexp»			(to ":sexp")
;; «.setoverlay-buttons»	(to "setoverlay-buttons")
;; «.typedescr-buttons»		(to "typedescr-buttons")
;; «.:sexptree»			(to ":sexptree")
;; «.find-show-conses»		(to "find-show-conses")
;; «.typedescr»			(to "typedescr")
;; «.find-classtree»		(to "find-classtree")


;; See: (find-eev "eev-load.el")
;;      (find-eev-levels-intro)
(require 'eev-load)
(require 'cl-extra)



;;;                    _       _ 
;;;  ___  ___     _ __| | ___ / |
;;; / __|/ __|___(_) _` |/ _ \| |
;;; \__ \ (_|_____| (_| | (_) | |
;;; |___/\___|   (_)__,_|\___/|_|
;;;                              
;; «sc-:do1»  (to ".sc-:do1")
;; (sc-:do1 "foo")
;; (sc-:do1 123456)
;; (sc-:do1 'asymbol)
;; (sc-:do1 '(:do1 "foo"))
;; (sc-:do1 '(:bar "foo"))
;; (sc-:do1 '(plic "foo"))
;;
(defun sc-:do1 (o)
  (cond ((stringp o) o)
	((keywordp (car-safe o))
	 (apply (ee-intern "sc-%s" (car o)) (cdr o)))
	(t (error "sc-:do1 is confused by: %S" o))))

;; (insert "\n" (sc-:face1 'warning "foo"))
;; (insert "\n" (sc-:fg1 "red" "foo"))
;; (insert "\n" (sc-:fg1 'red "aaa") (sc-:fg1 'yellow "bbb"))
(defun sc-:face1 (face str) (propertize str 'font-lock-face face))
(defun sc-:face1 (face str) (propertize str 'font-lock-face face 'face face))
(defun sc-:fg1  (color str) (sc-:face1 `(:foreground ,(format "%s" color)) str))


;;;  __  __       _                                  _   
;;; |  \/  | __ _| | _____  ___ ___  _ __   ___ __ _| |_ 
;;; | |\/| |/ _` | |/ / _ \/ __/ _ \| '_ \ / __/ _` | __|
;;; | |  | | (_| |   <  __/ (_| (_) | | | | (_| (_| | |_ 
;;; |_|  |_|\__,_|_|\_\___|\___\___/|_| |_|\___\__,_|\__|
;;;                                                      
;; «makeconcat»  (to ".makeconcat")
;;
(defmacro sc-makeconcat (keyword arglist stringcmd)
  `(defun ,(ee-intern "sc-%s" keyword) (,@arglist &rest objs)
     (cl-loop for o in objs
	      concat (if (stringp o)
			 ,stringcmd
		       (sc-:do1 o)))))

;; To understand `sc-makeconcat',
;; compare:
' (find-2a nil '(find-eppm '(sc-makeconcat :fg (color) (sc-:fg1 color o))))
;; with:
' (defun sc-:fg (color &rest objs)
    (cl-loop for o in objs
	     concat (if (stringp o)
			(sc-:fg1 color o)
		      (sc-:do1 o))))

;;;   ____      _                
;;;  / ___|___ | | ___  _ __ ___ 
;;; | |   / _ \| |/ _ \| '__/ __|
;;; | |__| (_) | | (_) | |  \__ \
;;;  \____\___/|_|\___/|_|  |___/
;;;                              
;; «colors»  (to ".colors")
;; To understand the words made by `sc-makeconcat', try:
;;
;; (insert "\n" (sc-:do1                     '(:fg green  "c" "d")))
;; (insert "\n" (sc-:do1                        '(:green  "c" "d")))
;; (insert "\n" (sc-:do1           '(:yellow "b" (:green  "c" "d") "e")))
;; (insert "\n" (sc-:do1 '(:red "a" (:yellow "b" (:green  "c" "d") "e") "f")))
;; (insert "\n" (sc-:do1 '(:red "a" (:yellow "b" (:concat "c" "d") "e") "f")))
;;
(sc-makeconcat :concat ()      o)
(sc-makeconcat :face   (face)  (sc-:face1 face o))
(sc-makeconcat :fg     (color) (sc-:fg1 color o))
(sc-makeconcat :red    ()      (sc-:fg1 'red o))
(sc-makeconcat :blue   ()      (sc-:fg1 'blue o))
(sc-makeconcat :green  ()      (sc-:fg1 'green o))
(sc-makeconcat :yellow ()      (sc-:fg1 'yellow o))



;;;                    _                   
;;;  ___  ___      ___| |__   _____      __
;;; / __|/ __|____/ __| '_ \ / _ \ \ /\ / /
;;; \__ \ (_|_____\__ \ | | | (_) \ V  V / 
;;; |___/\___|    |___/_| |_|\___/ \_/\_/  
;;;                                        
;; «sc-show»  (to ".sc-show")

(defun sc-last-sexp ()
  (ee-eval-last-sexp-0)			; highlight
  (read (ee-last-sexp)))

(defun sc-show (sexp)
  (interactive (list (sc-last-sexp)))
  (find-2a nil `(find-estring (sc-:do1 ',sexp) :end)))

(defun sc-message (sexp)
  (interactive (list (sc-last-sexp)))
  (message "%s" (sc-:do1 sexp)))

(defalias 'scs 'sc-show)
(defalias 'scm 'sc-message)

;; Try `M-x scs' and `M-x scm' at various points of:
;; (:red "a" (:yellow "b" (:concat "c" "d") "e") "f")



;; (find-showconses "show-conses.el" "markers")

;;;  __  __            _                 
;;; |  \/  | __ _ _ __| | _____ _ __ ___ 
;;; | |\/| |/ _` | '__| |/ / _ \ '__/ __|
;;; | |  | | (_| | |  |   <  __/ |  \__ \
;;; |_|  |_|\__,_|_|  |_|\_\___|_|  |___/
;;;                                      
;; «markers»  (to ".markers")
;; Test:
;; (sc-:deleteallmarkers)
;; (sc-:setmarker "a")
;; (sc-:setmarker "b")
;; (find-ehashtable sc-markers)
;; (sc-:getmarker "a")
;; (sc-:getmarker "err")
;;
(defvar sc-markers (make-hash-table :test 'equal))

(defun sc-:deleteallmarkers ()
  (setq sc-markers (make-hash-table :test 'equal)))

(defun sc-:deletemarker (name)
  (remhash name sc-markers))

(defun sc-:getmarker (name)
  (or (gethash name sc-markers)
      (error (format "Failed: (get-hash %S sc-markers)" name))))

(defun sc-:setmarker (name &optional pos buffer)
  (with-current-buffer (or buffer (current-buffer))
    (if pos (goto-char pos))
    (puthash name (point-marker) sc-markers)))


;;;   ___                 _             
;;;  / _ \__   _____ _ __| | __ _ _   _ 
;;; | | | \ \ / / _ \ '__| |/ _` | | | |
;;; | |_| |\ V /  __/ |  | | (_| | |_| |
;;;  \___/  \_/ \___|_|  |_|\__,_|\__, |
;;;                               |___/ 
;; «overlay»  (to ".overlay")
;; We use a single overlay to highlight parts of a sexp -
;; and we store it in the variable `sc-overlay'. Tests:
;; (sc-:deleteoverlay)
;; (sc-:setoverlay3 (+ 2 (ee-bol)) (+ 4 (ee-bol)))
;; (sc-:setoverlay3 (+ 5 (ee-bol)) (+ 9 (ee-bol)))
;; (sc-:deleteoverlay)
;; (sc-:setmarker "a")
;; (sc-:setmarker "b")
;; (sc-:setmarker "c")
;; (sc-:setoverlay2 "a" "b")
;; (sc-:setoverlay2 "a" "c")
;; (sc-:setoverlay2 "b" "c")
;; (sc-:deleteoverlay)
;; (sc-:setmarker "foo beg")
;; (sc-:setmarker "foo end")
;; (sc-:setoverlay1 "foo")
;; (sc-:deleteoverlay)
;;
(defvar sc-overlay nil)

(defun sc-:deleteoverlay ()
  "Delete the overlay `sc-overlay' if it exists."
  (if (overlayp sc-overlay)
      (delete-overlay sc-overlay)))

(defun sc-:setoverlay3 (start end &optional buffer)
  "A low-level function called by `sc-:setoverlay2'."
  (setq buffer (or buffer (current-buffer)))
  (sc-:deleteoverlay)
  (setq sc-overlay
	(with-current-buffer buffer
	  (make-overlay start end)))
  (overlay-put sc-overlay 'face 'highlight))

(defun sc-:setoverlay2 (start end)
  "Make `sc-:setoverlay3' highlight the region between START and END.
START and END should be strings and should be the names of two markers
in `sc-markers' that point to the same buffer."
  (let* ((startmarker (sc-:getmarker start))
	 (startpos    (marker-position    startmarker))
	 (startbuf    (marker-buffer      startmarker))
	 (endmarker   (sc-:getmarker end))
	 (endpos      (marker-position    endmarker))
	 (endbuf      (marker-buffer      endmarker)))
    (if (not (eq startbuf endbuf))
	(error (format "Markers %S and %S are in different buffers!"
		       start end)))
    (sc-:setoverlay3 startpos endpos startbuf)))

(defun sc-:setoverlay1 (stem)
  "Like `sc-:setoverlay2', but receives a single argument."
  (sc-:setoverlay2 (format "%s beg" stem)
		   (format "%s end" stem))
  "")


;;;              _ 
;;;  _ _ __ ___ / |
;;; (_) '_ ` _ \| |
;;;  _| | | | | | |
;;; (_)_| |_| |_|_|
;;;                
;; «:m1»  (to ".:m1")

(defvar sc-:m1 'setmarkers
  "When the variable `sc-:m1' is nil the function `sc-:m1'
doesn't run `sc-:setmarker'.")

(defun sc-:m1 (name)
  (if sc-:m1
      (sc-:setmarker (format "%s" name))) ; optional
  "")					  ; always returns ""
  
(defun sc-:concat-nom (&rest rest)
  "Like `:concat', but \"with no markers\".
This function doesn't run the `sc-:setmarker's in the `sc-:m1's."
  (let ((sc-:m1 nil)) (apply 'sc-:concat rest)))




;;;  ___                     _   
;;; |_ _|_ __  ___  ___ _ __| |_ 
;;;  | || '_ \/ __|/ _ \ '__| __|
;;;  | || | | \__ \  __/ |  | |_ 
;;; |___|_| |_|___/\___|_|   \__|
;;;                              
;; «insert»  (to ".insert")

(defun sc-:insert (&rest objs)
  (cl-loop for o in objs
	   do (insert (sc-:do1 o)))
  "")

;; Test:
;; (sc-:do1 '(:insert "\n;; " (:m1 abL) "ab" (:m1 abR) (:m1 cdL) "cd" (:m1 cdR)))
;; (sc-:setoverlay2 "abL" "abR")
;; (sc-:setoverlay2 "cdL" "cdR")
;; (sc-:deleteoverlay)




;;;  ____  _                                    _                 
;;; / ___|| |__   _____      __  _ __ ___  __ _(_) ___  _ __  ___ 
;;; \___ \| '_ \ / _ \ \ /\ / / | '__/ _ \/ _` | |/ _ \| '_ \/ __|
;;;  ___) | | | | (_) \ V  V /  | | |  __/ (_| | | (_) | | | \__ \
;;; |____/|_| |_|\___/ \_/\_/   |_|  \___|\__, |_|\___/|_| |_|___/
;;;                                       |___/                   
;;
;; «show-regions»  (to ".show-regions")
;; Test:
;; (find-2a nil '(find-ehashtable sc-markers))
;; (sc-:deleteallmarkers)
;; (find-2a nil '(find-ehashtable sc-markers))
;; (sc-:setmarker "foo beg")
;; (sc-:setmarker "foo end")
;; (sc-:setmarker "bar beg")
;; (sc-:setmarker "bar end")
;; (find-2a nil '(find-showregions))
;;
(defun sc-region-names ()
  "Return all the names of sc-regions, sorted."
  (let* ((stems (cl-loop for k being the hash-keys of sc-markers
			 if (string-match " beg$" k)
			 collect (replace-regexp-in-string " beg$" "" k))))
    (sort stems 'string<)))

(defun find-showregions ()
  "Show a temporary buffer with commands to highlight all the sc-regions."
  (let* ((names (sc-region-names))
	 (sexps (cl-loop for name in names
			 collect `(sc-:setoverlay1 ,name))))
    (find-elinks-elisp
     `((find-sc-regions)
       (find-efunction 'find-sc-regions)
       ""
       ,(ee-ppp00 sexps)
       "(sc-:deleteoverlay)"
       ))))



;;;  ____           _           _     _           _       
;;; |  _ \ ___  ___| |_    ___ | |__ (_) ___  ___| |_ ___ 
;;; | |_) / _ \/ __| __|  / _ \| '_ \| |/ _ \/ __| __/ __|
;;; |  _ <  __/ (__| |_  | (_) | |_) | |  __/ (__| |_\__ \
;;; |_| \_\___|\___|\__|  \___/|_.__// |\___|\___|\__|___/
;;;                                |__/                   
;;
;; «rect-objects»  (to ".rect-objects")
;; A `:line' object is a list that can be concatenated by `sc-:concat'.
;; A `:lines' object is a list of `:line's.
;; A `:rect' object is a `:lines' preceded by the symbol `:rect'.
;; In this example,
;;
;;   (setq o1 '("a" "b"))
;;   (setq o2 '("a" (:red "b" "c") "d"))
;;   (setq o3 (sc-:expand '("a" (:@m "bc" "b" "c") "d")))
;;   (setq o4       `(,o1 ,o2 ,o3))
;;   (setq o5 `(:rect ,o1 ,o2 ,o3))
;;
;; `o1', `o2' and `o3' are `:line's, `o4' is a `:lines', and `o5' is a
;; `:rect'.
;;
;; A `:preline' object is either a string or a `:line' object.
;; A `:prerect' object is either a string or a `:line' or a `:rect'.
;; The function `sc-:toline' converts a `:preline' to a `:line'.
;; The function `sc-:torect' converts a `:prerect' to a `:rect'.
;; The function `sc-:tolines' converts a `:prerect' to a `:lines'.
;; Try:
;;
;;                (sc-:torect '(:rect ("a" "b") ("c" "d")))
;;   (sc-:tolines (sc-:torect '(:rect ("a" "b") ("c" "d"))))
;;                (sc-:torect        '("a" "b"))
;;   (sc-:tolines (sc-:torect        '("a" "b")))
;;                (sc-:torect          "a")
;;   (sc-:tolines (sc-:torect          "a"))
;;
(defun sc-:rectp   (o) (if (eq (car-safe o) ':rect) t))
(defun sc-:toline  (o) (if (atom o) (list o) o))
(defun sc-:torect  (o) (if (sc-:rectp o) o `(:rect ,(sc-:toline o))))
(defun sc-:tolines (o) (cdr (sc-:torect o)))



;;;    _              _       _       
;;;  _| |_ ___  _ __ | | __ _(_)_ __  
;;; (_) __/ _ \| '_ \| |/ _` | | '_ \ 
;;;  _| || (_) | |_) | | (_| | | | | |
;;; (_)\__\___/| .__/|_|\__,_|_|_| |_|
;;;            |_|                    
;;
;; «:toplain»  (to ".:toplain")
;; The functions `sc-*-line', below are very strict in whay they
;; accept - they only accept `:line's, and they fail when o is a
;; string.
;;
(defun sc-:totext-line  (o) (let ((sc-:m1 nil)) (apply 'sc-:concat o)))
(defun sc-:totext-line  (o) (apply 'sc-:concat-nom o))
(defun sc-:toplain-line (o) (ee-no-properties (sc-:totext-line o)))
(defun sc-:width-line   (o) (length (sc-:toplain-line o)))

;; The functions below are variants of the `sc-*-line's above that are
;; very lenient in what they accept - they accept a `:prerect' and
;; they convert it internally to a `:rect'.
;;
(defun sc-:totext (o)
  (let* ((lines1 (sc-:tolines o))
	 (lines2 (cl-loop for line in lines1
			  collect (sc-:totext-line line))))
    (mapconcat 'identity lines2 "\n")))

(defun sc-:toplain (o)
  (let* ((lines1 (sc-:tolines o))
	 (lines2 (cl-loop for line in lines1
			  collect (sc-:toplain-line line))))
    (mapconcat 'identity lines2 "\n")))

(defun sc-:width (o)
  (let* ((lines  (sc-:tolines o))
	 (widths (mapcar 'sc-:width-line lines)))
    (apply 'max widths)))

;; The function `sc-:pile' below accepts a list of `:prerects'
;; and returns a `:rect'. Try:
;;               (sc-:pile "a" '("b" "c") '(:rect ("d") ("e" "f")))
;;  (sc-:toplain (sc-:pile "a" '("b" "c") '(:rect ("d") ("e" "f"))))
;;               (sc-:pile "a")
;;     (sc-:pile (sc-:pile "a"))
;;
(defun sc-:pile (&rest objs)
  (let* ((liness (mapcar 'sc-:tolines objs))
	 (lines  (apply 'append liness)))
    `(:rect ,@lines)))


;;;  ___                     _                 _       
;;; |_ _|_ __  ___  ___ _ __| |_ _ __ ___  ___| |_ ___ 
;;;  | || '_ \/ __|/ _ \ '__| __| '__/ _ \/ __| __/ __|
;;;  | || | | \__ \  __/ |  | |_| | |  __/ (__| |_\__ \
;;; |___|_| |_|___/\___|_|   \__|_|  \___|\___|\__|___/
;;;                                                    
;; «insertrects»  (to ".insertrects")
;; See: (to "insert")
;; Tests:
;;  (sc-:insertrects "" "a" '(:rect ("b" "c") ("d")))
;; (find-insertrects "" "a" '(:rect ("b" "c") ("d")))
;; (find-insertrects-2a "a" '(:rect ("b" "c") ("d")))
;; (find-insertrects-2a '((:m1 abL) "ab" (:m1 abR) (:m1 cdL) "cd" (:m1 cdR)))
;; (sc-:setoverlay2 "abL" "abR")
;; (sc-:setoverlay2 "cdL" "cdR")
;; (sc-:deleteoverlay)
;; (find-insertrects-3a '("a" (:m1 "bc beg") "bc" (:m1 "bc end") "d"))
;; (sc-:setoverlay1 "bc")
;; (sc-:deleteoverlay)
;;
(defun sc-:insertrects (&rest objs)
  (dolist (o objs)
    (cl-loop for line in (sc-:tolines o)
	     do (progn (apply 'sc-:insert line)
		       (insert "\n")))))

(defun find-insertrects (&rest objs)
  (sc-:deleteallmarkers)
  (find-eoutput-rerun (or ee-buffer-name "*show-conses*")
		      `(apply 'sc-:insertrects ',objs)
		      :end))

(defun find-insertrects-2a (&rest objs)
  (find-2a nil `(apply 'find-insertrects ',objs)))

(defun find-insertrects-3a (&rest objs)
  (find-3a
   nil
   `(apply 'find-insertrects ',objs)
   '(find-showregions)))
  






;;;  _   _                 _                   _                         _ 
;;; | | | |_ __   __ _  __| |   __ _ _ __   __| | __   ___ __   __ _  __| |
;;; | |_| | '_ \ / _` |/ _` |  / _` | '_ \ / _` | \ \ / / '_ \ / _` |/ _` |
;;; |  _  | |_) | (_| | (_| | | (_| | | | | (_| |  \ V /| |_) | (_| | (_| |
;;; |_| |_| .__/ \__,_|\__,_|  \__,_|_| |_|\__,_|   \_/ | .__/ \__,_|\__,_|
;;;       |_|                                           |_|                
;;
;; «hpad-and-vpad»  (to ".hpad-and-vpad")
;; Tests:
;;   (sc-:hpad '(:rect ("a") ("b" "c")) 5 ?_)
;;   (sc-:vpad '(:rect ("a") ("b" "c")) 5)
;;
(defun sc-:hpad-line (o newwidth &optional char)
  (let* ((oldwidth (sc-:width-line o))
	 (nchars   (max 0 (- newwidth oldwidth)))
	 (newchars (make-string nchars (or char 32))))
    `(,@o ,newchars)))

(defun sc-:hpad (o newwidth &optional char)
  (let* ((lines1 (sc-:tolines o))
	 (lines2 (cl-loop for line in lines1
			  collect (sc-:hpad-line line newwidth char))))
    `(:rect ,@lines2)))

(defun sc-:height (o) (length (sc-:tolines o)))

(defun sc-:vpad (o newheight)
  (let* ((lines1  (sc-:tolines o))
	 (height1 (length lines1))
	 (nblanks (max 0 (- newheight height1)))
	 (blanks  (cl-loop for i from 1 to nblanks
			   collect '(""))))
    `(:rect ,@lines1 ,@blanks)))



;;;   ____ _            _                             _       
;;;  / ___| |_   _  ___(_)_ __   __ _   _ __ ___  ___| |_ ___ 
;;; | |  _| | | | |/ _ \ | '_ \ / _` | | '__/ _ \/ __| __/ __|
;;; | |_| | | |_| |  __/ | | | | (_| | | | |  __/ (__| |_\__ \
;;;  \____|_|\__,_|\___|_|_| |_|\__, | |_|  \___|\___|\__|___/
;;;                             |___/                         
;;
;; «glueing-rects»  (to ".glueing-rects")
;; Tests:
;; (setq o1 '(:rect ("a") ("|") ("bcd")))
;; (setq o2 (sc-:addwire o1))
;; (setq o3 '(:rect ("e") ("|") ("f")))
;; (setq o4 (sc-:glue o2 o3))
;; (find-insertrects-2a o1 "" o2 "" o3 "" o4)
;; (find-insertrects-2a o1 "" o3 "" (sc-:glue o1 "__" o3))
;; (find-insertrects-2a o1 "" o3 "" (sc-:gluewithwires o1 o3))
;;
(defun sc-:glue2 (a b)
  (let* ((newheight (max (sc-:height a) (sc-:height b))))
    (setq a (sc-:vpad a newheight))
    (setq b (sc-:vpad b newheight))
    (setq a (sc-:hpad a (sc-:width a)))
    (let ((clines (cl-loop for aline in (sc-:tolines a)
			   for bline in (sc-:tolines b)
			   collect `(,@aline ,@bline))))
      `(:rect ,@clines))))

(defun sc-:glue (&rest objs)
  (if (< (length objs) 2)
      (car objs)
    (sc-:glue2 (car objs)
	       (apply 'sc-:glue (cdr objs)))))

(defun sc-:addwire (o)
  (let* ((lines      (sc-:tolines o))
	 (topline    (car lines))
	 (otherlines (cdr lines))
	 (newwidth   (+ (sc-:width o) 2))
	 (newtopline (sc-:hpad-line topline newwidth ?_))) 
    `(:rect ,newtopline ,@otherlines)))

(defun sc-:gluewithwires (&rest list)
  (if (null list) (error))
  (let* ((revlist (reverse list))
	 (last    (car revlist))
	 (revrest (cdr revlist))
	 (result  (sc-:torect last)))
    (dolist (o revrest)
      (setq result (sc-:glue (sc-:addwire o) result)))
    result))

;; Test:
;; (find-insertrects-2a (sc-:rtree "abc" (sc-:rtree "d" "e" "f") "ghi"))
(defun sc-:rtree (root &rest subtrees)
  (if (null subtrees)
      root
   (sc-:glue root "  " (apply 'sc-:pile subtrees))))




;;;  _____                            _ 
;;; | ____|_  ___ __   __ _ _ __   __| |
;;; |  _| \ \/ / '_ \ / _` | '_ \ / _` |
;;; | |___ >  <| |_) | (_| | | | | (_| |
;;; |_____/_/\_\ .__/ \__,_|_| |_|\__,_|
;;;            |_|                      
;;
;; «:expand»  (to ".:expand")
;; `sc-:expand' implements another way to expand expressions
;; containing `:'-keywords; `sc-:expand' is much harder to
;; implement than `sc-:do1', so we only introduce it now.
;;
;; Let's define a function `sc-:<>' for tests:
(defun sc-:<> (&rest body) `(< ,@body >))
;;
;; Compare:
;;   (sc-:expand '(1     (:<> 2 3)   4     (:@<> 5 6)  7))
;;               `(1 ,(sc-:<> 2 3)   4 ,@(sc-:<> 5 6)  7)
;; they both return this:
;;               '(1       (< 2 3 >) 4         < 5 6 > 7)
;;
;; ...so the `:<>' is "expanded normally",
;;  and the `:@<>' is "expanded with splicing".
;; Note that `:<>' and `:@<>' do not expand their arguments:
;;
;;   (sc-:expand '(1 (:@<> 2 (:@<> 3 4) 3)  5))
;;           --> '(1     < 2 (:@<> 3 4) 3 > 5)
;;
;; We will only define an expander that expands its arguments at the
;; end of this section - look for `:@<e>'.

;; Tests:
;; (sc-:symbol   :<>)
;;        --> sc-:<>
;; (sc-:@symbol :@<>)
;;        --> sc-:<>
;;
(defun sc-:symbol (symbol)
  "If SYMBOL is `:foo' and `sc-:foo' is fbound, then return `sc-:foo'."
  (if (keywordp symbol)
      (let ((symbol2 (intern-soft (format "sc-%s" symbol))))
	(if (fboundp symbol2)
	    symbol2))))

(defun sc-:@symbol (symbol)
  "If SYMBOL is `:@foo' and `sc-:foo' is fbound, then return `sc-:foo'."
  (if (keywordp symbol)
      (if (string-match "^:@" (symbol-name symbol))
	  (let* ((stem (substring (symbol-name symbol) 2))
		 (symbol2 (intern-soft (format "sc-:%s" stem))))
	    symbol2))))

;; Test:
;; (sc-:expand-:list     '(:<>  2 3))
;; (sc-:expand-:@list    '(:@<> 4 5))
;; (sc-:expand-plainlist '(1 (2 3) (:<> 4 5)   (:@<> 6 7)  8))
;; (sc-:expand           '(1 (2 3) (:<> 4 5)   (:@<> 6 7)  8))
;;                   --> '(1 (2 3)   (< 4 5 >)     < 6 7 > 8)
;;
(defun sc-:expand-plainlist (list)
  (cl-loop for o in list
	   append (sc-:expand-inner o)))

(defun sc-:expand-:list (list)
    (apply (sc-:symbol (car list)) (cdr list)))

(defun sc-:expand-:@list (list)
    (apply (sc-:@symbol (car list)) (cdr list)))

(defun sc-:expand-inner (o)
  (cond ((atom o)              (list o))
	((sc-:symbol  (car o)) (list (sc-:expand-:list o)))
	((sc-:@symbol (car o)) (sc-:expand-:@list o))
	(t                     (list (sc-:expand-plainlist o)))))

(defun sc-:expand (o)
  (cond ((atom o)              o)
	((sc-:symbol  (car o)) (sc-:expand-:list o))
	((sc-:@symbol (car o)) (sc-:expand-:@list o))
	(t                     (sc-:expand-plainlist o))))

(defun sc-:expand2 (o) (sc-:expand (sc-:expand o)))

;; Remember that `sc-:<>' _quotes_ its arguments...
;; The function `sc-:<e>' below _expands_ its arguments.
;; Test:
;; (sc-:expand '(1 (:@<e> (:@<e> 2 3))   (:@<> (:@<> 4 5))  6))
;;         --> '(1           < < 2 3 > >     < (:@<> 4 5) > 6)
;;
(defun sc-:<e> (&rest body) `(< ,@(sc-:expand-plainlist body) >))




;;;              
;;;  _ _ __ ___  
;;; (_) '_ ` _ \ 
;;;  _| | | | | |
;;; (_)_| |_| |_|
;;;              
;; «:m»  (to ".:m")
;; `:m' is a macro that adds `beg' and `end' markers - or,
;; in debugging mode, adds "<tag: " and ">". For example:
;;
;;     (sc-:expand '(:m  "ab"      "a" "b"))
;;            --> '((:m1 "ab beg") "a" "b" (:m1 "ab end"))
;;  (sc-:expand<>  '(:m  "ab"      "a" "b")
;;            -->     '("<ab: "    "a" "b" ">")
;;  (sc-:expand<>c '(:m  "ab"      "a" "b"))
;;            -->       "<ab: ab>"
;;
;; A high-level test:
;;
;; (setq o '("a" (:@m "bcde" "b" (:@m "cd" "c" "d") "e") "f"))
;;                      (sc-:expand o)
;; (find-insertrects-3a (sc-:expand o))
;; (sc-:setoverlay1 "bcde")
;; (sc-:setoverlay1 "cd")
;; (sc-:deleteoverlay)
;;
(defvar sc-:m 'sc-:m-:m1)
(defun  sc-:m (&rest rest) (apply sc-:m rest))

(defun  sc-:m-:m1 (tag &rest body)
  (let* ((tagbeg (format "%s beg" tag))
	 (tagend (format "%s end" tag)))
  `((:m1 ,tagbeg) ,@(sc-:expand-plainlist body) (:m1 ,tagend))))

(defun sc-:m-<> (tag &rest body)
  `(,(format "<%s: " tag) ,@(sc-:expand-plainlist body) ">"))

(defun sc-:expand<>  (o) (let ((sc-:m 'sc-:m-<>)) (sc-:expand o)))
(defun sc-:expand<>c (o) (apply 'concat (sc-:expand<> o)))



;;;                        
;;;  _ ___  _____  ___ __  
;;; (_) __|/ _ \ \/ / '_ \ 
;;;  _\__ \  __/>  <| |_) |
;;; (_)___/\___/_/\_\ .__/ 
;;;                 |_|    
;;
;; «:sexp»  (to ".:sexp")
;; Tests - choose one of the `setqs', then run the rest:
;;   (setq o (sc-:sexp 42))
;;   (setq o (sc-:sexp '(2 . 3)))
;;   (setq o (sc-:sexp '(2 3)))
;;   (setq o (sc-:sexp '(2 3 4)))
;;   (setq o (sc-:sexp '(2 3 . 4)))
;;   (setq o (sc-:sexp '(2)))
;;   (setq o (sc-:sexp '((2))))
;;   (setq o (sc-:sexp '((2) 3 (4 5) . 6)))
;;                        (sc-:expand  o)
;;                        (sc-:expand2 o)
;;   (find-insertrects-3a (sc-:expand2 o))

(defvar sc-context "o")

(defun sc-:m-sub (fmt &rest body)
  (let ((sc-context (format fmt sc-context)))
    `(:@m ,sc-context ,@(sc-:expand-plainlist body))))

(defun sc-:m-c   (&rest body) (apply 'sc-:m-sub "%s"     body))
(defun sc-:m-car (&rest body) (apply 'sc-:m-sub "%s.car" body))
(defun sc-:m-cdr (&rest body) (apply 'sc-:m-sub "%s.cdr" body))

(defun sc-:cons0 (p o)
  (if (null (cdr o))
      `(,p    (:m-car ,@(sc-:sexp0 (car o))) ")")
    (if (atom (cdr o))
      `(,p    (:m-car ,@(sc-:sexp0 (car o)))
	" . " (:m-cdr ,@(sc-:sexp0 (cdr o)))
	")")
    `(,p (:m-car ,@(sc-:sexp0 (car o)))
	 (:m-cdr ,@(sc-:cons0 " " (cdr o)))))))

(defun sc-:sexp0 (o)
  (if (atom o)
      (list (ee-S o))
    (sc-:cons0 "(" o)))

(defun sc-:sexp (o)
  `(:m-c ,@(sc-:sexp0 o)))





;;;  ____       _                      _             
;;; / ___|  ___| |_ _____   _____ _ __| | __ _ _   _ 
;;; \___ \ / _ \ __/ _ \ \ / / _ \ '__| |/ _` | | | |
;;;  ___) |  __/ || (_) \ V /  __/ |  | | (_| | |_| |
;;; |____/ \___|\__\___/ \_/ \___|_|  |_|\__,_|\__, |
;;;                                            |___/ 
;;
;; «setoverlay-buttons»  (to ".setoverlay-buttons")
;; See: (to "overlay")
;; When we draw a cons cell diagram like this one,
;;
;;   (2 3)
;;   .__.
;;   |  |
;;   2  3
;;
;; each node of the lower half gets text properties that make it
;; highlight a part of the upper half when we press `C-c C-c' on the
;; node. These text properties are similar to how Emacs implements
;; buttons, but are much simpler - and I will say that node in the
;; lower half becomes a "setoverlay button".
;;
;; Note that `sc-:hl1' is similar to `sc-:face1' and `sc:fg1', that
;; are defined here:
;;   (to "sc-:do1")

(defun sc-setoverlay-action (tag) 
  (interactive (list (get-text-property (point) 'tag)))
  (message "Set overlay %s" tag)
  (sc-:setoverlay1 tag))

(defvar     sc-setoverlay-keymap (make-sparse-keymap))
(define-key sc-setoverlay-keymap (kbd "C-c C-c") 'sc-setoverlay-action)

(defvar    sc-setoverlay-category "See `sc-:hl1'.")
(setplist 'sc-setoverlay-category
	  `(mouse-face      highlight
	    face            (:foreground "red")
            font-lock-face  (:foreground "red")
	    keymap          ,sc-setoverlay-keymap))

(defun sc-:hl1 (tag &optional str)
  (propertize (or str tag)
	      'tag tag
	      'category       'sc-setoverlay-category
	      'face           '(:foreground "red")
              'font-lock-face '(:foreground "red")
	      'help-echo      (format "Set overlay %s" tag) ; broken
	      ))

;; (setq o (sc-:sexp '(2 . 3)))
;; (sc-:deleteallmarkers)
;; (apply 'sc-:insert "\n;;  --> " (sc-:expand (sc-:expand o)))
;;  --> (2 . 3)
;; (find-2a nil '(find-showregions))
;; (insert "\n;; <" (sc-:hl1 "o") "><" (sc-:hl1 "o.car") ">")
;; <o><o.car>

;; (find-es "emacs" "propertize")



;;;  _____                     _                     
;;; |_   _|   _ _ __   ___  __| | ___  ___  ___ _ __ 
;;;   | || | | | '_ \ / _ \/ _` |/ _ \/ __|/ __| '__|
;;;   | || |_| | |_) |  __/ (_| |  __/\__ \ (__| |   
;;;   |_| \__, | .__/ \___|\__,_|\___||___/\___|_|   
;;;       |___/|_|                                   
;;
;; «typedescr-buttons»  (to ".typedescr-buttons")

(defun sc-typedescr-action (tag) 
  (interactive (list (get-text-property (point) 'tag)))
  (message "Run (find-etypedescr '%s)" tag)
  (find-2a nil `(find-etypedescr ',tag)))

(defvar     sc-typedescr-keymap (make-sparse-keymap))
(define-key sc-typedescr-keymap (kbd "C-c C-c") 'sc-typedescr-action)

(defvar    sc-typedescr-category "")
(setplist 'sc-typedescr-category
	  `(face           (:foreground "red")
            font-lock-face (:foreground "red")
	    mouse-face     highlight
	    keymap         ,sc-typedescr-keymap))

(defun sc-:td1 (tag &optional str)
  (propertize (or str (format "%s" tag))
	      'tag tag
	      'category 'sc-typedescr-category
	      'help-echo (format "Run (find-etypedescr '%s)" tag) ; broken
	      ))





;;;  ____                  _                 
;;; / ___|  _____  ___ __ | |_ _ __ ___  ___ 
;;; \___ \ / _ \ \/ / '_ \| __| '__/ _ \/ _ \
;;;  ___) |  __/>  <| |_) | |_| | |  __/  __/
;;; |____/ \___/_/\_\ .__/ \__|_|  \___|\___|
;;;                 |_|                      
;;
;; «:sexptree»  (to ".:sexptree")
;; Compare with: (to ":sexp")
;; Tests:
;;   (setq o1 '((2) 3 (4 5) . 6))
;;           (sc-:toplain (sc-:sexptree      o1))
;;           (sc-:toplain (sc-:sexp-and-tree o1))
;;   (find-insertrects-2a (sc-:sexp-and-tree o1))
;;
(defun sc-:sexptree-car (o)
  (let ((sc-context (format "%s.car" sc-context)))
    (sc-:sexptree (car o))))

(defun sc-:sexptree-cdr (o)
  (let ((sc-context (format "%s.cdr" sc-context)))
    (sc-:sexptree (cdr o))))

(defun sc-:sexptree-car+ (o)
  (sc-:pile (sc-:hl1 sc-context ".")
	    "|"
	    (sc-:sexptree-car o)))

(defun sc-:sexptree (o)
  (if (atom o)
      (sc-:hl1 sc-context (ee-S o))
    (if (null (cdr o))
	(sc-:sexptree-car+ o)
      (sc-:gluewithwires
       (sc-:sexptree-car+ o)
       (sc-:sexptree-cdr  o)))))

(defun sc-:sexp-and-tree (o)
  (let* ((sexpwithms (sc-:expand (sc-:expand (sc-:sexp o))))
	 (tree       (sc-:sexptree o))
	 (message     "^ Try `C-c C-c' on the nodes"))
    (sc-:pile sexpwithms
	      ""
	      tree
	      ""
	      message)))


;;;   __ _           _       _                                                   
;;;  / _(_)_ __   __| |  ___| |__   _____      __   ___ ___  _ __  ___  ___  ___ 
;;; | |_| | '_ \ / _` | / __| '_ \ / _ \ \ /\ / /  / __/ _ \| '_ \/ __|/ _ \/ __|
;;; |  _| | | | | (_| | \__ \ | | | (_) \ V  V /  | (_| (_) | | | \__ \  __/\__ \
;;; |_| |_|_| |_|\__,_| |___/_| |_|\___/ \_/\_/    \___\___/|_| |_|___/\___||___/
;;;                                                                              
;; «find-show-conses»  (to ".find-show-conses")
;; Tests:
;; (find-show-conses '((2) 3 ("4" 5) . 6))
;; (find-2a nil '(find-show-conses    '((2) 3 (4 5) . 6)))
;;               (find-show-conses-2a '((2) 3 (4 5) . 6))
;;               (find-show-conses-3a '((2) 3 (4 5) . 6))

(defun find-show-conses (o)
  (find-insertrects (sc-:sexp-and-tree o)))

(defun find-show-conses-2a (o)
  (find-2a nil `(find-show-conses ',o)))

(defun find-show-conses-3a (o)
  (find-3a
   nil
   `(find-show-conses ',o)
   '(find-showregions)))








;;;   __ _           _            _               _                 
;;;  / _(_)_ __   __| |       ___| | __ _ ___ ___| |_ _ __ ___  ___ 
;;; | |_| | '_ \ / _` |_____ / __| |/ _` / __/ __| __| '__/ _ \/ _ \
;;; |  _| | | | | (_| |_____| (__| | (_| \__ \__ \ |_| | |  __/  __/
;;; |_| |_|_| |_|\__,_|      \___|_|\__,_|___/___/\__|_|  \___|\___|
;;;                                                                 
;; «find-classtree»  (to ".find-classtree")
;; Tests:
;; (sc-:classchildren 'oclosure)
;; (sc-:toplain (sc-:classtree 'function))
;; (find-classtree 'function)
;; (find-classtree 't)
;; (find-insertrects-2a (sc-:td "abc" (sc-:rtree "d" "e" "f") "ghi"))

;;
(defun sc-:td (tag)
  `(:rect ((:td1 ,tag))))

(defun sc-:classchildren (symbol)
  (ee-sort-symbols (cl--class-children (cl-find-class symbol))))

(defun sc-:classtree (symbol)
  (let* ((children1 (sc-:classchildren symbol))
	 (children2 (mapcar 'sc-:classtree children1)))
    (apply 'sc-:rtree (sc-:td symbol) children2)))

(defun find-classtree (symbol)
  (let ((ee-buffer-name
	 (or ee-buffer-name
	     (format "*(find-classtree %s)*" symbol))))
    (find-estring (sc-:totext (sc-:classtree symbol)))))







;; (find-showconses "show-conses.el" "keymap")





;; Local Variables:
;; coding:  utf-8-unix
;; End:
