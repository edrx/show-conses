;;; show-conses.el --- Shows cons-cell diagrams -*- lexical-binding: nil; -*-
;;
;; Copyright (C) 2024 Eduardo Ochs
;;
;; Author: Eduardo Ochs <eduardoochs@gmail.com>
;; Maintainer: Eduardo Ochs <eduardoochs@gmail.com>
;; Created: 2024oct20
;; Modified: 2024oct20
;; Version: 0.0.20241020
;; Homepage: http://anggtwu.net/show-conses.html
;; Package-Requires: ((eev))
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; See:
;;   http://anggtwu.net/show-conses.html
;;   http://anggtwu.net/show-conses/show-conses.el.html
;;   http://anggtwu.net/show-conses/show-conses.el
;;                (find-showconses "show-conses.el")
;; Git repository:
;;   https://github.com/edrx/show-conses
;;
;; EVERYTHING HERE IS VERY NEW & PRELIMINARY!
;;
;; (defun e () (interactive) (find-showconses "show-conses.el"))
;; (Re)load with: (load (buffer-file-name))

;; Index:
;;
;; «.code-c-d»		(to "code-c-d")
;; «.intro»		(to "intro")
;; «.markers»		(to "markers")
;; «.overlay»		(to "overlay")
;; «.regions»		(to "regions")
;; «.context»		(to "context")
;; «.minilang»		(to "minilang")
;; «.lisp»		(to "lisp")
;; «.toplain»		(to "toplain")
;; «.expand»		(to "expand")
;; «.keymap»		(to "keymap")
;; «.totext»		(to "totext")
;; «.insert»		(to "insert")
;; «.show»		(to "show")
;; «.eval-last-sexp»	(to "eval-last-sexp")
;; «.width»		(to "width")
;; «.pad»		(to "pad")
;; «.lr»		(to "lr")
;; «.tree»		(to "tree")
;; «.export»		(to "export")

;; See: (find-eev "eev-load.el")
;;      (find-eev-levels-intro)
(require 'eev-load)

;; «code-c-d»  (to ".code-c-d")
;; See: (find-eev "eev-code.el" "code-c-d-s")
;; Try: (find-showconsesfile "")
(code-c-d "showconses" (ee-locate-library "show-conses.el" t) :anchor)




;;;  ___       _             
;;; |_ _|_ __ | |_ _ __ ___  
;;;  | || '_ \| __| '__/ _ \ 
;;;  | || | | | |_| | | (_) |
;;; |___|_| |_|\__|_|  \___/ 
;;;                          
;; «intro»  (to ".intro")
;; Skel: (find-intro-links "show-conses")
;; Test: (find-show-conses-intro)

(defun find-show-conses-intro (&rest pos-spec-list) (interactive)
  (let ((ee-buffer-name "*(find-show-conses-intro)*"))
    (apply 'find-eintro "\
\(Re)generate: (find-show-conses-intro)
Source code:  (find-efunction 'find-show-conses-intro)
More intros:  (find-eev-quick-intro)
              (find-eev-intro)
This buffer is _temporary_ and _editable_.
It is meant as both a tutorial and a sandbox.


Prerequisites:
  (find-eev-quick-intro \"2. Evaluating Lisp\")
  (find-eev-quick-intro \"3. Elisp hyperlinks\")




1. Introduction
===============
This is a \"cons diagram\",

  (1 (2 \"3\") . 4)

  .__._______4
  |  |
  1  .__.    
     |  |
     2  \"3\"

that shows a sexp, (1 (2 \"3\") . 4), and how that sexp is represented
using conses. It uses a format that is much more compact than the
formats used by pair-tree.el - that is an Emacs package that is MELPA -
and by \"Sdraw\" from Racket. Compare:

  https://github.com/zainab-ali/pair-tree.el
  https://docs.racket-lang.org/sdraw/index.html

If you are reading this in Emacs then you have already loaded
\"show-conses.el\", that is able to display such diagrams. Try:

  (find-2a nil '(find-show-conses-lisp '(1 (2 \"3\") . 4)))

You will get a two-window setting like this,

   _________________________
  |         |               |
  |         |               |
  |  intro  | *show-conses* |
  |         |               |
  |_________|_______________|

and in the \"*show-conses*\" buffer some parts of the cons tree are
\"highlighters\". For example, if you go to the \".\" that corresponds
to the sub-sexp (2 \"3\") and type `C-c C-c' there it will highlight
the `(2 \"3\")' in the upper part. Try that now.

The highlighters are implemented using text properties, and the regions
that they highlight are implemented using a hash table that associates
names to markers. Try:

  (find-epp (show-conses-propertize-h '(h \".\" \"cadr\")))

  (show-conses-delete-markers)
  (find-show-conses-lisp-3a '(1 (2 \"3\") . 4) :end)
  (show-conses-set-overlay-1 \"car\")
  (show-conses-set-overlay-1 \"cr\")
  (show-conses-delete-overlay \"cr\")
  (find-2a nil '(find-ehashtable show-conses-markers))




2. Intended audience
====================
You can use sexps like

  (find-show-conses-lisp-2a '(1 (2 \"3\") . 4) :end)

to explain sexps and conses to your friends, but I consider that this
package is:

  \"...more like a toy that is _slightly interesting_ if you play with
  it for a few seconds, and _much more interesting_ if you open it and
  take its pieces apart to see how everything works.\"

For more on that, see:

  http://anggtwu.net/2024-eev-for-5-year-olds.html#taking-apart




3. Namespaces
=============
The file \"show-conses.el\" is well-behaved: it only defines symbols
that start with \"show-conses-\" or \"find-show-conses\", plus a few
extensions to `M-e', that start with \"ee-\" and that, ahem, \"invade
the eev namespace\". Check:

  (find-eloadhistory-for 'show-conses-shorten)
  (find-eloadhistory-for 'ee-eval-last-sexp)

Remember that _most_ short and cryptic names, like `a' and `foo' - but
not `t', `car', and `pi' - are reserved for users. The function
`show-conses-export' (sort of) exports the symbols of show-conses.el
halfway towards this \"user namespace\", by creating shorter aliases in
which each \"show-conses\" is replaced by just \"sc\". You can inspect
what `show-conses-export' does by running this:

  (find-estring-elisp (show-conses-export0))

Run this to define these shorter symbols:

  ;; See: (find-efunction 'show-conses-export)
  (show-conses-export)

I will use the shorter symbols in most of the examples of this intro.



4. The DSL
==========

" pos-spec-list)))

;; (defun e () (interactive) (find-angg "elisp/show-conses.el"))
;; (find-show-conses-intro)



;;;  __  __            _                 
;;; |  \/  | __ _ _ __| | _____ _ __ ___ 
;;; | |\/| |/ _` | '__| |/ / _ \ '__/ __|
;;; | |  | | (_| | |  |   <  __/ |  \__ \
;;; |_|  |_|\__,_|_|  |_|\_\___|_|  |___/
;;;                                      
;; «markers»  (to ".markers")
;; Test:
;; (show-conses-delete-markers)
;; (show-conses-set-marker "a")
;; (show-conses-set-marker "b")
;; (find-ehashtable show-conses-markers)
;; (show-conses-marker "a")
;; (show-conses-marker "err")
;;
(defvar show-conses-markers (make-hash-table :test 'equal))

(defun show-conses-delete-markers ()
  (setq show-conses-markers (make-hash-table :test 'equal)))

(defun show-conses-delete-marker (name)
  (remhash name show-conses-markers))

(defun show-conses-set-marker (name &optional pos buffer)
  (with-current-buffer (or buffer (current-buffer))
    (if pos (goto-char pos))
    (puthash name (point-marker) show-conses-markers)))

(defun show-conses-marker (name)
  (or (gethash name show-conses-markers)
      (error (format "Failed: (get-hash %S show-conses-markers)" name))))




;;;   ___                 _             
;;;  / _ \__   _____ _ __| | __ _ _   _ 
;;; | | | \ \ / / _ \ '__| |/ _` | | | |
;;; | |_| |\ V /  __/ |  | | (_| | |_| |
;;;  \___/  \_/ \___|_|  |_|\__,_|\__, |
;;;                               |___/ 
;; «overlay»  (to ".overlay")
;; Tests:
;; (show-conses-delete-overlay)
;; (show-conses-set-overlay-0 (+ 2 (ee-bol)) (+ 4 (ee-bol)))
;; (show-conses-set-overlay-0 (+ 5 (ee-bol)) (+ 9 (ee-bol)))
;; (show-conses-delete-overlay)
;; (show-conses-set-marker "a")
;; (show-conses-set-marker "b")
;; (show-conses-set-marker "c")
;; (show-conses-set-overlay "a" "b")
;; (show-conses-set-overlay "a" "c")
;; (show-conses-set-overlay "b" "c")
;; (show-conses-delete-overlay)
;; (show-conses-set-marker "foo-start")
;; (show-conses-set-marker "foo-end")
;; (show-conses-set-overlay-1 "foo")
;; (show-conses-delete-overlay)
;;
(defvar show-conses-overlay nil)

(defun show-conses-delete-overlay ()
  "Delete the overlay `show-conses-overlay' if it exists."
  (if (overlayp show-conses-overlay)
      (delete-overlay show-conses-overlay)))

(defun show-conses-set-overlay-0 (start end &optional buffer)
  "A low-level function called by `show-conses-set-overlay'."
  (setq buffer (or buffer (current-buffer)))
  (show-conses-delete-overlay)
  (setq show-conses-overlay
	(with-current-buffer buffer
	  (make-overlay start end)))
  (overlay-put show-conses-overlay 'face 'highlight))

(defun show-conses-set-overlay (start end)
  "Make `show-conses-overlay' highlight the region between START and END.
START and END should be strings and should be the names of two markers
in `show-conses-markers' that point to the same buffer."
  (let* ((startmarker (show-conses-marker start))
	 (startpos    (marker-position    startmarker))
	 (startbuf    (marker-buffer      startmarker))
	 (endmarker   (show-conses-marker end))
	 (endpos      (marker-position    endmarker))
	 (endbuf      (marker-buffer      endmarker)))
    (if (not (eq startbuf endbuf))
	(error (format "Markers %S and %S are in different buffers!"
		       start end)))
    (show-conses-set-overlay-0 startpos endpos startbuf)))

(defun show-conses-set-overlay-1 (stem)
  "Like `show-conses-set-overlay', but receives a single argument."
  (show-conses-set-overlay (format "%s-start" stem)
			   (format "%s-end"   stem)))


;;;  ____            _                 
;;; |  _ \ ___  __ _(_) ___  _ __  ___ 
;;; | |_) / _ \/ _` | |/ _ \| '_ \/ __|
;;; |  _ <  __/ (_| | | (_) | | | \__ \
;;; |_| \_\___|\__, |_|\___/|_| |_|___/
;;;            |___/                   
;;
;; «regions»  (to ".regions")
;; Test:
;; (find-ehashtable show-conses-markers)
;; (show-conses-delete-markers)
;; (show-conses-set-marker "foo-start")
;; (show-conses-set-marker "foo-end")
;; (show-conses-set-marker "bar-start")
;; (show-conses-set-marker "bar-end")
;; (find-2b nil '(find-show-conses-regions))
;; (show-conses-delete-overlay)
;;
(defun show-conses-region-names ()
  (let* ((stems (cl-loop for k being the hash-keys of show-conses-markers
			 if (string-match "-start$" k)
			 collect (replace-regexp-in-string "-start$" "" k))))
    (sort stems 'string<)))

(defun find-show-conses-regions ()
  (let* ((names (show-conses-region-names))
	 (sexps (cl-loop for name in names
			 collect `(show-conses-set-overlay-1 ,name))))
    (find-elinks-elisp
     `((find-show-conses-regions)
       (find-efunction 'find-show-conses-regions)
       ""
       ,(ee-ppp00 sexps)))))


;;;   ____            _            _   
;;;  / ___|___  _ __ | |_ _____  _| |_ 
;;; | |   / _ \| '_ \| __/ _ \ \/ / __|
;;; | |__| (_) | | | | ||  __/>  <| |_ 
;;;  \____\___/|_| |_|\__\___/_/\_\\__|
;;;                                    
;; «context»  (to ".context")
;; Tests:
;; (show-conses-context)
;; (show-conses-showcontext)
;; (show-conses-runincontext "bla" "ble" (show-conses-context))
;; (show-conses-runincontext "bla" "ble" (show-conses-context))
;; (show-conses-runinsubcontext ""       (show-conses-context))
;; (show-conses-runinsubcontext "foo"    (show-conses-context))
;; See: (find-elnode "Indenting Macros")
;;
(defvar show-conses-prefix "c")
(defvar show-conses-suffix "r")

(defun show-conses-context     () (concat show-conses-prefix show-conses-suffix))
(defun show-conses-showcontext () (list   show-conses-prefix show-conses-suffix))

(defmacro show-conses-runincontext (prefix suffix &rest code)
  (declare (indent 2))
  `(let ((show-conses-prefix ,prefix)
         (show-conses-suffix ,suffix))
     ,@code))

(defmacro show-conses-runinsubcontext (s &rest code)
  (declare (indent 1))
  `(let ((show-conses-suffix (format "%s%s" ,s show-conses-suffix)))
     ,@code))



;;;  _____                            _ 
;;; | ____|_  ___ __   __ _ _ __   __| |
;;; |  _| \ \/ / '_ \ / _` | '_ \ / _` |
;;; | |___ >  <| |_) | (_| | | | | (_| |
;;; |_____/_/\_\ .__/ \__,_|_| |_|\__,_|
;;;            |_|                      
;;
;; «expand»    (to ".expand")
;; Tests:
;;   (show-conses-expand-1 "4")
;;   (show-conses-expand-1 '(m car-start))
;;   (show-conses-expand-1              '(is car "4"))
;;   (show-conses-expand-1 '(is cr "(* " (is car "4") " 5 . nil)"))
;;   (show-conses-expand-1 '(is cr "(* "         "4"  " 5 . nil)"))
;;   (find-eppp (show-conses-expand-lines show-conses-demo-lines-3))
;;
(defun show-conses-expand-1 (o)
  (cond ((stringp o) (list o))
	((listp o)   (apply (ee-intern "show-conses-%s" (car o)) (cdr o)))
	(t           (list '_ o))))

(defun show-conses-expand (list)
  (cl-loop for o in list
	   append (show-conses-expand-1 o)))

(defun show-conses-expand-line (line)
  (show-conses-expand line))

(defun show-conses-expand-lines (lines)
  (mapcar 'show-conses-expand-line lines))


;;;  __  __ _       _ _                   
;;; |  \/  (_)_ __ (_) | __ _ _ __   __ _ 
;;; | |\/| | | '_ \| | |/ _` | '_ \ / _` |
;;; | |  | | | | | | | | (_| | | | | (_| |
;;; |_|  |_|_|_| |_|_|_|\__,_|_| |_|\__, |
;;;                                 |___/ 
;;
;; «minilang»  (to ".minilang")
;; Expansion keeps highlighters and markers unchanged.
;; (show-conses-h           "car" "+")
;; (show-conses-expand '((h "car" "+")))
;; (show-conses-m           "car-start")
;; (show-conses-expand '((m "car-start") "+" (m "car-end")))
;;
(defun show-conses-h (name text) `((h ,name ,text)))
(defun show-conses-m (name)      `((m ,name)))

;; Things that can be expanded.
;; The most basic is "is".
;; (show-conses-is           "car" "+")
;; (show-conses-expand '((is "car" "+")))
;; (show-conses-expand '((is "main" "(" (is "car" "foo") ")")))
;; (show-conses-expand '((is "cr"   "(" (is "car" "foo") ")")))
(defun show-conses-mstart (name) `((m ,(format "%s-start" name))))
(defun show-conses-mend   (name) `((m ,(format "%s-end"   name))))
(defun show-conses-is     (name &rest list)
   `(,@(show-conses-mstart name)
     ,@(show-conses-expand list)
     ,@(show-conses-mend   name)))

(defun show-conses-issub (s &rest rest)
  (show-conses-runinsubcontext s
    `(,@(show-conses-mstart (show-conses-context))
      ,@(show-conses-expand rest)
      ,@(show-conses-mend   (show-conses-context)))))

(defun show-conses-main (&rest rest) (apply 'show-conses-issub ""  rest))
(defun show-conses-a    (&rest rest) (apply 'show-conses-issub "a" rest))
(defun show-conses-d    (&rest rest) (apply 'show-conses-issub "d" rest))



;;;  _     _           
;;; | |   (_)___ _ __  
;;; | |   | / __| '_ \ 
;;; | |___| \__ \ |_) |
;;; |_____|_|___/ .__/ 
;;;             |_|    
;;
;; «lisp»  (to ".lisp")
'("This is a test block!"
  ;; (find-estring-elisp (show-conses-export0))
  (show-conses-export)

  (show-conses-lisp      '(1 . 2))
  (show-conses-islisp1   '(1 . 2))
  (show-conses-lisp2 "(" '(1 . 2))
  (show-conses-lisp1     '(1 . 2))

  (show-conses-delete-markers)
  (show-conses-expand-1                     '(lisp (1 . 2)))
  (show-conses-expand (show-conses-expand-1 '(lisp (1 . 2))))
  (show-conses-expand (show-conses-islisp1  '(1 . 2)))


  (show-conses-expand-line       '((lisp (1 . 2))))
  (show-conses-expand-line   '("a" (lisp (1 . 2)) "b"))
  (show-conses-expand-lines     '(((lisp (1 . 2)))))
  (show-conses-toplain-line      '((lisp (1 . 2))))
  (show-conses-toplain-lines    '(((lisp (1 . 2)))))
  (show-conses-toplain-line      '("a" (lisp (1 . 2)) "b"))
  (show-conses-toplain-lines    '(("a" (lisp (1 . 2)) "b")))

  (show-conses-delete-markers)
  (find-show-conses-3c      '(("a" (lisp (1 . 2)) "b")))
  (find-show-conses-3c      '(("a" (lisp (1 2 3)) "b")))
  (find-show-conses-3c      '(("a" (lisp (1 (2 3) . 4)) "b")))

  (find-show-conses      '(((lisp (1 . 2)))))


  (show-conses-lisp  '(1 2))

(consp '(1 . 2))

"--")

(defun show-conses-lisp (o)
  (show-conses-expand-1
   (show-conses-islisp1 o)))

;; The functions below generate lists with "is".
;; (show-conses-lisp      '(1 . 2))
;; (show-conses-islisp1   '(1 . 2))
;; (show-conses-lisp1     '(1 . 2))
;; (show-conses-lisp2 "(" '(1 . 2))

(defun show-conses-islisp1 (o)
  `(is ,(show-conses-context) ,@(show-conses-lisp1 o)))

(defun show-conses-lisp1 (o)
  (if (consp o)
      (show-conses-lisp2 "(" o)
    (list (format "%S" o))))

(defun show-conses-lisp2 (init o)
  (cond ((eq nil o) '(")"))
        ((consp o)  `(,init
                      ,(show-conses-runinsubcontext "a"
                         (show-conses-islisp1 (car o)))
                      ,(show-conses-runinsubcontext "d"
                         (show-conses-islisp2 " " (cdr o)))))
        (t          `(;; " . "
		      ,(format " . %S)" o)
                      ;; ,(show-conses-runinsubcontext "d"
                      ;;    ;; (show-conses-islisp1 o)
		      ;;    )
                      ;; ")"
		      ))
        ))

(defun show-conses-islisp2 (init o)
  `(is ,(show-conses-context) ,@(show-conses-lisp2 init o)))





;;;  _____             _       _         _            _   
;;; |_   _|__    _ __ | | __ _(_)_ __   | |_ _____  _| |_ 
;;;   | |/ _ \  | '_ \| |/ _` | | '_ \  | __/ _ \ \/ / __|
;;;   | | (_) | | |_) | | (_| | | | | | | ||  __/>  <| |_ 
;;;   |_|\___/  | .__/|_|\__,_|_|_| |_|  \__\___/_/\_\\__|
;;;             |_|                                       
;;
;; «toplain»  (to ".toplain")
;; Convert things to plain text.
;;
;; Tests:
;; (show-conses-toplain-line '((is cr "(* "          "4"  " 5 . nil)")))
;; (show-conses-toplain-line '((is cr "(* " (is cadr "4") " 5 . nil)")))
;;
(defun show-conses-toplain-1 (o)
  (cond ((stringp o)      o)
	((eq 'h  (car o)) (caddr o))
	((eq 'm  (car o)) (caddr o))
	(t                "?")))

(defun show-conses-toplain-line (line)
  (mapconcat 'show-conses-toplain-1
	     (show-conses-expand-line line)
	     ""))

(defun show-conses-toplain-lines (lines)
  (mapconcat 'show-conses-toplain-line
	     (show-conses-expand-lines lines)
	     "\n"))

;; Tests:
;; (show-conses-toplain-lines show-conses-demo-lines-1)
;; (show-conses-toplain-lines show-conses-demo-lines-2)
;; (show-conses-toplain-lines show-conses-demo-lines-3)
;; (show-conses-toplain-lines show-conses-demo-lines-4)
;;
(defvar show-conses-demo-lines-1
  '(("(* 4 5 . nil)")
    ("             ")
    (".__.__.__nil ")
    ("|  |  |      ")
    ("*  4  5      ")))

(defvar show-conses-demo-lines-2
  '(("(* 4 5 . nil)")
    ("")
    (".__.__.__nil")
    ("|  |  |")
    ("*  4  5")))

(defvar show-conses-demo-lines-3
  '(((is cr "(* " (is car "4") " 5 . nil)"))
    ("")
    ((h cr ".") "__.__.__nil")
    ("|  |  |")
    ((h car "*") "  4  5")))

(defvar show-conses-demo-lines-4
  '(((is cr "(" (is car "*")
      (is cdr " " (is cadr "4")
       (is cddr " " (is caddr "5") " . " (is cdddr "nil") ")"))))
    ("")
    ((h cr ".") "__" (h cdr ".") "__" (h cddr ".") "__" (h cdddr "nil"))
    ("|  |  |")
    ((h car "*") "  " (h cadr "4") "  " (h caddr "5") " ")))




;;;  _  __                                
;;; | |/ /___ _   _ _ __ ___   __ _ _ __  
;;; | ' // _ \ | | | '_ ` _ \ / _` | '_ \ 
;;; | . \  __/ |_| | | | | | | (_| | |_) |
;;; |_|\_\___|\__, |_| |_| |_|\__,_| .__/ 
;;;           |___/                |_|    
;;
;; «keymap»  (to ".keymap")
;; Test:
;;   (show-conses-set-marker "foo-start")
;;   (show-conses-set-marker "foo-end")
;;   (find-epropertize-2b `(keymap ,show-conses-keymap stem foo))
;;   (show-conses-delete-overlay)
;; See:
;;   (find-eev "eev-blinks.el" "find-epropertize")
;;
(defvar show-conses-keymap (make-sparse-keymap))

(defun show-conses-keymap-action (&rest rest) 
  (interactive)
  (show-conses-set-overlay-1
   (get-text-property (point) 'stem)))

(define-key show-conses-keymap (kbd "C-c C-c") 'show-conses-keymap-action)

;; Test:
;;   (show-conses-set-marker "foo-start")
;;   (show-conses-set-marker "foo-end")
;;   (find-estring-2a (show-conses-propertize-h '(h "foo" "Type C-c C-c here")))
;;   (show-conses-delete-overlay)
(defun show-conses-propertize-h (o) 
  (let* ((h    (car   o))
	 (stem (cadr  o))
	 (text (caddr o))
	 (properties `(mouse-face highlight
				  stem ,stem
				  keymap ,show-conses-keymap)))
    (if (not (eq h 'h)) (error "Not (h _ _)"))
    (apply 'propertize text properties)))



;;;  ___                     _   
;;; |_ _|_ __  ___  ___ _ __| |_ 
;;;  | || '_ \/ __|/ _ \ '__| __|
;;;  | || | | \__ \  __/ |  | |_ 
;;; |___|_| |_|___/\___|_|   \__|
;;;                              
;; «insert»  (to ".insert")
;; Test: (find-show-conses-2a show-conses-demo-lines-4)
;;
(defvar show-conses-footer "\n\n\n^ Try `C-c C-c' on the nodes\n")

(defun show-conses-insert-1 (o)
  (cond ((stringp o)     (insert o))
        ((eq 'm (car o)) (show-conses-set-marker (cadr o)))
        ((eq 'h (car o)) (insert (show-conses-propertize-h o)))
        (t               (insert "?"))))

(defun show-conses-insert-line (line)
  (let* ((expandedline (show-conses-expand-line line)))
    (mapcar 'show-conses-insert-1 line)))

(defun show-conses-insert-lines (lines)
  (let* ((expandedlines (show-conses-expand-lines lines)))
    (show-conses-insert-line (car expandedlines))
    (cl-loop for expandedline in (cdr expandedlines)
	     do (insert "\n")
	     do (show-conses-insert-line expandedline))))


;;;  ____  _                   
;;; / ___|| |__   _____      __
;;; \___ \| '_ \ / _ \ \ /\ / /
;;;  ___) | | | | (_) \ V  V / 
;;; |____/|_| |_|\___/ \_/\_/  
;;;                            
;; «show»  (to ".show")
;; Tests:
;;   (show-conses-delete-markers)
;;   (find-2a nil '(find-eppp show-conses-demo-lines-3))
;;   (find-show-conses    show-conses-demo-lines-3 :end)
;;   (find-show-conses-2a show-conses-demo-lines-3 :end)
;;   (find-show-conses-3a show-conses-demo-lines-3 :end)
;;
(defun find-show-conses (lines &rest pos-spec-list)
  (apply 'find-eoutput-rerun "*show-conses*"
	 '(progn (show-conses-insert-lines lines)
		 (insert show-conses-footer))
	 pos-spec-list))

(defun find-show-conses-2a (lines &rest pos-spec-list)
  (find-2a nil `(find-show-conses ',lines ,@pos-spec-list)))

(defun find-show-conses-2b (lines &rest pos-spec-list)
  (find-2b nil `(find-show-conses ',lines ,@pos-spec-list)))

(defun find-show-conses-3a (lines &rest pos-spec-list)
  (find-3a nil `(find-show-conses ',lines ,@pos-spec-list)
	       '(find-show-conses-regions)))

(defun find-show-conses-3b (lines &rest pos-spec-list)
  (find-3b nil `(find-show-conses ',lines ,@pos-spec-list)
	       '(find-show-conses-regions)))

(defun find-show-conses-3c (lines &rest pos-spec-list)
  (find-3c nil `(find-show-conses ',lines ,@pos-spec-list)
	       '(find-show-conses-regions)))

;; Tests:
;;   (find-show-conses-lisp     '(1 (2 "3") . 4) :end)
;;   (find-show-conses-lisp-3a  '(1 (2 "3") . 4) :end)
;;
(defun find-show-conses-lisp (o &rest pos-spec-list)
  (apply 'find-eoutput-rerun "*show-conses*"
	 '(progn (show-conses-insert-lines
		  (show-conses-lisp-and-constree o))
		 (insert show-conses-footer))
	 pos-spec-list))

(defun find-show-conses-lisp-2a (o &rest pos-spec-list)
  (find-2a nil `(find-show-conses-lisp ',o ,@pos-spec-list)))

(defun find-show-conses-lisp-3a (o &rest pos-spec-list)
  (find-3a nil `(find-show-conses-lisp ',o ,@pos-spec-list)
	       '(find-show-conses-regions)))

(defun find-show-conses-lisp-3b (o &rest pos-spec-list)
  (find-3b nil `(find-show-conses-lisp ',o ,@pos-spec-list)
	       '(find-show-conses-regions)))

(defun find-show-conses-lisp-3c (o &rest pos-spec-list)
  (find-3c nil `(find-show-conses-lisp ',o ,@pos-spec-list)
	       '(find-show-conses-regions)))


;;;  _____            _       _           _                            
;;; | ____|_   ____ _| |     | | __ _ ___| |_      ___  _____  ___ __  
;;; |  _| \ \ / / _` | |_____| |/ _` / __| __|____/ __|/ _ \ \/ / '_ \ 
;;; | |___ \ V / (_| | |_____| | (_| \__ \ ||_____\__ \  __/>  <| |_) |
;;; |_____| \_/ \__,_|_|     |_|\__,_|___/\__|    |___/\___/_/\_\ .__/ 
;;;                                                             |_|    
;; «eval-last-sexp»    (to ".eval-last-sexp")
;; See: 
;;   (find-eev-quick-intro "2. Evaluating Lisp")
;;   (find-eev-quick-intro "2. Evaluating Lisp" "M-0 M-e")
;;   (find-efunction 'ee-eval-sexp-eol)
;;   (find-efunction 'ee-eval-sexp-eol "To add a special" "behavior")
;;   (find-eaproposf "ee-eval-last-sexp")
;; Tests:
;;   (eek "<down> M-3 M-3 M-e")
;;        show-conses-demo-lines-4
;;   (eek "<down> M-4 M-4 M-e")
;;        show-conses-demo-lines-4
;; Also, try `M-22e' at the line below:
;;        show-conses-demo-lines-4
;;
(defun ee-eval-last-sexp-22 ()
  "Like `ee-eval-last-sexp', but uses `show-conses-toplain-lines'."
  (interactive)
  (let* ((lines      (ee-eval (ee-read (ee-last-sexp))))
	 (plainlines (show-conses-toplain-lines lines)))
    (message plainlines)))

(defun ee-eval-last-sexp-33 ()
  "Like `ee-eval-last-sexp', but inserts the result at the window at the right."
  (interactive)
  (let* ((lines (ee-eval (ee-read (ee-last-sexp)))))
    (find-show-conses-2a lines)))

(defun ee-eval-last-sexp-44 ()
  "Like `ee-eval-last-sexp', but inserts the result at the window at the right."
  (interactive)
  (let* ((lines (ee-eval (ee-read (ee-last-sexp)))))
    (find-show-conses-2b lines)))



;;;  _____                       _   
;;; | ____|_  ___ __   ___  _ __| |_ 
;;; |  _| \ \/ / '_ \ / _ \| '__| __|
;;; | |___ >  <| |_) | (_) | |  | |_ 
;;; |_____/_/\_\ .__/ \___/|_|   \__|
;;;            |_|                   
;;
;; «export»  (to ".export")
;; See:
;;   (find-eaproposf "show-conses-")
;;   (find-eaproposv "show-conses-")
;;   (find-eloadhistory-for 'show-conses-shorten)
;;   (find-efunction 'find-eloadhistory-for)
;; Test:
;;   (find-eppp (show-conses-loadhistory))
;;   (show-conses-shorten 'variable            'show-conses-markers)
;;   (show-conses-shorten 'function            'show-conses-shorten)
;;   (show-conses-shorten "(defalias '%s '%s)" 'show-conses-shorten)
;;   (show-conses-shorten "(setq %s %s)"       "show-conses-markers")
;;   (find-estring-elisp (show-conses-export0))
;;   (find-epp (ee-read (show-conses-export0)))
;;
(defun show-conses-loadhistory (&optional optsymbol)
  (let* ((symbol (or optsymbol 'show-conses-marker))
	 (fname  (symbol-file symbol)))
    (assoc fname load-history)))

(defun show-conses-shorten (fmtortype stringorsymbol)
  (let* ((origname (format "%s" stringorsymbol))
	 (newname  (replace-regexp-in-string "show-conses" "sc" origname))
	 (fmt      (cond ((stringp fmtortype) fmtortype)
			 ((eq fmtortype 'function) "(defalias '%-20s '%s)\n")
			 ((eq fmtortype 'variable) "(setq      %-20s  %s)\n"))))
    (if (equal newname origname)
	""
      (format fmt newname origname))))

(defun show-conses-export0 ()
  (cl-loop for o in (cdr (show-conses-loadhistory))
	   concat (cond ((symbolp o)
			 (show-conses-shorten 'variable o))
			((eq 'defun (car o))
			 (show-conses-shorten 'function (cdr o))))))

(defun show-conses-export ()
  (eval (ee-read (show-conses-export0))))





;;; __        ___     _ _   _     
;;; \ \      / (_) __| | |_| |__  
;;;  \ \ /\ / /| |/ _` | __| '_ \ 
;;;   \ V  V / | | (_| | |_| | | |
;;;    \_/\_/  |_|\__,_|\__|_| |_|
;;;                               
;; «width»  (to ".width")
;; Tests:
;;                              show-conses-demo-lines-3
;; (show-conses-widths-of-lines show-conses-demo-lines-3)
;; (show-conses-width-lines     show-conses-demo-lines-3)
;;
(defun show-conses-width-line (line)
  (length (show-conses-toplain-line line)))

(defun show-conses-widths-of-lines (lines)
  (mapcar 'show-conses-width-line lines))

(defun show-conses-width-lines (lines)
  (apply 'max (show-conses-widths-of-lines lines)))


;;;  ____           _ 
;;; |  _ \ __ _  __| |
;;; | |_) / _` |/ _` |
;;; |  __/ (_| | (_| |
;;; |_|   \__,_|\__,_|
;;;                   
;; «pad»  (to ".pad")
;; Tests:
;; (show-conses-pad-line 4 '("ab"         "cd"))
;; (show-conses-pad-line 6 '("ab"         "cd"))
;; (show-conses-pad-line 6 '("ab"         "cd") ?_)
;; (show-conses-pad-line 6 '("ab" (m "M") "cd") ?_)
;;
(defun show-conses-pad-line (wtotal line &optional char)
  "Pad LINE to the width WTOTAL."
  (let* ((wleft  (show-conses-width-line line))
	 (wright (- wtotal wleft))
	 (spaces (make-string wright (or char 32))))
    (if (< wleft wtotal)
	(append line (list spaces))
      line)))

(defun show-conses-pad-lines (lines)
  (let ((maxwidth (show-conses-width-lines lines)))
    (cl-loop for line in lines
	     collect (show-conses-pad-line maxwidth line))))


;;;  _     ____  
;;; | |   |  _ \ 
;;; | |   | |_) |
;;; | |___|  _ < 
;;; |_____|_| \_\
;;;              
;; «lr»  (to ".lr")
;; Tests:
;; (find-estring-elisp (show-conses-export0))
;; (show-conses-export)
;; (setq sc-tree-1  '(((h "car"   "*"))))
;; (setq sc-tree-2  '(((h "cadr"  "2"))))
;; (setq sc-tree-3  '(((h "caddr" "3"))))
;; (setq sc-tree-1a (show-conses-add-pin '((h "cr"   ".")) sc-tree-1))
;; (setq sc-tree-2a (show-conses-add-pin '((h "cdr"  ".")) sc-tree-2))
;; (setq sc-tree-3a (show-conses-add-pin '((h "cddr" ".")) sc-tree-3))
;; (setq sc-tree-3b sc-tree-3a)
;; (setq sc-tree-2b (show-conses-l_r sc-tree-2a sc-tree-3b))
;; (setq sc-tree-1b (show-conses-l_r sc-tree-1a sc-tree-2b))
;;
(defun show-conses-add-pin (newtopline lines)
  `(,newtopline ("|") ,@lines))

(defun show-conses-add-hline (lines &optional wtotal)
  (setq wtotal (or wtotal (+ 2 (show-conses-width-lines lines))))
  (let* ((topline    (car lines))
         (otherlines (cdr lines))
         (newtopline (show-conses-pad-line wtotal topline ?_)))
    `(,newtopline ,@otherlines)))

(defun show-conses-pad-bottom (lines newheight)
  (let ((currentheight (length lines)))
    (if (>= currentheight newheight)
	lines
      (let ((newlines (make-list (- newheight currentheight) ())))
	`(,@lines ,@newlines)))))

(defun show-conses-lr (leftlines rightlines)
  (let* ((leftheight  (length leftlines))
         (rightheight (length rightlines))
	 (maxheight   (max leftheight rightheight))
	 (leftlines2  (show-conses-pad-bottom leftlines  maxheight))
	 (rightlines2 (show-conses-pad-bottom rightlines maxheight))
	 (leftlines3  (show-conses-pad-lines  leftlines2)))
    (cl-loop for l in leftlines3
	     for r in rightlines2
	     collect `(,@l ,@r))))

(defun show-conses-l_r (leftlines rightlines)
  (let ((leftlines_ (show-conses-add-hline leftlines)))
    (show-conses-lr leftlines_ rightlines)))


;;;  _____              
;;; |_   _| __ ___  ___ 
;;;   | || '__/ _ \/ _ \
;;;   | || | |  __/  __/
;;;   |_||_|  \___|\___|
;;;                     
;; «tree»  (to ".tree")
;; Tests:
;; (show-conses-toplain-lines (show-conses-constree '(* 2 3)))
;;                            (show-conses-constree '(* 2 3))
;;
(defun show-conses-constree (o)
  (cond ((eq o nil)  `(((h ,(show-conses-context) "nil"))))
	((symbolp o) `(((h ,(show-conses-context) ,(format "%S" o)))))
	((stringp o) `(((h ,(show-conses-context) ,(format "%S" o)))))
	((numberp o) `(((h ,(show-conses-context) ,(format "%S" o)))))
	((listp o)
	 (let* ((pin `(h ,(show-conses-context) "."))
		(down (show-conses-runinsubcontext "a"
			(show-conses-constree (car o))))
		(ltree (show-conses-add-pin `(,pin) down)))
	   (if (not (cdr o))
	       ltree
	     (let ((rtree (show-conses-runinsubcontext "d"
			    (show-conses-constree (cdr o)))))
	       (show-conses-l_r ltree rtree)))))
	(t '(("?")))))

;; Tests:
;; (setq o '(+ (* 2 3) (* 4 5)))
;; (setq lines (show-conses-lisp-and-constree o))
;; (show-conses-toplain-lines lines)
;; (show-conses-delete-markers)
;; (find-show-conses-3c lines)
;;
(defun show-conses-lisp-and-constree (o)
  `(,(show-conses-lisp o)
    ("")
    ,@(show-conses-constree o)))





'("This is a test block!"
  ;; (find-estring-elisp (show-conses-export0))
  (show-conses-export)

  (setq sc-tree-1   '(((h "car"   "*"))))
  (setq sc-tree-2   '(((h "cadr"  "2"))))
  (setq sc-tree-3   '(((h "caddr" "3"))))
  (setq sc-tree-1a (show-conses-add-pin '((h "cr"   ".")) sc-tree-1))
  (setq sc-tree-2a (show-conses-add-pin '((h "cdr"  ".")) sc-tree-2))
  (setq sc-tree-3a (show-conses-add-pin '((h "cddr" ".")) sc-tree-3))
  (setq sc-tree-3b sc-tree-3a)
  (setq sc-tree-2b (show-conses-l_r sc-tree-2a sc-tree-3b))
  (setq sc-tree-1b (show-conses-l_r sc-tree-1a sc-tree-2b))

"--")


(provide 'show-conses)




;; Local Variables:
;; coding:  utf-8-unix
;; End:
