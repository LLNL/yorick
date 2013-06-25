;;; yorick.el --- Yorick Development Environment for Emacs
;;; for emacs versions >=19 (UNIX or MS Windows)
;;; $Id: yorick.el,v 1.2 2006-01-30 08:33:03 thiebaut Exp $

;; Copyright (c) 2000 David H. Munro.

;; Author: David H. Munro <dhmunro at users.sf.net>
;; Keywords: yorick

;; yorick.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; yorick.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This module is partly based on yorick-mode.el by Eric Thiebaut
;; and inf-yorick.el by Juan Jose Garcia-Ripoll.  It hopefully supplies
;; all the features of both those packages, and more.

;; INSTRUCTIONS

;; 0. Install yorick, so that typing "yorick" in a shell starts it.
;;    See ftp://ftp-icf.llnl.gov/pub/Yorick/.

;; 1. Load this file, yorick.el, into emacs.

;;    You can do this by placing the following line in your ~/.emacs:
;;      (load "/full/path/to/yorick.el" nil t)
;;    where you fill in /full/path/to appropriately.

;;    Alternatively, if you administer your system and want to make
;;    yorick.el available on a site-wide basis, then put yorick.el in
;;    your site-lisp directory, and put the following in site-start.el:
;;      (autoload 'yorick "yorick" "see yorick.el" t)
;;      (autoload 'yorick-mode "yorick" "see yorick.el" t)
;;      (autoload 'ysh "yorick" "see yorick.el" t)
;;      (autoload 'yssh "yorick" "see yorick.el" t)
;;      (autoload 'yutil-setup "yorick" "see yorick.el" nil)
;;      (if (assoc "\\.i$" auto-mode-alist)
;;          (setcdr (assoc "\\.i$" auto-mode-alist) 'yorick-mode)
;;        (setq auto-mode-alist (append '(("\\.i$" . yorick-mode))
;;              auto-mode-alist)))
;;    The modification to auto-mode-alist overrides the default,
;;    which is to edit *.i files in C-mode.  Very few people even know
;;    what a ".i" C-file is for; however, you may not want to make this
;;    change.  When yorick.el is loaded, the change will be made
;;    anyway, but you may prefer users to be mad at me rather than you.

;; 3. Also in your ~/.emacs (after loading yorick.el if you need to)
;;    you may wish to invoke yutil-setup, which can change the look and
;;    feel of emacs substantially.  If you are already completely
;;    satisfied and comfortable with emacs, just skip this step.

;;    If you have no experience with emacs, and want its mouse and
;;    scrollbar bindings to have a look-and-feel similar to typical
;;    word processing programs (e.g.- MS Word), put this in ~/.emacs:
;;      (yutil-setup t t t t t)

;;    If you want to try my own favorite setup, do this:
;;      (yutil-setup 1 1)
;;    This removes the scrollbars and puts a "windowshade" scrolling
;;    function on the left mouse button to compensate.

;;    See the features section below for details.

;; 4. The command M-x yorick RET launches a yorick process in a comint-mode
;;    buffer called *yoterm*.

;; 5. Opening a yorick .i source file puts that buffer in yorick-mode.
;;    An easy way to do that is to type help RET in the *yoterm* buffer,
;;    then ESC ? to pop to the LINE/FILE.

;; FEATURES

;; 1. yorick-mode: A derivative of c-mode allows you to send
;;    statements, regions, functions, or the whole file to the
;;    yorick process running in the *yoterm* yoterm-mode buffer.

;; Default yorick-mode key bindings that differ from c-mode:
;; Key          Function         Comments

;; RET     newline-and-indent    RET automatically indents next line
;; C-j     newline               C-j to avoid indentation
;; C-x C-e yorick-eval-statement run statement at point in *yoterm*
;; ESC C-x yorick-eval-func      define function at point in *yoterm*
;; C-c C-s yorick-include-file   save then include in *yoterm*
;; C-c i   yorick-show-info      run info command on symbol at point
;; C-c d   yorick-show-help      run help command on symbol at point

;; Assorted useful c-mode bindings:

;; ESC C-f forward-sexp          goto matching ) } or ]
;; ESC C-b backward-sexp         goto matching ( { or [
;; ESC C-a beginning-of-defun    goto beginning of current function
;; ESC C-e end-of-defun          goto end of current function
;; TAB     c-indent-command      indent current line properly
;; ESC C-q c-indent-exp          with point on { indent all lines to }

;; 2. yoterm-mode: a derivative of comint-mode for yorick processes
;;    You ordinarily enter yoterm mode by typing
;;      M-x yorick RET

;; Default yorick-mode key bindings that differ from c-mode:
;; Key          Function         Comments

;; RET     yoterm-send-input
;;   on final line: sends command line to yorick for execution
;;   elsewhere: copies line (minus prompt) to final line
;;              second RET resends it, or you can edit
;; up      yoterm-match-backward
;; down    yoterm-match-forward
;;   anywhere but at end-of-buffer, arrow keys operate as usual
;;   at end-of-buffer:
;;   up arrow recalls previous command line
;;   down arrow recalls next command line
;;     down before any up recalls next line after the last one
;;     recalled, so that RET down RET down RET down etc. will
;;     recall an entire sequence of previous lines (you can edit
;;     any or all in the sequence before hitting RET)
;;   if you begin to type a line, then hit either up or down,
;;   it recalls only lines which match what you have typed so far

;; C-u     yoterm-kill-line-backward  kill line back to prompt
;; C-k     kill-line             kill line forward to end of line
;; C-a     yoterm-bol            goto beginning of line or prompt
;; C-o     comint-kill-output    erase most recent yorick output
;; C-c     yoterm-interrupt      send SIGINT signal to yorick

;; C-.     yofile-pop            open/position the file mentioned in
;;                                 the previous error or help message
;; ESC ?   yofile-pop            (same as C-.)
;; C-,     yoterm-toggle-history show/hide the history buffer
;; C-n     yoterm-toggle-history (same as C-,)
;; TAB     comint-dynamic-complete-filename
;;                               complete filename before point

;; 3. yohist-mode: Every yoterm-mode buffer keeps a record of all
;;    previous input lines in an explicit buffer.  You can recall and
;;    send old lines directly from that history buffer; lines recalled
;;    in this manner become the most-recently-recalled line for the
;;    purposes of the up/down recall mechanism in the yoterm-mode buffer.

;; Default yorick-mode key bindings that differ from c-mode:
;; Key          Function         Comments

;; C-,     yoterm-toggle-history show/hide the history buffer
;; C-n     yoterm-toggle-history (same as C-,)
;; RET     yoterm-hist-input
;;   if you are typing a command line at end-of-buffer RET sends it
;;     (thus you can control yorick from the history buffer)
;;   elsewhere, copy the line containing point to the *yoterm* buffer
;;     and mark it as the one previously recalled for up/down there
;;     a second RET sends the line and advances point to the end of
;;     the next line; a third RET both copies and sends that line,
;;     advancing to the next, and so on

;; 4. M-x ysh RET
;;      starts your UNIX shell (csh, ksh, bash, etc) in a yoterm-mode
;;      buffer.  This is an alternative to shell-mode.
;;    M-x yssh RET
;;      prompts you for a host to connect to using ssh.  The remote
;;      session takes place in a yoterm-mode bufffer.

;; 5. yutil-setup
;;    You can put a call to yutil-setup in your ~/.emacs to alter the
;;    look-and-feel of emacs.  The general form is
;;      (yutil-setup buttons scrollbars mouse-mini mouse-qr hilite)
;;    but you can drop out arguments from right to left to get the
;;    default values.  You can also type this line followed by C-j
;;    in the *scratch* buffer to change settings without exiting emacs.
;;    The possible values of the arguments are as follows:

;;      buttons      nil  use default emacs mouse bindings
;;                   t    use PC-like mouse bindings
;;                   1    use enhanced emacs mouse bindings
;;        By default, emacs binds the mouse buttons as follows:
;;          left button:   click to set editing point
;;                         drag to select region
;;                         double click selects words, triple lines
;;          middle button: click to paste where you click
;;          right button:  click sets end of region begun with left click
;;                           second click at same place kills region
;;                         drag extends selection
;;        You can use scroll bar between left and right clicks to select
;;        multipage regions.  The "enhanced" emacs bindings are the same
;;        except for the left button:
;;          left button:   click to set editing point
;;                         drag vertically to scroll window
;;        The "enhancement" makes it possible to dispense with scroll bars.
;;        The PC-like mouse bindings are similar to the default bindings,
;;        but the selection becomes "pending delete" -- that is, if
;;        something is selected, it will be replaced by what you type.
;;        Details are explained in the documentation for the emacs
;;        commands delete-selection-mode, transient-mark-mode, and
;;        the variable highlight-nonselected-windows.

;;      scrollbars   nil  default emacs scroll bar bindings
;;                   t    use PC-like scroll bar bindings
;;                   1    turn off scroll bars
;;        By default, emacs scroll bars work the same as xterm scroll
;;        bars, which is very different from the way scroll bars work
;;        in typical Windows or MacIntosh applications.  Set this to t
;;        to get the following bindings when you click on a scrollbar:
;;          left button:   click above slider to page up
;;                         click below slider to page down
;;                         drag slider to move long distances
;;          middle button: click to move slider to place you clicked
;;                         drag moves slider as with left button
;;          right button:  drag mouse to do windowshade scrolling
;;                         for fine adjustments
;;        If you set buttons to 1, you don't need scrollbars, and you
;;        set scrollbars to 1 as well.

;;      mouse-mini   nil  default emacs behavior
;;                   t    exits minibuffer when you click outside it
;;        By default, if you click outside the minibuffer when it is
;;        active, emacs allows you to continue editing, but keeps the
;;        minibuffer operation active.  This can be useful, but often
;;        confuses novice users.  Emacs novices should set this to t.

;;      mouse-qr     nil  default query-replace
;;                   t    use an (optionally) mouse-driven query-replace
;;        By default, the emacs find-and-replace function can be
;;        puzzling to novice users.  The mouse-driven version is
;;        identical except that during the query, the menu bar shows
;;        buttons so you don't need to know or remember the keystrokes.
;;      ***breaks modern emacsen, disabled***

;;      hilite       nil  syntax hiliting initially off in yorick-mode
;;                   t    syntax hiliting initially on in yorick-mode
;;        Syntax highlighting colorizes different syntactic elements
;;        so comments show in one color, keywords in another, quoted
;;        strings in a third, and so on.  This can take some time in
;;        a large source file, and the attempts to colorize incomplete
;;        lines as you are editing can be annoying.  If you like it
;;        anyway, you should consider putting hilite to nil and
;;        instead putting the line
;;          (global-font-lock-mode 1)
;;        in your ~/.emacs.  This overrides the hilite argument and
;;        turns on syntax hiliting in *every* emacs buffer.

;; BUGS
;;
;; 1. Input events from mouse clicks on the menu bar and menus are
;;    not delivered to the lisp interpreter until the entire sequence
;;    is complete -- that is, until the sequence [menu-bar menu submenu
;;    ... action] corresponds to a function rather than to another
;;    submenu keymap.  When the sequence finishes, compiled code delivers
;;    the sequence of input events menu-bar, menu, submenu, ..., action.
;;
;;    For emacs 19.29 and earlier (but not in 19.34 and later), if
;;    the menu bar button is bound directly to a function [menu-bar action],
;;    no events at all are delivered to the interpreter, as if the
;;    input sequence never finished.  Hence, menu bar buttons (such as
;;    the STOP button in yoterm-mode, and yutil-query-replace, ybrowse-mode,
;;    and ymenu-list-buffers) which cause immediate actions rather than
;;    popping up a menu do not work in those versions of emacs.
;;
;; 2. Numerous features used here (e.g.- kill-buffer-hook, make-local-hook)
;;    do not exist at all in emacs 19.28, so 19.29 is the earliest version
;;    that will work at all.

(if (or (< emacs-major-version 19)
      (and (= emacs-major-version 19) (< emacs-minor-version 29)))
    (error "You need at least emacs 19.29, preferably 19.34, for yorick.el"))
;; try to get rid of c-mode if it was loaded instead of cc-mode (19.29)
;; this may be antisocial, but cc-mode really is better
(if (not (and (featurep 'c-mode) (not (featurep 'cc-mode)))) nil
  (makunbound 'c-mode-map)
  (makunbound 'c-style-alist))
(require 'cc-mode)  ;; yorick-mode derives from c-mode
(if (fboundp 'c-initialize-cc-mode) (c-initialize-cc-mode))
(require 'comint)   ;; yoterm-mode derives from comint-mode
(require 'easymenu)

;; ------------------------------------------------------------------------

(defvar yorick-executable-name "yorick"
  "*The path name of the yorick executable.")

(defvar yorick-syntax-hilite nil
  "*Non-nil means syntax hilighting initially on in yorick-mode buffers.
Unnecessary if you use global-font-lock-mode function.")

(defvar yorick-mode-map nil
  "Keymap used in yorick-mode buffers.")
(if yorick-mode-map
    nil
  (setq yorick-mode-map (copy-keymap (if (boundp 'c-mode-base-map)
                                         c-mode-base-map
                                       c-mode-map)))
  ;; reverse C-m, C-j from defaults
  (define-key yorick-mode-map "\C-m" 'newline-and-indent)
  (define-key yorick-mode-map "\C-j" 'newline)
  (define-key yorick-mode-map "\C-c\C-s" 'yorick-include-file)
  (define-key yorick-mode-map "\C-ci" 'yorick-show-info)
  (define-key yorick-mode-map "\C-cd" 'yorick-show-help)
  (define-key yorick-mode-map "\C-x\C-e" 'yorick-eval-statement)
  (define-key yorick-mode-map "\e\C-x" 'yorick-eval-func)
  )

(defvar yorick-mode-menu nil)
(easy-menu-define
 yorick-mode-menu yorick-mode-map "Yorick Mode Commands"
 (cons "Yorick"
       `(["Toggle Syntax Hilite"  font-lock-mode t]
         "---"
         ["Insert doc comment"    yorick-insert-doc t]
         ["Show doc comment"      yorick-show-help t]
         ["Show runtime info"     yorick-show-info t]
         "---"
         ["Goto func end"         end-of-defun t]
         ["Goto func top"         beginning-of-defun t]
         "---"
         ["Eval selection"        yorick-eval-region t]
         ["Eval statement"        yorick-eval-statement t]
         ["Eval func"             yorick-eval-func t]
         ["Save and Include"      yorick-include-file t]
         )))

(defvar yorick-mode-abbrev-table nil
  "Abbreviation table used in yorick-mode buffers.")
(define-abbrev-table 'yorick-mode-abbrev-table ())

(defvar yorick-version 1
  "Version of yorick (for document comment format).")

;; ------------------------------------------------------------------------

(if (< emacs-major-version 20)
    ;; these are not defined in emacs19 font-lock.el
    (progn
      (defvar font-lock-builtin-face            'font-lock-reference-face
        "Face name to use for builtins.")
      (defvar font-lock-constant-face           'font-lock-reference-face
        "Face name to use for constant and label names.")
      (defvar font-lock-warning-face            'font-lock-reference-face
        "Face name to use for things that should stand out.")))

(defun xemacs-clean-flkwds (item)
  (cond ((not item) nil)
        ((listp item) (mapcar xemacs-clean-flkwds item))
        ((eq 'font-lock-builtin-face item) 'font-lock-reference-face)
        ((eq 'font-lock-constant-face item) 'font-lock-reference-face)
        (t item)))
(setq xemacs-clean-flkwds (symbol-function 'xemacs-clean-flkwds))

;; by eric thiebaut (thiebaut@obs.univ-lyon1.fr).
(defvar yorick-font-lock-keywords
  '(
    ;;
    ;; Fontify function name definitions.
    ("\\<\\(func\\)[ \t]+\\([_A-Za-z][_A-Za-z0-9]*\\)?"
     (1 font-lock-keyword-face) (2 font-lock-function-name-face nil t))
    ;;
    ;; Fontify function structure definitions.
    ("\\<\\(struct\\)[ \t]+\\(\\([_A-Za-z][_A-Za-z0-9]*\\)[ \t]*{\\)?"
     (1 font-lock-keyword-face) (3 font-lock-type-face nil t))
    ;;
    ;; Fontify warning directives.
    ("\\<\\(FIXME\\):" 1 font-lock-warning-face prepend)
    ("\\<\\(DOCUMENT\\|SEE[ \t]+ALSO\\|KEYWORDS\\)\\>"
     1 font-lock-warning-face prepend)
    ;;
    ;; Fontify #KEYWORD directives (Note: Yorick does
    ;; not care about spaces before/after the `#'):
    ;;   1. fontify filenames in #include <...> directives as strings.
    ("^[ \t]*\\(#[ \t]*include\\)[ \t]*\\(<[^>\"\n]*>\\)"
     (1 font-lock-builtin-face nil t) (2 font-lock-string-face nil t))
    ;;   2. fontify directives.
    ("^[ \t]*\\(#[ \t]*\\(if\\|include\\|endif\\)\\)\\>"
     1 font-lock-builtin-face)
    ;;
    ;; Fontify some common Yorick's functions (only `more_args' and `next_arg'
    ;; are reserved words in Yorick):
    ("\\<\\(am_subroutine\\|catch\\|error\\|include\\|more_args\\|next_arg\\|require\\)\\>" 1 font-lock-builtin-face)
    ;;
    ;; Fontify goto keywords and targets, and case default/goto tags.
    ("\\<\\(goto\\)\\>[ \t]*\\(-?\\sw+\\)?"
     (1 font-lock-keyword-face) (2 font-lock-constant-face nil t))
    ;; Anders Lindgren <andersl@csd.uu.se> points out that it is quicker to use
    ;; MATCH-ANCHORED to effectively anchor the regexp on the left.
    ;; This must come after the one for keywords and targets.
    (":" ("^[ \t]*\\(\\sw+\\)[ \t]*:" (forward-line 0) (end-of-line)
          (1 font-lock-constant-face)))
    ;;
    ;; Fontify Yorick's types/scope specifiers.
    ("\\<\\(c\\(har\\|omplex\\)\\|double\\|extern\\|float\\|int\\|lo\\(cal\\|ng\\)\\|pointer\\|s\\(hort\\|tring\\)\\)\\>" 1 font-lock-type-face)
    ;;
    ;; Fontify Yorick's keywords (`goto' and `func' are handled elsewhere):
    ("\\<\\(break\\|continue\\|do\\|else\\|for\\|if\\|return\\|while\\)\\>"
     1 font-lock-keyword-face)
    ;; Fontify Yorick's Yorick's range functions:
    ("\\<\\(avg\\|cum\\|dif\\|m\\(ax\\|in\\|nx\\|xx\\)\\|p\\(cen\\|sum\\|tp\\)\\|rms\\|sum\\|uncp\\|zcen\\)\\>" 1 font-lock-builtin-face)
    ))

(if (string-match "XEmacs" emacs-version)
    (setq yorick-font-lock-keywords
          (mapcar xemacs-clean-flkwds yorick-font-lock-keywords)))

(defvar yorick-font-lock-defaults
  '((yorick-font-lock-keywords)
    nil nil ((?_ . "w")) beginning-of-defun
    ;; font-lock-comment-start-regexp required for emacs19
    (font-lock-comment-start-regexp . "/[*/]")
    (font-lock-mark-block-function . mark-defun)))

(defconst yorick-class-key "\\(struct\\)")
;(defconst yorick-extra-toplevel-key "\\(extern\\|local\\)[^_]")
(defconst yorick-access-key nil)
(defconst yorick-conditional-key
  "\\b\\(for\\|if\\|do\\|else\\|while\\)\\b[^_]")

(defun yorick-mode ()
  "Major mode for editing Yorick code.

Some commands will start a yorick process (or use one running in an
existing *yoterm* buffer) and send sections of the code in this buffer
to yorick for execution.

The hook variable `yorick-mode-hook' is run with no args, if that
variable is bound and has a non-nil value.  Also the hook
`c-mode-common-hook' is run first.

Key bindings:
\\{yorick-mode-map}"
  (interactive)
  ;; safer to implement yorick-mode differences from c++-mode,
  ;; in order to minimize emacs19-20 changes
  (let (c++-mode-hook)
    (c++-mode))
  (setq major-mode 'yorick-mode
        mode-name "Yorick"
        local-abbrev-table yorick-mode-abbrev-table)
  (use-local-map yorick-mode-map)
  (easy-menu-add yorick-mode-menu)
  (make-local-variable 'font-lock-defaults)
  (setq c-conditional-key yorick-conditional-key
        c-class-key yorick-class-key
        c-access-key yorick-access-key
        font-lock-defaults yorick-font-lock-defaults)
  (and (boundp 'cc-imenu-c-generic-expression)
       (setq imenu-generic-expression cc-imenu-c-generic-expression))
  (setq indent-tabs-mode nil)  ;; source should look same in all editors
  (if yorick-syntax-hilite (font-lock-mode))
  (run-hooks 'yorick-mode-hook)
  (c-update-modeline))

;; recognize yorick .i files and use yorick mode
(if (assoc "\\.i$" auto-mode-alist)
    ;; this presumes user doesn't want .i files to be in C mode (default)
    ;; -- possibly this is antisocial, but an expert can easily put it back
    (setcdr (assoc "\\.i$" auto-mode-alist) 'yorick-mode)
  (setq auto-mode-alist (append '(("\\.i$" . yorick-mode)) auto-mode-alist)))

(defun yorick-include-file ()
  "Include current yorick-mode file, saving it if modified."
  (interactive)
  (if (buffer-modified-p (current-buffer))
      (save-buffer))
  (yoterm-send-string (concat "#include \""
                              (expand-file-name buffer-file-name) "\"")
                      'show))

(defun yorick-show-info (sym)
  "Show runtime info about a symbol (default is symbol containing point)."
  (interactive (list (read-from-minibuffer "Info for: "
                                           (let ((init (yorick-var-at-pt)))
                                             (if init (cons init 0))))))
  (yoterm-send-string (format "info, %s" sym) 'show))

(defun yorick-show-help (sym)
  "Show document comment about a symbol (default is symbol containing point)."
  (interactive (list (read-from-minibuffer "Help for: "
                                           (let ((init (yorick-var-at-pt)))
                                             (if init (cons init 0))))))
  (yoterm-send-string (format "help, %s" sym) 'show))

(defvar yorick-doc-template
  (cond ((>= yorick-version 2)
         "/** NAME: ?
 * USAGE:
 * DESCRIPTION:
 * SEE ALSO:
 */
")
        (t
         "/* DOCUMENT ?
     
   SEE ALSO:
 */
"))
  "Template for yorick document comments, ? where name should go.")

(defun yorick-insert-doc ()
  "Insert yorick document comment near point."
  (interactive)
  (end-of-line)
  (let ((pt (point)))
    (forward-line 0)
    (cond ((and (looking-at "[ \\t]+") (= (match-end 0) pt))
           (end-of-line 0)
           (delete-region (point) pt)
           (forward-line 1))
          ((not (looking-at "{"))
           (goto-char pt)
           (if (eobp) (insert "\n")
             (forward-line 1))))
    (setq pt (point))
    (insert yorick-doc-template)
    (goto-char pt)
    (if (search-forward "?" nil t)
        (delete-backward-char 1))))

(defun yorick-eval-region (start end &optional and-go)
  "Send the current region as input to yorick.
Prefix argument means switch to the *yoterm* buffer afterwards."
  (interactive "r\nP")
  (yoterm-send-string (buffer-substring start end) 'show)
  (if and-go (pop-to-buffer (yoterm-find-buffer))))

(defun yorick-eval-func (&optional and-go)
  "Send func containing point as input to yorick.
Prefix argument means switch to the *yoterm* buffer afterwards."
  (interactive "P")
  (save-excursion
    (beginning-of-defun 1)
    (search-backward-regexp "^func\\b")
    (let ((start (point)))
      (end-of-defun 1)
      (yorick-eval-region start (point))))
  (if and-go (pop-to-buffer (yoterm-find-buffer))))

(defun yorick-eval-statement (&optional and-go)
  "Send func containing point as input to yorick.
Prefix argument means switch to the *yoterm* buffer afterwards."
  (interactive "P")
  (save-excursion
    (c-beginning-of-statement)
    (let ((start (point)))
      (c-end-of-statement)
      (yorick-eval-region start (point))))
  (if and-go (pop-to-buffer (yoterm-find-buffer))))

(defun yorick-var-at-pt ()
  (condition-case ()
      (save-excursion
        (forward-sexp -1)
        (let ((obj (read (current-buffer))))
          (and (symbolp obj) (symbol-name obj))))
    (error nil)))

;; ------------------------------------------------------------------------

(defvar yorick-prompt-pattern "^[a-zA-Z0-9_-]*> *"
  "Regexp to match yorick prompts, must begin with ^.")

(defvar yoterm-mode-hook '()
  "Hook for customizing yorick mode.")

;; special MS Windows hacks
(defvar yoterm-win32 (and (boundp 'window-system)
                          (memq window-system '(w32 win32))))
(if yoterm-win32 (setenv "NO_MDI" "1"))

(defvar yoterm-mode-map nil
  "Keymap used in yoterm-mode buffers.")

(if yoterm-mode-map
    nil
  ;; Keys:
  (setq yoterm-mode-map (make-sparse-keymap))
  (substitute-key-definition 'beginning-of-line 'yoterm-bol
                             yoterm-mode-map global-map)
  (define-key yoterm-mode-map [up] 'yoterm-match-backward)
  (define-key yoterm-mode-map [down] 'yoterm-match-forward)
  (define-key yoterm-mode-map "\C-u" 'yoterm-kill-line-backward)
  (define-key yoterm-mode-map "\C-o" 'comint-kill-output)
  (define-key yoterm-mode-map "\C-m" 'yoterm-send-input)
  (define-key yoterm-mode-map "\C-c" 'yoterm-interrupt)
  (define-key yoterm-mode-map "\M-?" 'yofile-pop)
  (define-key yoterm-mode-map "\t" 'comint-dynamic-complete-filename)
  (define-key yoterm-mode-map "\C-n" 'yoterm-toggle-history)
  ;; note control , . ; ' are all available this way
  (define-key yoterm-mode-map [(control \.)] 'yofile-pop)
  (define-key yoterm-mode-map [(control \,)] 'yoterm-toggle-history)
  )

(defvar yoterm-mode-menu nil)
(easy-menu-define
 yoterm-mode-menu yoterm-mode-map "Yorick Terminal Commands"
 (cons "Yorick"
       `(["STOP"                 yoterm-interrupt t]
         "---"
         ["Toggle Auto dbug>"    yoterm-dbauto t]
         ["Exit dbug>"           yoterm-dbexit t]
         "---"
         ["Toggle History"       yoterm-toggle-history t]
         ["Kill output"          comint-kill-output t]
         ["Recall this"          yoterm-send-input t]
         ["Recall prev"          yoterm-match-backward t]
         ["Recall next"          yoterm-match-forward t]
         ["Goto LINE/FILE"       yofile-pop t]
         ["Change Directory..."  yoterm-cd t]
         )))

(defvar yohist-mode-map nil
  "Keymap used in yohist-mode buffers.")
(if yohist-mode-map
    nil
  ;; Keys:
  (setq yohist-mode-map (make-sparse-keymap))
  (define-key yohist-mode-map "\C-m" 'yoterm-hist-input)
  (define-key yohist-mode-map "\C-n" 'yoterm-toggle-history)
  (define-key yohist-mode-map [(control \,)] 'yoterm-toggle-history)
  )

;; internal variables
(setq yoterm-histbuf nil)
(make-variable-buffer-local 'yoterm-histbuf)
(setq yoterm-termbuf nil)
(make-variable-buffer-local 'yoterm-termbuf)
(setq yoterm-histmark nil)
(make-variable-buffer-local 'yoterm-histmark)
(setq yoterm-prompt-regexp nil)
(make-variable-buffer-local 'yoterm-prompt-regexp)
(setq yoterm-cd-track nil)
(setq yoterm-must-prompt nil)
(setq yoterm-not-yorick nil)
(setq yoterm-cd-function nil)
(make-variable-buffer-local 'yoterm-cd-function)
(if (not (fboundp 'yoterm-true-cd))
    (fset 'yoterm-true-cd (indirect-function 'cd)))

(defun yoterm-mode ()
  "Major mode for running yorick in a terminal window.

Yorick runs in a *yoterm* terminal emulator buffer, and a separate
*yoterm*H buffer contains a history of all input to *yoterm*.  In the
*yoterm*H buffer, C-m is bound to yoterm-hist-input.

The hook variable `yoterm-mode-hook' is run with no args, if that
variable is bound and has a non-nil value.  The `comint-mode-hook'
is run first.

Key bindings:
\\{yoterm-mode-map}"
  (if (not (eq major-mode 'comint-mode))
      (comint-mode))
  (setq major-mode 'yoterm-mode)
  (setq mode-name "Y-Terminal")
  (use-local-map yoterm-mode-map)
  (easy-menu-add yoterm-mode-menu)
  (set-process-sentinel (get-buffer-process (current-buffer)) 'yoterm-died)
  (setq comint-prompt-regexp yorick-prompt-pattern)
  (setq yoterm-prompt-regexp (substring yorick-prompt-pattern 1))
  (setq comint-input-sender (function yoterm-sender))
  (add-hook 'comint-output-filter-functions 'yoterm-strikeout-prompt nil t)
  (if (not yoterm-not-yorick)
      (setq yoterm-cd-function 'yoterm-cd-echo)
    (if (not yoterm-cd-track) nil
      (make-local-variable 'shell-dirstack)
      (setq shell-dirstack nil)
      (make-local-variable 'shell-last-dir)
      (setq shell-last-dir nil)
      (make-local-variable 'shell-dirtrackp)
      (setq shell-dirtrackp t)
      (add-hook 'comint-input-filter-functions
                'shell-directory-tracker nil t)))
  (setq yoterm-histbuf (generate-new-buffer (concat (buffer-name) "H")))
  ;; make-local-hook is obsolete since Emacs 21.1, the work is done
  ;; automatically by add-hook
  (if (or (< emacs-major-version 21)
          (and (= emacs-major-version 21) (< emacs-minor-version 1)))
      (make-local-hook 'kill-buffer-hook))
  (add-hook 'kill-buffer-hook 'yoterm-killbuf nil t)
  (let ((buf (current-buffer)))
    (save-excursion
      (set-buffer yoterm-histbuf)
      (yohist-mode)
      (setq yoterm-termbuf buf)))
  (run-hooks 'yoterm-mode-hook)
  (comint-read-input-ring t))

(defun yohist-mode ()
  "Major mode of history buffer associated with a yoterm-mode buffer.
C-m is bound to yorick-hist-input in this buffer."
  (setq major-mode 'yohist-mode)
  (setq mode-name "Y-History")
  (setq yoterm-histmark (make-marker))
  (set-marker yoterm-histmark 1)
  (make-local-variable 'kill-buffer-query-functions)
  (setq kill-buffer-query-functions '(yoterm-histkill))
  (use-local-map yohist-mode-map))

(defun yoterm-killbuf ()
  (save-excursion
    (set-buffer yoterm-histbuf)
    (setq yoterm-termbuf nil))
  (kill-buffer yoterm-histbuf))

(defun yoterm-histkill ()
  (if (null yoterm-termbuf)
      t
    (bury-buffer)
    nil))

(defun yoterm-died (process event)
  (kill-buffer (process-buffer process)))

(defun yorick (&optional name)
  "Run yorick, with I/O through buffer `*yoterm*'.

If `*yoterm*' already exists, prompt for a terminal name; if it is the
name of an existing yoterm-mode buffer, switch to that buffer, else
start a new yorick process in a buffer of the name you specify.

Key bindings:
\\{yoterm-mode-map}"
  (interactive (yoterm-read-terms "Terminal name: " "yoterm" t))
  (let ((same-window-regexps '(".*")))
    (pop-to-buffer (yoterm-find-buffer name))))

(setq yoterm-accepted-output nil)
(defun yoterm-accept-output (proc timeout)
  (let ((already-got yoterm-accepted-output))
    (setq yoterm-accepted-output nil)
    (or already-got (accept-process-output proc timeout))))

(defun yoterm-find-buffer (&optional name)
  (if (or (not name) (equal name "")) (setq name "yoterm"))
  (let ((bname (concat "*" name "*")))
    (if (not (comint-check-proc bname))
        (let (buf)
          (save-excursion
            (set-buffer (make-comint name yorick-executable-name))
            (setq buf (current-buffer))
            (yoterm-mode)
            (let (got-prompt yoterm-accepted-output)
              (while (not got-prompt)
                (or (yoterm-accept-output
                     (get-buffer-process (current-buffer)) 2)
                    (error "Yorick error or response too slow"))
                (save-excursion
                  (forward-line 0)
                  (setq got-prompt (looking-at comint-prompt-regexp))))))
          buf)
      (get-buffer bname))))

(defun yoterm-read-terms (prompt init immediate)
  (list
   (let ((existing-shells
          (let ((bufs (buffer-list)) names)
            (save-excursion
              (while bufs
                (set-buffer (car bufs))
                (if (eq major-mode 'yoterm-mode)
                    (setq names (cons
                                 (list (substring (buffer-name) 1 -1))
                                 names)))
                (setq bufs (cdr bufs))))
            names)))
     (if (and (not existing-shells) immediate) ""
       (completing-read prompt existing-shells nil nil
                        (if init (cons init 0)))))))

(defvar yoterm-send-invisible nil)
(defun yoterm-sender (proc string)
  (let ((intxt (if (and (not (zerop (length string)))
                        (equal (substring string -1) "\n"))
                   string
                 (concat string "\n"))))
    (if yoterm-send-invisible
        (comint-send-string proc intxt)
      (save-excursion
        (set-buffer yoterm-histbuf)
        (goto-char (point-max))
        (insert intxt))
      (if (< (length intxt) 500)
          (comint-send-string proc intxt)
        ;; make sure big input is sent in line-oriented chunks
        ;; this may not work if individual lines are >500 characters
        (let ((start 0)
              (yoterm-must-prompt t)
              yoterm-accepted-output)
          (while (string-match "\n" intxt start)
            (let ((chunk (match-end 0)))
              (while (and (string-match "\n" intxt chunk)
                          (< (- (match-end 0) start) 500))
                (setq chunk (match-end 0)))
              (and (> start 0);; pause for response on multiple chunks
                   (while yoterm-must-prompt
                     (or (yoterm-accept-output proc 2)
                         (error "Yorick error or response too slow"))))
              (comint-send-string proc (substring intxt start chunk))
              (setq start chunk))))))))

(defun yoterm-send-input ()
  "On final line, send line to yorick; elsewhere, copy line to end.

Precisely, if point is beyond last output from yorick (usually a prompt),
send everything beyond last output to yorick, adding a newline if the
final character is not already a newline.  Otherwise, delete anything
beyond the last yorick output, and copy the current line there -- first
removing any prompt from the beginning of the line -- then move to the
end of the copied line.

Note that two successive yoterm-send-input commands send a previous
line to yorick unmodified.

See also yoterm-match-forward and yoterm-match-backward."
  (interactive)
  (let* ((proc (get-buffer-process (current-buffer)))
         (pmark (marker-position (process-mark proc))))
    (end-of-line)
    (cond ((>= (point) pmark)
           (comint-send-input))
          (t
           (save-excursion
             (goto-char (point-max))
             (if (> (point) pmark)
                 (delete-region pmark (point))))
           (comint-copy-old-input)))))

(defun yoterm-send-string (string &optional record)
  (let* ((buf (yoterm-find-buffer))
         (proc (get-buffer-process buf)))
    (if (not record)
        (comint-send-string proc (concat string))
      (save-excursion
        (set-buffer buf)
        (goto-char (process-mark proc))
        (if (not (eobp))
            (delete-region (point) (point-max)))
        (insert string)
        (comint-send-input))
      (cond ((not (eq record t))
             (let ((win (display-buffer buf)))
               (set-buffer buf)
               (set-window-point win (point-max))))))))

(defun yoterm-hist-input ()
  "Copy current line to end of *yoterm* buffer, sending it if at eob.

Precisely, copy current line to end of *yoterm* buffer, and set the
history marker for yoterm-match-forward (in *yoterm* buffer) as if
this line had been recalled by the history recall mechanism.
Additionally, if this line is at the end of the history buffer,
send it to yorick as well (this allows you to type input to yorick
from the *yoterm*H buffer as well as from the *yoterm* buffer).

Finally, if this command is given twice consecutively, the line copied
to the yorick buffer the first time is sent to yorick the second time,
and the cursor moves to the end of the next line.  A third time both
copies and sends the following line, again advancing to the next line.
A sequence of yoterm-hist-input commands thus allows you to resend
a series of lines to yorick.

See also yoterm-match-forward and yoterm-match-backward."
  (interactive)
  (forward-line 0)
  (let ((beg (point)))
    (end-of-line)
    (let ((win (get-buffer-window yoterm-termbuf t))
          (end (point))
          (at-end (eobp))
          (send-it (or (eobp)
                       (eq last-command 'yoterm-hist-input)))
          (proc (get-buffer-process yoterm-termbuf))
          (string (buffer-substring beg (point))))
      (save-excursion
        (set-buffer yoterm-termbuf)
        (let ((pmark (marker-position (process-mark proc))))
          (goto-char (point-max))
          (if (> (point) pmark)
              (delete-region pmark (point)))
          (insert string)
          (if send-it (yoterm-send-input))
          (cond (win
                 (set-window-point win (point-max))))))
      (cond (at-end
             (delete-region beg end)
             (goto-char (point-max)))
            (send-it
             (forward-line 1)
             (end-of-line))))
    (set-marker yoterm-histmark beg)))

(defun yoterm-interrupt ()
  "Send SIGINT \(ctrl-c) to yorick, and erase any unsent input."
  (interactive)
  (let ((proc (get-buffer-process (current-buffer))))
    (if (not proc) (error "Current buffer has no process")
      (let ((pmark (marker-position (process-mark proc))))
        (goto-char (point-max))
        (if (> (point) pmark)
            (delete-region pmark (point)))
        (if yoterm-win32
            (process-send-string proc "\03\n")
          (comint-interrupt-subjob))))))

(defvar yoterm-match nil)
(make-variable-buffer-local 'yoterm-match)

(defun yoterm-match (pmark arg)  ; run at eob in command window buffer
  (if (memq last-command '(yoterm-match-backward
                           yoterm-match-forward))
      yoterm-match
    (save-excursion
      (set-buffer yoterm-histbuf)
      (if (> arg 0)
          (goto-char (point-max))
        (goto-char yoterm-histmark) (end-of-line)))
    (setq yoterm-match
          (concat "^"
                  (if (= pmark (point-max))
                      ".+"
                    (regexp-quote (buffer-substring pmark (point-max))))))))

(defun yoterm-match-grab (string arg)
  (save-excursion
    (set-buffer yoterm-histbuf)
    (end-of-line (if (> arg 0) 0 1))
    (cond ((re-search-backward string nil t arg)
           (set-marker yoterm-histmark (match-beginning 0))
           (end-of-line)
           (buffer-substring yoterm-histmark (point))))))

(defun yoterm-match-backward (arg)
  "Recall previous input line, matching what you have typed so far.

This action assumes point is at end-of-buffer; if not, yorick-match-backward
is the same as previous-line (which just moves point up a line).  The idea
is to bind yoterm-match-backward to the up arrow key.

The line is recalled from the *yoterm*H buffer, which is marked so
that subsequent calls to yoterm-match-forward or yoterm-match backward
move forward or backward in the history buffer.  The first call to
yoterm-match-backward resets the mark in *yoterm*H and recalls the
most recently sent line.  However, the first call to yoterm-match-forward
does reset the line, making it easy to resend a sequence of old input
lines.

Sending a line from the *yoterm*H buffer using yoterm-hist-input also
sets the line which will be recalled by yoterm-match-forward.

If you have begun to type a new line, yoterm-match-backward or
yoterm-match-forward only recall lines matching what you have typed.

After recalling a line, you may edit it, then send it with
yoterm-send-input."
  (interactive "p")
  (if (not (eobp))
      (previous-line arg)
    (let* ((pmark (marker-position
                   (process-mark (get-buffer-process (current-buffer)))))
           (string (yoterm-match-grab (yoterm-match pmark arg) arg)))
      (cond (string
             (delete-region pmark (point-max))
             (insert string))
            (t
             (message "Not found")
             (ding))))))

(defun yoterm-match-forward (arg)
  "Recall next input line, matching what you have typed so far.

This action assumes point is at end-of-buffer; if not, yorick-match-forward
is the same as next-line (which just moves point up a line).  The idea
is to bind yoterm-match-forward to the down arrow key.

The line is recalled from the *yoterm*H buffer, which is marked so
that subsequent calls to yoterm-match-forward or yoterm-match backward
move forward or backward in the history buffer.  The first call to
yoterm-match-backward resets the mark in *yoterm*H and recalls the
most recently sent line.  However, the first call to yoterm-match-forward
does reset the line, making it easy to resend a sequence of old input
lines.

Sending a line from the *yoterm*H buffer using yoterm-hist-input also
sets the line which will be recalled by yoterm-match-forward.

If you have begun to type a new line, yoterm-match-backward or
yoterm-match-forward only recall lines matching what you have typed.

After recalling a line, you may edit it, then send it with
yoterm-send-input."
  (interactive "p")
  (if (not (eobp))
      (let (next-line-add-newlines)
        (next-line arg))
    (yoterm-match-backward (- arg))))

(defun yoterm-dbexit ()
  "Exit dbug> mode."
  (interactive)
  (yoterm-send-string "dbexit" 'show))

(defun yoterm-dbauto ()
  "Toggle automatic entry to dbug> mode."
  (interactive)
  (yoterm-send-string "dbauto" 'show))

(defun yoterm-bol ()
  "Goto beginning of line, except forward over any prompt string."
  (interactive)
  (forward-line 0)
  (if (looking-at comint-prompt-regexp)
      (goto-char (match-end 0))))

(defun yoterm-kill-line-backward ()
  "Kill to beginning of line, except any prompt string."
  (interactive)
  (let (beg)
    (save-excursion
      (yoterm-bol)
      (setq beg (point)))
    (if (> (point) beg) (kill-region beg (point))
      (ding))))

; eliminate multiple prompts at the beginning of a line
; -- should be in comint-output-filter-functions
(defun yoterm-strikeout-prompt (string)
  (let ((pmark (process-mark (get-buffer-process (current-buffer)))) beg)
    (setq yoterm-accepted-output t)
    (save-excursion
      (goto-char comint-last-output-start)
      (forward-line 0)
      (setq beg (point))
      (goto-char pmark)
      (cond ((re-search-backward comint-prompt-regexp beg t)
             (let ((bol (point)) eom)
               (goto-char (match-end 0))
               (while (looking-at yoterm-prompt-regexp)
                 (setq eom (point))
                 (goto-char (match-end 0)))
               (if eom (delete-region bol eom)))
             (setq yoterm-must-prompt nil)))
      (goto-char pmark)
      (while (re-search-backward "[\C-m\C-g]" comint-last-output-start t)
        (if (eq ?\C-g (char-after (point))) (ding))
        (replace-match "")))))

(defun yoterm-show-terminal ()
  "Show *yoterm* terminal window."
  (interactive)
  (let (pop-up-frames)
    (delete-other-windows
     (get-buffer-window (pop-to-buffer (yoterm-find-buffer))))))

(defun yoterm-toggle-history ()
  "Toggle terminal window only and terminal+history window."
  (interactive)
  (cond (yoterm-histbuf   ;; this is a terminal buffer
         (cond ((get-buffer-window yoterm-histbuf)
                (delete-other-windows)
                (bury-buffer yoterm-histbuf))
               (t
                (let ((pop-up-windows t))
                  (pop-to-buffer yoterm-histbuf)))))
        (yoterm-termbuf   ;; this is a history buffer
         (let ((buf (current-buffer))
               pop-up-frames)
           (delete-other-windows
            (get-buffer-window (pop-to-buffer yoterm-termbuf)))
           (bury-buffer buf)))
        (t                ;; this is some other kind of buffer
         (yoterm-show-terminal))))

(defun yoterm-cd (dir)
  "Make DIR become the current buffer's default directory.
If your environment includes a `CDPATH' variable, try each one of that
colon-separated list of directories when resolving a relative directory name.
If yoterm-cd-function is defined, call it with dir."
  (interactive
   (list (read-file-name "Change default directory: "
                         default-directory default-directory
                         (and (member cd-path '(nil ("./")))
                              (null (getenv "CDPATH"))))))
  (yoterm-true-cd dir)
  (if yoterm-cd-function (funcall yoterm-cd-function dir)))

(defun yoterm-cd-echo (dir)
  (yoterm-send-string (concat "cd, \"" dir "\"") 'show))

(defvar yoterm-replace-cd t
  "Set to nil before loading yorick.el to avoid replacing cd by yoterm-cd.")
(if yoterm-replace-cd
    (fset 'cd (symbol-function 'yoterm-cd)))

;; ------------------------------------------------------------------------

(defvar yofile-regexp ""
  "Regexp to recognize line number and file for yofile.
yofile-line and yofile-file are the subexpression numbers for
the line number and file name, respectively.")

(defvar yofile-line 1
  "Subexpression number of line number in yofile-regexp.")

(defvar yofile-file 2
  "Subexpression number of file in yofile-regexp.")

(defvar yofile-expander 'yofile-default-expander
  "Nil or a function which takes a filename argument and expands it
to a full pathname for use by the yofile function.")

(defvar yofile-expander-context nil
  "Passed to yofile-expander after the name to be expanded -- typically
a list of directory names to be searched.")

(defun yofile-expand (name context)
  "Expands NAME by yofile-expander, or just returns name.  The CONTEXT
argument is typically nil, but might be a list of directories to search."
  (if yofile-expander (funcall yofile-expander name context) name))

(defun yofile ()
  "Pop up file mentioned in most recent occurrence of yofile-regexp
and put point at the specified line number.  The regexp can be on the
current line, or any previous line."
  (let ((p (point)))
    (end-of-line)
    (unwind-protect
        (re-search-backward yofile-regexp)
      (goto-char p))
    (let ((line (buffer-substring (match-beginning yofile-line)
                                  (match-end yofile-line)))
          (file (yofile-expand
                 (buffer-substring (match-beginning yofile-file)
                                   (match-end yofile-file))
                 yofile-expander-context))
          (pop-up-windows t)
          window)
      (pop-to-buffer (let ((find-file-existing-other-name t))
                       (find-file-noselect file)))
      (goto-line (string-to-int line)))))

(defun yofile-default-expander (name context)
  "The default yofile-expander expands NAME according to the CONTEXT.  If
CONTEXT is nil, NAME is returned.  Otherwise, CONTEXT is a list of directory
pathnames; these are searched until a file of the given NAME is found."
  (if (not context)
      (if (file-readable-p name) name)
    (yofile-locate name context)))

(defun yofile-locate (name context)
  (while (and context
              (not (file-readable-p (concat (car context) name))))
    (setq context (cdr context)))
  (if context (concat (car context) name)))

(defun yofile-pop ()
  "Pop up the file for the most recent Yorick error message."
  (interactive)
  (let ((yofile-regexp "LINE: *\\([0-9]+\\) *FILE: *\\([^\r\n]+\\)")
        (yofile-line 1)
        (yofile-file 2)
        yofile-expander)
    (yofile)))

;; ------------------------------------------------------------------------

(if (< emacs-major-version 21)
    (fset 'hacked-perform-replace (symbol-function `perform-replace))
  (defun hacked-perform-replace (from-string replacements
                                 query-flag regexp-flag delimited-flag
                                 &optional repeat-count map)
    (perform-replace from-string replacements nil nil
                     query-flag regexp-flag delimited-flag
                     repeat-count map)))

(defun yutil-query-replace (from-string to-string &optional arg start end)
  "Like query-replace, but offers choices in menu bar.
WARNING - breaks newer versions of emacs."
  (interactive (query-replace-read-args "Query replace" nil))
  (let ((local-lookup-key (symbol-function 'lookup-key))
        (map (yutil-nuke-menu-bar (copy-keymap query-replace-map)))
        (query-replace-highlight t))
    (define-key map [menu-bar BACK] '("BACK" . backup))
    (define-key map [menu-bar SKIP] '("SKIP" . skip))
    (define-key map [menu-bar REPLACE] '("REPLACE" . act))
    (define-key map [menu-bar ALL] '("ALL" . automatic))
    (define-key map [menu-bar FINAL] '("FINAL" . act-and-exit))
    (define-key map [menu-bar DONE] '("DONE" . exit))
    (unwind-protect
        (progn
          ;; egregious hack -- overrides mechanism perform-replace
          ;; function uses to lookup key in query-replace-map
          (fset 'lookup-key
                (lambda (map key)
                  (if (equal key [(menu-bar)])
                      (setq key (vector 'menu-bar (read-event))))
                  (funcall local-lookup-key map key)))
          (let ((query-replace-map map)
                (overriding-terminal-local-map map)
                (overriding-local-map-menu-flag t)
                inhibit-local-menu-bar-menus)
            (hacked-perform-replace from-string to-string t nil arg)))
      (fset 'lookup-key local-lookup-key)
      (force-mode-line-update))))

(defun yutil-wreplace ()
  "Query-replace words -- FROM-STRING only found on word boundaries."
  (interactive)
  (let ((current-prefix-arg 1))
    (call-interactively 'yutil-query-replace)))

(defun yutil-case-sensitize ()
  "Make all searching case sensitive -- see case-fold-search."
  (interactive)
  (setq case-fold-search nil))

(defun yutil-case-desensitize ()
  "Make all searching case insensitive -- see case-fold-search."
  (interactive)
  (setq case-fold-search t))

(put 'yutil-case-sensitize 'menu-enable 'case-fold-search)
(put 'yutil-case-desensitize 'menu-enable '(not case-fold-search))

(defun yutil-nuke-menu-bar (map)
  "Undefine all current local and global menu-bar bindings in MAP."
  (let ((syms (append (yutil-menu-events (current-global-map))
                      (yutil-menu-events (current-local-map)))))
    (while syms
      (define-key map (vector 'menu-bar (car syms)) 'undefined)
      (setq syms (cdr syms))))
  map)

(defun yutil-menu-events (map)
  (if map
      (let ((bar (lookup-key map [menu-bar])) syms)
        (while bar
          (if (and (consp (car bar))
                   (symbolp (car (car bar)))
                   (not (string-match "^mouse-.?$"
                                      (symbol-name (car (car bar))))))
              (setq syms (append (list (car (car bar))) syms)))
          (setq bar (cdr bar)))
        syms)))

;; place and windowshade scrolling function (bind to [down-mouse-X] event)
(defun yutil-mouse-drag-scroll (start-event)
  "Set point to mouse, then scroll to keep point on same line as mouse."
  (interactive "e")
  (run-hooks 'mouse-leave-buffer-hook)
  (let* (did-drag
         (start-posn (event-start start-event))
         (start-col-row (posn-col-row start-posn))
         (row (cdr start-col-row))
         (start-point (posn-point start-posn))
         (start-window (posn-window start-posn))
         (start-frame (window-frame start-window))
         (bounds (window-edges start-window))
         (top (nth 1 bounds))
         (bottom (if (window-minibuffer-p start-window)
                     (nth 3 bounds)
                   ;; Don't count the mode line.
                   (1- (nth 3 bounds)))))
    (select-window start-window)
    (goto-char start-point)
    (let (event end end-point delta)
      (track-mouse
        (while (progn
                 (setq event (read-event))
                 (or (mouse-movement-p event)
                     (eq (car-safe event) 'switch-frame)))

          (if (eq (car-safe event) 'switch-frame)
              nil ;; Ignore switch-frame events.
            (if (and (not did-drag)
                     (not (equal (posn-col-row (event-start event))
                                 start-col-row)))
                (setq did-drag t))
            (setq end (event-end event)
                  end-point (posn-point end))

            (cond

             ;; Are we moving within the original window?
             ((and (eq (posn-window end) start-window)
                   (integer-or-marker-p end-point))
              (setq delta
                    (- (cdr (posn-col-row end)) row))
              (condition-case nil
                  (progn (scroll-down delta)
                         (setq row (+ row delta)))
                (error nil)))

             (t
              (let ((mouse-row (cdr (cdr (mouse-position)))))
                (cond
                 ((null mouse-row))
                 ((< mouse-row top)
                  (mouse-scroll-subr start-window (- top mouse-row))
                  (goto-char (window-start (selected-window))))
                 ((and (not (eobp))
                       (>= mouse-row bottom))
                  (let ((jump (- bottom (1+ mouse-row))))
                    (mouse-scroll-subr start-window jump)
                    (goto-char (window-end (selected-window)))
                    (vertical-motion (1- jump))))))))))))
  did-drag))

;; windowshade on scrollbar (bind to [vertical-scroll-bar down-mouse-X] event)
(defun yutil-shade-scroll-bar (event)
  "Scroll to keep line level with mouse (like a windowshade)."
  (interactive "e")
  (let* (done winrow winpnt sup sdn ntot winhgt
         (echo-keystrokes 0)
         (end-position (event-end event))
         (window (nth 0 end-position))
         (before-scroll))
    (save-excursion
      (set-buffer (window-buffer window))
      (setq before-scroll point-before-scroll))
    (save-selected-window
      (select-window window)
      (setq winhgt (1- (window-height)))
      (setq winrow (scroll-bar-scale (nth 2 end-position) winhgt))
      (setq before-scroll
            (or before-scroll (point))))
    (track-mouse
      (while (not done)
        (setq event (read-event))
        (if (eq (car-safe event) 'mouse-movement)
            (setq event (read-event)))
        (cond ((eq (car-safe event) 'scroll-bar-movement)
               (let* ((pos (event-start event))
                      (window (nth 0 pos))
                      (pair (nth 2 pos)))
                 (save-selected-window
                   (select-window window)
                   (let ((row (scroll-bar-scale pair winhgt)))
                     (scroll-down (- row winrow))
                     (setq winrow row)))))
              (t
               ;; Exit when we get the drag event; ignore that event.
               (setq done t)))))
    (sit-for 0)
    (save-excursion
      (set-buffer (window-buffer window))
      (setq point-before-scroll before-scroll))))

;; pc-like scroll bar (bind to [vertical-scroll-bar down-mouse-X] event)
;; based on scroll-bar-drag from scroll-bar.el
(defun yutil-pc-scroll-bar (event)
  "Scroll the window by dragging the scroll bar slider.
If you click outside the slider, the window scrolls one page up or down."
  (interactive "e")
  (let* (done winrow winpnt sup sdn ntot winhgt
         (echo-keystrokes 0)
         (end-position (event-end event))
         (window (nth 0 end-position))
         (before-scroll))
    (save-excursion
      (set-buffer (window-buffer window))
      (setq before-scroll point-before-scroll))
    (save-selected-window
      (select-window window)
      ;; get winrow so clicking on scroll bar doesn't instantly jump
      (setq ntot (- (point-max) (point-min)))
      (setq winhgt (1- (window-height)))
      (setq winpnt (+ (point-min)
                      (scroll-bar-scale (nth 2 end-position) ntot)))
      (if (< winpnt (window-start))
          (setq sdn t)
        (setq winrow (1- (count-lines (window-start) winpnt)))
        (if (>= winrow winhgt) (setq sup t))
        (if sup (setq winrow nil)))
      ;; make it easier to grab tiny scroll bar slider in huge buffers
      (cond ((and (not winrow) (> winhgt 0))
             (let ((sbtop (window-start)) (sbbot (window-end))
                   (sberr (truncate (* yutil-slider-rows (/ ntot winhgt)))))
               (cond ((and (< (- sbtop sbbot) (* sberr 2))
                           (< (abs (- winpnt (/ (+ sbtop sbbot) 2))) sberr))
                      (cond (sup
                             (setq winrow winhgt)
                             (setq sup nil))
                            (sdn
                             (setq winrow 0)
                             (setq sdn nil))))))))
      (setq before-scroll
            (or before-scroll (point))))
    (track-mouse
      (while (not done)
        (setq event (read-event))
        (if (eq (car-safe event) 'mouse-movement)
            (setq event (read-event)))
        (cond ((eq (car-safe event) 'scroll-bar-movement)
               (cond
                (winrow
                 (let* ((pos (event-start event))
                        (window (nth 0 pos))
                        (pair (nth 2 pos)))
                   (save-excursion
                     (set-buffer (window-buffer window))
                     (goto-char
                      (+ (point-min)
                         (scroll-bar-scale pair
                                           (- (point-max) (point-min)))))
                     (forward-line 0)
                     (forward-line (- winrow))
                     (set-window-start window (point)))))))
              (t
               ;; Exit when we get the drag event; ignore that event.
               (setq done t)))))
    (sit-for 0)
    (cond ((or sup sdn)
           (save-selected-window
             (select-window window)
             (if sdn (scroll-down)
               (if sup (scroll-up))))))
    (save-excursion
      (set-buffer (window-buffer window))
      (setq point-before-scroll before-scroll))))

(defvar yutil-slider-rows 0.2
  "*Floating point fractional number of rows for minimum effective size
of the scrollbar slider for yutil-pc-scroll-bar.  A bigger number makes
it easier to grab the slider in huge buffers when the slider is tiny.")

(defun yutil-mouse-exit-minibuffer ()
  (let ((mini (active-minibuffer-window)))
    (and mini
         (listp last-command-event)
         (let ((win (posn-window (event-start last-command-event))))
           (and (not (eq win mini))
                (not (eq (window-buffer win) (get-buffer "*Completions*")))
                (keyboard-escape-quit))))))

(setq yutil-setup nil)

(defun yutil-setup (&optional buttons scrollbars mouse-mini mouse-qr hilite)
  "Set up various emacs options.  Intended to be called in ~/.emacs
after yorick.el has been loaded.  The general command for ~/.emacs is:

\(yutil-setup buttons scrollbars mouse-mini mouse-qr hilite\)

where:

buttons =     nil   use default emacs mouse bindings
              t     use PC-like mouse bindings \(see mouse-sel-mode\)
              1     use mouse bindings designed for no scrollbars
                      \(left button does windowshade scrolling\)

scrollbars =  nil   use default emacs scrollbars
              t     use PC-like scrollbar bindings \(see yutil-scroll-mode\)
              1     turn off scrollbars

mouse-mini =  nil   default emacs behavior
              t     mouse click outside active minibuffer exits it
                    \(highly recommended for emacs novices\)

mouse-qr =    nil   default emacs behavior
              t     use mouse-driven query-replace function
         DISABLED because it breaks newer versions of emacs

hilite =      nil   syntax highlighting off by default
              t     syntax highlighting on by default in yorick-mode
                    \(unnecessary if you set global-font-lock-mode\)

Read yorick.el for examples."
  (if hilite (setq yorick-syntax-hilite t)
    (setq yorick-syntax-hilite nil))
  (if (not (string-match "XEmacs" emacs-version))
      (yutil-setup-1 buttons scrollbars mouse-mini mouse-qr)
    (delete-selection-mode (if (not (equal buttons 1)) 1 -1))))

;; these do not work in xemacs
(defun yutil-setup-1 (buttons scrollbars mouse-mini mouse-qr)
  (remove-hook 'mouse-leave-buffer-hook 'yutil-mouse-exit-minibuffer)
  (if mouse-mini
      (add-hook 'mouse-leave-buffer-hook 'yutil-mouse-exit-minibuffer))
  ;(if mouse-qr
  ;    (substitute-key-definition 'query-replace 'yutil-query-replace
  ;                               global-map)
  ;    (substitute-key-definition 'yutil-query-replace 'query-replace
  ;                               global-map))
  (if (not (boundp 'mouse-sel-default-bindings))
      (setq mouse-sel-default-bindings 'interprogram-cut-paste))
  (require 'mouse-sel)
  ; work around 21.3 mouse-sel.el bug/feature:
  (if (boundp 'mouse-sel-original-interprogram-cut-function)
      (setq mouse-sel-original-interprogram-cut-function
            interprogram-cut-function
            mouse-sel-original-interprogram-paste-function
            interprogram-paste-function))
  (if yutil-setup nil
    (setq yutil-setup
          (list (lookup-key (current-global-map) [S-down-mouse-1])
                (lookup-key (current-global-map) [S-mouse-1])
                (lookup-key (current-global-map)
                            [vertical-scroll-bar down-mouse-1])
                (lookup-key (current-global-map)
                            [vertical-scroll-bar down-mouse-3])
                (lookup-key (current-global-map) [vertical-scroll-bar mouse-1])
                (lookup-key (current-global-map)
                            [vertical-scroll-bar drag-mouse-1]))))
  (global-unset-key [down-mouse-3])
  (cond ((not (equal buttons 1))
         (if (fboundp 'mouse-sel-mode)
             (mouse-sel-mode 1)
           ;; Primary selection bindings.
           (global-unset-key [mouse-1])
           (global-unset-key [drag-mouse-1])
           (global-unset-key [mouse-3])
           (global-set-key [down-mouse-1]       'mouse-select)
           (if (eq mouse-sel-default-bindings 'interprogram-cut-paste)
               nil
             (global-set-key [mouse-2]          'mouse-insert-selection)
             (setq interprogram-cut-function nil
                   interprogram-paste-function nil))
           (global-set-key [down-mouse-3]       'mouse-extend)
           ;;
           ;; Secondary selection bindings.
           (global-unset-key [M-mouse-1])
           (global-unset-key [M-drag-mouse-1])
           (global-unset-key [M-mouse-3])
           (global-set-key [M-down-mouse-1]     'mouse-select-secondary)
           (global-set-key [M-mouse-2]          'mouse-insert-secondary)
           (global-set-key [M-down-mouse-3]     'mouse-extend-secondary))
         (transient-mark-mode 1)
         (setq highlight-nonselected-windows nil)
         (delete-selection-mode 1))
        (t
         (if (fboundp 'mouse-sel-mode)
             (mouse-sel-mode -1)
           ;; Primary selection bindings.
           (global-set-key [mouse-1]    'mouse-set-point)
           (global-set-key [mouse-2]    'mouse-yank-at-click)
           (global-set-key [mouse-3]    'mouse-save-then-kill)
           (global-set-key [down-mouse-1]               'mouse-drag-region)
           (global-set-key [drag-mouse-1]               'mouse-set-region)
           (global-set-key [double-mouse-1]     'mouse-set-point)
           (global-set-key [triple-mouse-1]     'mouse-set-point)
           ;; Secondary selection bindings.
           (global-set-key [M-mouse-1]  'mouse-start-secondary)
           (global-set-key [M-mouse-2]  'mouse-yank-secondary)
           (global-set-key [M-mouse-3]  'mouse-secondary-save-then-kill)
           (global-set-key [M-drag-mouse-1]     'mouse-set-secondary)
           (global-set-key [M-down-mouse-1]     'mouse-drag-secondary))
         (transient-mark-mode -1)
         (setq highlight-nonselected-windows t)
         (delete-selection-mode -1)))
  (global-set-key [S-down-mouse-1] (nth 0 yutil-setup))
  (global-set-key [S-mouse-1] (nth 1 yutil-setup))
  (global-set-key [vertical-scroll-bar down-mouse-1] (nth 2 yutil-setup))
  (global-set-key [vertical-scroll-bar down-mouse-3] (nth 3 yutil-setup))
  (global-set-key [vertical-scroll-bar mouse-1] (nth 4 yutil-setup))
  (global-set-key [vertical-scroll-bar drag-mouse-1] (nth 5 yutil-setup))
  (cond ((equal buttons 3)
         (global-set-key [down-mouse-3] 'yutil-mouse-drag-scroll)
         (global-unset-key [S-mouse-1])
         (global-set-key [S-down-mouse-1] 'mouse-extend))
        ((equal buttons 1)
         (global-set-key [down-mouse-1] 'yutil-mouse-drag-scroll)))
  (cond ((equal scrollbars 1)
         (scroll-bar-mode -1))
        (t
         (scroll-bar-mode 1)
         (cond (scrollbars
                (global-set-key [vertical-scroll-bar down-mouse-1]
                                'yutil-pc-scroll-bar)
                (global-set-key [vertical-scroll-bar down-mouse-3]
                                'yutil-shade-scroll-bar)
                (global-unset-key [vertical-scroll-bar mouse-1])
                (global-unset-key [vertical-scroll-bar drag-mouse-1]))))))

;; ------------------------------------------------------------------------
;; ysh and yssh are similar to ordinary shell-mode,
;; but with history in a separate buffer, as in yoterm-mode

(defvar ysh-prompt-pattern "^[^#$%> ]*\\([#$%>] *\\|\\[[0-9][0-9]*]#? *\\)"
  "Regexp to match shell prompts.")
;; "^[^#$%>\n]*[#$%>] *" is shell-mode default

(defvar ysh-password-prompt-regexp
  "\\(\\([Oo]ld \\|[Nn]ew \\|Kerberos \\|'s \\|login \\|^\\)\
[Pp]assword\\( (again)\\)?\\|pass phrase\\|Enter passphrase\\)\
\\( for [^@ \t\n]+@[^@ \t\n]+\\)?:\\s *\\'"
  "*Regexp matching prompts for passwords in the inferior process.
This is used by `comint-watch-for-password-prompt' in ysh.")

(defvar ysh-mode-hook '()
  "Hook for customizing ysh generated yoterm mode buffers.
The yoterm-mode-hook runs first.")

(defvar yssh-explicit-file-name "ssh"
  "File name of ssh executable for yssh.")

(defvar ysh-mode-map nil
  "Keymap used in ysh-mode buffers.")

(if ysh-mode-map
    nil
  ;; Keys:
  (setq ysh-mode-map (make-sparse-keymap))
  (substitute-key-definition 'beginning-of-line 'yoterm-bol
                             ysh-mode-map global-map)
  (define-key ysh-mode-map [up] 'yoterm-match-backward)
  (define-key ysh-mode-map [down] 'yoterm-match-forward)
  (define-key ysh-mode-map "\C-u" 'yoterm-kill-line-backward)
  (define-key ysh-mode-map "\C-o" 'comint-kill-output)
  (define-key ysh-mode-map "\C-m" 'yoterm-send-input)
  (define-key ysh-mode-map "\C-c" 'ysh-interrupt)
  (define-key ysh-mode-map "\C-d" 'ysh-self-send)
  (define-key ysh-mode-map "\C-z" 'ysh-self-send)
  (define-key ysh-mode-map "\C-\\" 'ysh-self-send)
  (define-key ysh-mode-map "\t" 'comint-dynamic-complete-filename)
  (define-key ysh-mode-map "\C-n" 'yoterm-toggle-history)

  ;; yorick
  (define-key ysh-mode-map [menu-bar yorick]
    (cons "Ysh" (make-sparse-keymap "Ysh")))
  (define-key ysh-mode-map [menu-bar yorick yoterm-hist]
    '("Toggle History" . yoterm-toggle-history))
  (define-key ysh-mode-map [menu-bar yorick yoterm-kill]
    '("Kill output" . comint-kill-output))
  (define-key ysh-mode-map [menu-bar yorick yoterm-this]
    '("Recall this" . yoterm-send-input))
  (define-key ysh-mode-map [menu-bar yorick yoterm-prev]
    '("Recall prev" . yoterm-match-backward))
  (define-key ysh-mode-map [menu-bar yorick yoterm-next]
    '("Recall next" . yoterm-match-forward))
  )

(defun ysh (&optional name)
  "Run a shell, with I/O through buffer *NAME*.

This is like shell-mode, but with the two-buffer command history
mechanism of yoterm-mode.  See shell command for more help."
  (interactive (yoterm-read-terms "Shell name: " "ysh" t))
  (let ((same-window-regexps '(".*")))
    (pop-to-buffer (ysh-find-buffer name))))

(defun yssh (&optional name)
  "Run an ssh secure shell, NAME is `host' or `user@host'.

This is like shell-mode, but with the two-buffer command history
mechanism of yoterm-mode.  See shell command for more help."
  (interactive (yoterm-read-terms "Host (or user@host): " nil nil))
  (if (or (not name) (equal name ""))
      (error "No host name specified")
    (let ((host name) user)
      (if (string-match "^\\(.+\\)@.+" name)
          (progn
            (setq user (substring name 0 (match-end 1)))
            (setq host (substring name (+ (match-end 1) 1)))))
      (let ((explicit-shell-file-name yssh-explicit-file-name)
            (explicit-ssh-args (if user
                                   (list "-l" user host)
                                 (list host)))
            (same-window-regexps '(".*")))
        (pop-to-buffer (ysh-find-buffer name t))))))

(defun ysh-find-buffer (&optional name no-dirtrack)
  (require 'shell)    ;; for shell-directory-tracker
  (if (or (not name) (equal name "")) (setq name "ysh"))
  (let ((bname (concat "*" name "*")))
    (if (not (comint-check-proc bname))
        (let* (buf
               (prog (or explicit-shell-file-name
                         (getenv "ESHELL")
                         (getenv "SHELL")
                         "/bin/sh"))                 
               (sname (file-name-nondirectory prog))
               (startfile (concat "~/.emacs_" sname))
               (xargs-name (intern-soft (concat "explicit-" sname "-args"))))
          (save-excursion
            (set-buffer (apply 'make-comint name prog
                               (if (file-exists-p startfile) startfile)
                               (if (and (boundp xargs-name) xargs-name)
                                   (symbol-value xargs-name)
                                 '("-i"))))
            (setq buf (current-buffer))
            (let ((yoterm-mode-map ysh-mode-map)
                  (yorick-prompt-pattern ysh-prompt-pattern)
                  (yoterm-not-yorick t)
                  (yoterm-cd-track (not no-dirtrack)))
              (yoterm-mode))
            (make-local-variable 'comint-password-prompt-regexp)
            (setq comint-password-prompt-regexp ysh-password-prompt-regexp)
            (add-hook 'comint-output-filter-functions
                      'ysh-watch-for-password-prompt nil t)
            (run-hooks 'ysh-mode-hook))
          buf)
      (get-buffer bname))))

(defun ysh-self-send ()
  "Send the character to the underlying process."
  (interactive)
  (goto-char (point-max))
  (process-send-string nil (char-to-string last-input-char)))

(defun ysh-interrupt ()
  "Send C-c to underlying process."
  (interactive)
  (goto-char (point-max))
  (let ((p (marker-position
            (process-mark (get-buffer-process (current-buffer))))))
    (if (< p (point))
        (delete-region p (point))))
  (process-send-string nil "\C-c"))

(defun ysh-watch-for-password-prompt (string) 
  "Prompt in the minibuffer for password and send without echoing.
This function uses `send-invisible' to read and send a password to the buffer's
process if STRING contains a password prompt defined by 
`comint-password-prompt-regexp'.

This function could be in the list `comint-output-filter-functions'."
  (if (string-match comint-password-prompt-regexp string)
      (let ((yoterm-send-invisible t)) (send-invisible nil))))

;; ------------------------------------------------------------------------

(provide 'yorick)
;;; yorick.el ends here
