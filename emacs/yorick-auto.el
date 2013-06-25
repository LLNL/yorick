;; autoloads for yorick.el
;; put yorick-auto.el and yorick.el in site-lisp directory and
;; (load "yorick-auto")        in site-start.el
(autoload 'yorick "yorick" "see yorick.el in site-lisp dir" t)
(autoload 'ysh "yorick" "see yorick.el in site-lisp dir" t)
(autoload 'yssh "yorick" "see yorick.el in site-lisp dir" t)
(autoload 'yorick-mode "yorick" "see yorick.el in site-lisp dir" t)
(autoload 'yutil-setup "yorick" "see yorick.el in site-lisp dir" nil)
(if (assoc "\\.i\\'" auto-mode-alist)
    ;; this presumes user doesn't want .i files to be in C mode (default)
    ;; -- possibly this is antisocial, but an expert can easily put it back
    (setcdr (assoc "\\.i\\'" auto-mode-alist) 'yorick-mode)
  (setq auto-mode-alist (append '(("\\.i\\'" . yorick-mode)) auto-mode-alist)))
