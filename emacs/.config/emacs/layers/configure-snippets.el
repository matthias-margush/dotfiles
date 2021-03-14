;; -*- mode: emacs-lisp; lexical-binding: t; -*-

(mm/package 'yasnippet)
(global-set-key (kbd "s-y") #'yas-insert-snippet)
(setq yas-verbosity 0)
(yas-global-mode t)

(mm/package 'yasnippet-snippets)

(provide 'configure-snippets)
