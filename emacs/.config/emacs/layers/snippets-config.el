;; -*- mode: emacs-lisp; lexical-binding: t; -*-

(use-package yasnippet
  :bind ("s-y" . yas-insert-snippet)
  :init
  (setq yas-verbosity 0)
  :config
  (yas-global-mode t))

(use-package yasnippet-snippets
  :after yasnippet)

(provide 'snippets-config)
