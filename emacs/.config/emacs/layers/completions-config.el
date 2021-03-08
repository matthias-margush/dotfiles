;; -*- mode: emacs-lisp; lexical-binding: t; -*-

(require 'package-config)

(use-package company
  :hook (after-init . global-company-mode)
  :bind (:map company-active-map
              ("C-n" . company-select-next-or-abort)
              ("C-p" . company-select-previous-or-abort))

  :config
  (global-company-mode))

(provide 'completions-config)
