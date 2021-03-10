;; -*- mode: emacs-lisp; lexical-binding: t; -*-

(require 'package-config)

(use-package company
  :demand t

  :init
  (setq company-selection-wrap-around t)

  :config
  (global-company-mode))

(provide 'completions-config)
