;; -*- mode: emacs-lisp; lexical-binding: t; -*-

(require 'package-config)
(require 'evil-config)

(use-package company
  :demand t

  :init
  (setq company-selection-wrap-around t)

  :config
  (define-key evil-insert-state-map (kbd "C-n") nil)
  (evil-define-key 'insert global-map (kbd "C-SPC") #'company-complete)

  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous)
  (global-company-mode))

(provide 'completions-config)
