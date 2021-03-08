;; -*- mode: emacs-lisp; lexical-binding: t; -*-

(require 'evil-config)

(setq me/toggles-map (make-sparse-keymap))

(define-key me/toggles-map (kbd "r") #'rainbow-mode)
(define-key me/toggles-map (kbd "v") #'visible-mode)
(define-key me/toggles-map (kbd "w") #'whitespace-mode)

(general-define-key :states 'normal :prefix leader "T" me/toggles-map)

(provide 'toggles-config)