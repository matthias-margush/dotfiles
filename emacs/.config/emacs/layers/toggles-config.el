;; -*- mode: emacs-lisp; lexical-binding: t; -*-

(require 'evil-config)

(setq me/toggles-map (make-sparse-keymap))

(define-key me/toggles-map (kbd "r") #'rainbow-mode)
(define-key me/toggles-map (kbd "v") #'visible-mode)
(define-key me/toggles-map (kbd "w") #'whitespace-mode)
(define-key me/toggles-map (kbd "d") #'toggle-debug-on-error)

(general-define-key :states 'normal :prefix leader "t" me/toggles-map)

(provide 'toggles-config)
