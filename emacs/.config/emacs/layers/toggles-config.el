;; -*- mode: emacs-lisp; lexical-binding: t; -*-

(require 'evil-config)

(setq me/toggles-map (make-sparse-keymap))

(define-key me/toggles-map (kbd "r") #'rainbow-mode)
(define-key me/toggles-map (kbd "v") #'visible-mode)
(define-key me/toggles-map (kbd "w") #'toggle-word-wrap)
(define-key me/toggles-map (kbd "W") #'whitespace-mode)
(define-key me/toggles-map (kbd "d") #'toggle-debug-on-error)
(define-key me/toggles-map (kbd "p") #'orgraphy-mode)
(define-key me/toggles-map (kbd "f") #'font-lock-mode)
(define-key me/toggles-map (kbd "m") #'smerge-mode)
(define-key me/toggles-map (kbd "t") #'toggle-truncate-lines)

(general-define-key :states 'normal :prefix leader "t" me/toggles-map)

(provide 'toggles-config)
