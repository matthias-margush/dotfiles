;; -*- mode: emacs-lisp; lexical-binding: t; -*-

(require 'evil-config)

(defvar me/pairing nil "Whether in pairing mode")

(defun me/toggle-pairing ()
  "Toggles useful when pairing."
  (interactive)
  (if me/pairing
      (progn
        (setq me/pairing nil)
        (global-linum-mode -1)
        (global-hl-line-mode -1))
    (setq me/pairing t)
    (global-linum-mode)
    (global-hl-line-mode)))

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
(define-key me/toggles-map (kbd "p") #'me/toggle-pairing)
(define-key me/toggles-map (kbd "s") #'evil-ex-nohighlight)

(general-define-key :states 'normal :prefix leader "t" me/toggles-map)
(general-define-key :states 'normal :prefix leader "s" #'evil-ex-nohighlight)

(provide 'toggles-config)
