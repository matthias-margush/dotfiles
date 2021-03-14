;; -*- mode: emacs-lisp; lexical-binding: t; -*-

(mm/package 'helpful)

(define-key help-map (kbd "f") #'helpful-callable)
(define-key help-map (kbd "v") #'helpful-variable)
(define-key help-map (kbd "k") #'helpful-key)
(define-key help-map (kbd "C") #'helpful-command)

(mm/package 'which-key)
(setq which-key-show-early-on-C-h t
      which-key-idle-delay 2.0
      which-key-allow-evil-operators t
      which-key-popup-type 'minibuffer)

(which-key-mode)

(provide 'configure-help)
