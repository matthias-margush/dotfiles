;; -*- mode: emacs-lisp; lexical-binding: t; -*-

(setq mac-right-command-modifier 'control)

(global-set-key (kbd "s-<down>") #'end-of-buffer)
(global-set-key (kbd "s-<up>") #'beginning-of-buffer)
(global-set-key (kbd "<home>") #'previous-buffer)
(global-set-key (kbd "<end>") #'next-buffer)

(define-key key-translation-map (kbd "M-<down>") (kbd "C-v" )))
(define-key key-translation-map (kbd "M-<up>") (kbd "M-v"))

(define-key key-translation-map (kbd "s-<left>") (kbd "C-a"))
(define-key key-translation-map (kbd "s-<right>") (kbd "C-e"))

(provide 'configure-keys)
