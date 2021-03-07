;; -*- mode: emacs-lisp; lexical-binding: t; -*-

;;; Clean up superflous visual elements, messages, and behavioral defaults

(setq max-specpdl-size 13000)           ; bigger stack

;;; Cleanup frame visual distractions
(setq frame-alist
  '((top . 100)
    (left . 1000)
    (width . 90)
    (height . 60)
    (internal-border-width . 20)
    (vertical-scroll-bars . nil)
    (font . "Hasklug Nerd Font Mono-10")))
(setq default-frame-alist frame-alist
      initial-frame-alist frame-alist)

(add-hook 'after-init-hook
          (lambda ()
            (set-face-attribute 'variable-pitch nil :font "Open Sans Condensed-16")
            (set-face-attribute 'fixed-pitch nil :font "Hasklug Nerd Font Mono-10")))

(setq-default fringe-indicator-alist nil) ; fringe wrap arrows
(fringe-mode '(8 . 4))                  ; fringe
(scroll-bar-mode -1)                    ; scrollbars
(setq frame-title-format '("\n"))       ; frame titles
(setq initial-scratch-message "")       ; scratch buffer content
(setq ns-use-proxy-icon nil)            ; frame icon
(tool-bar-mode -1)                      ; toolbar

(setq ffap-machine-p-known 'reject) 	; don't magically try to complete links

(global-set-key (kbd "s-}") #'ns-prev-frame)
(global-set-key (kbd "s-{") #'ns-next-frame)
(global-set-key (kbd "s-t") #'make-frame)

;;; Clean up noisy functions
(defun me/suppress-messages (func &rest args)
  "Suppress message output from FUNC."
  (cl-flet ((silence (&rest args1) (ignore)))
    (advice-add 'message :around #'silence)
    (unwind-protect
        (apply func args)
      (advice-remove 'message #'silence))))

;; Suppress "Cleaning up the recentf...done (0 removed)"
(advice-add 'recentf-cleanup :around #'me/suppress-messages)
(advice-add 'recentf-load-list :around #'me/suppress-messages)

(defun display-startup-echo-area-message ()
  "Redefine to clean up startup echo area message."
  (message ""))

;; Suppress "package cl is deprecated"
(setq byte-compile-warnings '(cl-functions))

;; Disable text wrapping generally
(setq-default truncate-lines t)

;;;  Literal noise
(setq ring-bell-function 'ignore)

(setq mac-option-modifier 'meta)

(provide 'tidyup-config)
