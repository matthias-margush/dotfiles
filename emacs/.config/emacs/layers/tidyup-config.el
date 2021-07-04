;; -*- mode: emacs-lisp; lexical-binding: t; -*-

;;; Clean up superflous visual elements, messages, and behavioral defaults

(setq scroll-margin 0
      scroll-conservatively 101
      scroll-up-aggressively 0.01
      scroll-down-aggressively 0.01
      scroll-preserve-screen-position nil
      auto-window-vscroll nil)

(setq-default indent-tabs-mode nil)
(setq max-specpdl-size 13000)           ; bigger stack
(setq ffap-machine-p-known 'reject) 	; don't magically try to complete links
(setq history-length 1000)              ; bigger histories

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
(advice-add 'tree-sitter-langs-install-grammars :around #'me/suppress-messages)

(defun display-startup-echo-area-message ()
  "Redefine to clean up startup echo area message."
  (message ""))

;; Suppress "package cl is deprecated"
(setq byte-compile-warnings '(cl-functions))

;; Disable text wrapping generally
(setq-default truncate-lines t)

;;  Literal noise
(setq ring-bell-function 'ignore)

;; help with big files
(setq bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)

(superword-mode)

(setq mac-option-modifier 'meta)

(provide 'tidyup-config)
