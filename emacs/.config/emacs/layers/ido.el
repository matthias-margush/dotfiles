;; -*- mode: emacs-lisp; lexical-binding: t; -*-

(use-package ido
  :ensure nil
  :bind ((:map ido-common-completion-map))
  :init
  (setq ido-max-window-height 1
        ido-enable-last-directory-history t
        ido-record-commands t
        ido-virtual-buffers t)
  :config
  (ido-mode)
  ;; (ido-everywhere)
  )

(use-package amx 			;; show recent first
  :config
  (amx-mode))

(use-package company
  :hook (after-init . global-company-mode)
  :bind (:map company-active-map
              ("C-n" . company-select-next-or-abort)
              ("C-p" . company-select-previous-or-abort)
              ("C-s" . company-select-next-or-abort)
              ("C-r" . company-select-previous-or-abort))

  :init
  (setq company-frontends '(company-echo-strip-common-frontend))

  :config
  (defadvice eldoc-display-message-no-interference-p
      (after dont-show-when-isearching activate)
    "Prevents eldoc from interfering with company-echo."
    (setq ad-return-value (and ad-return-value
                               (not company-candidates))))

  (global-company-mode))
