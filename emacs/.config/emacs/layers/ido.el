;; -*- lexical-binding: t; -*-

(use-package flx-ido
  :demand t
  :config (flx-ido-mode t))

(use-package ido
  :ensure nil
  :demand t
  :bind ((:map ido-common-completion-map))
  :init
  (setq ido-max-window-height 1
	ido-enable-last-directory-history t
	ido-record-commands t
	ido-virtual-buffers t)
  :config
  (ido-mode)
  (ido-everywhere))

(use-package ido-completing-read+
  :demand t
  :after ido
  :config
  (ido-ubiquitous-mode t))

(use-package amx
  :demand t
  :config
  (amx-mode))

;; IDO in places like describe-face
(use-package crm-custom
  :demand t
  :config
  (crm-custom-mode 1))

(require 'icomplete)
(icomplete-mode)

(use-package company
  :defer 2
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

