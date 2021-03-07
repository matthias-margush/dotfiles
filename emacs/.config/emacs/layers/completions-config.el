;; -*- mode: emacs-lisp; lexical-binding: t; -*-

(require 'package-config)

(use-package company
  :hook (after-init . global-company-mode)
  :bind (:map company-active-map
              ("C-n" . company-select-next-or-abort)
              ("C-p" . company-select-previous-or-abort))

  ;;             ("C-s" . company-select-next-or-abort)
  ;;             ("C-r" . company-select-previous-or-abort))


  :init
  ;; (setq company-backends '(company-capf))
  ;; (setq company-backends
  ;;       '(company-bbdb
  ;;         company-semantic
  ;;         company-cmake
  ;;         company-capf
  ;;         company-clang
  ;;         company-files
  ;;         (company-dabbrev-code company-gtags company-etags company-keywords)
  ;;         company-oddmuse
  ;;         company-dabbrev))
  ;; (setq company-frontends '(company-echo-strip-common-frontend))
  ;; (setq company-frontends '(counsel-company))

  :config
  (company-tng-configure-default)

  ;; (defadvice eldoc-display-message-no-interference-p
  ;;     (after dont-show-when-isearching activate)
  ;;   "Prevents eldoc from interfering with company-echo."
  ;;   (setq ad-return-value (and ad-return-value
  ;;                              (not company-candidates))))

  (global-company-mode))

(provide 'completions-config)
