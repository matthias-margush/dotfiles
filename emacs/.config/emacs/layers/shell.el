;; -*- lexical-binding: t; -*-

(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-to-list 'comint-output-filter-functions 'ansi-color-process-output)

(use-package exec-path-from-shell
  :init
  (setq exec-path-from-shell-check-startup-files nil)
  :config
  (exec-path-from-shell-initialize))

(defun eshell-clear ()
  "Clear the eshell buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))

(add-hook 'eshell-mode-hook
          #'(lambda ()
	      (setq-local completion-in-region-function #'ivy-completion-in-region)
	      (setq-local ivy-display-functions-alist nil)
	      (add-to-list 'eshell-visual-commands "docker")
	      (setq eshell-visual-commands (delete "docker" eshell-visual-commands))
	      (local-set-key (kbd "s-k") #'eshell-clear)
	      (local-set-key (kbd "s-h") #'counsel-esh-history)))

;(use-package esh-autosuggest
;  :hook (eshell-mode . esh-autosuggest-mode)
;  :config
  ;; (defun setup-eshell-grouped-backends ()
  ;;   (setq-local company-backends
  ;; 		'((company-capf esh-autosuggest))))

;;;;;; ;;   ;; (add-hook 'eshell-mode-hook #'setup-eshell-grouped-backends)
;;   )

(add-hook 'eshell-mode-hook
          (lambda ()
            (add-to-list 'eshell-visual-commands "ssh")
            (add-to-list 'eshell-visual-commands "tail")
            (add-to-list 'eshell-visual-commands "top")))

(when (require 'ansi-color nil t)
  (defun my-colorize-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (message "enabling ansi colors")
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  (add-hook 'compilation-filter-hook 'my-colorize-compilation-buffer))

(use-package with-editor
  :hook (;; (shell-mode . with-editor-export-editor)
	 ;; (term-mode . with-editor-export-editor)
	 (eshell-mode . with-editor-export-editor)))

(use-package vterm
  :general
  (states 'insert :keymaps '(vterm-mode-map)
          "s-[" #'vterm-send-escape)

  :init
  (setq vterm-always-compile-module t))

;;  prompt
(setq eshell-prompt-function
      (lambda ()
        (concat
         (propertize
          (concat
           "\n"
           (abbreviate-file-name (eshell/pwd))
           )
          'face '(:weight bold))
         "\n❯ ")))
(setq eshell-prompt-regexp "^ ❯ ")

(require 'dired-x)
(add-hook 'dired-load-hook
	  (lambda ()
	    (dired-omit-mode)
	    (require 'dired-x)))
(setq dired-omit-mode t)

;; brew install coreutils
(setq insert-directory-program "/usr/local/bin/gls")
(setq eshell-ls-use-in-dired t)
(setq eshell-highlight-prompt t)
(setq eshell-banner-message "")
(setq eshell-cd-shows-directory nil)

(require 'eshell)
(require 'em-smart)
(setq eshell-where-to-jump 'begin)
(setq eshell-review-quick-commands nil)
(setq eshell-smart-space-goes-to-end t)
(add-hook 'eshell-mode-hook 'eshell-smart-initialize)
