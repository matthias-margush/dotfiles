;; -*- lexical-binding: t; -*-

(require 'package-config)
(require 'evil-config)

(use-package fish-completion
  :init
  (when (and (executable-find "fish")
             (require 'fish-completion nil t))
    ;; (setq shell-file-name "/usr/local/bin/fish")
    (global-fish-completion-mode)))

(setq shell-prompt-pattern "^❯ *")

;; (add-hook 'shell-mode-hook (lambda () (setq comint-process-echoes t)))
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
;; (add-to-list 'comint-output-filter-functions 'ansi-color-process-output)

(define-key comint-mode-map (kbd "s-h") #'consult-history)
(define-key comint-mode-map (kbd "s-k") #'comint-clear-buffer)

(defun eshell-clear ()
  "Clear the eshell buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))

(defalias 'e #'find-file-other-window)

(defun me/eshell-rc ()
  (setenv "PAGER" "cat")
  (setenv "MANPAGER" "cat")
  (local-set-key (kbd "s-k") #'eshell-clear)
  (local-set-key (kbd "s-h") #'consult-history)
  (general-define-key
   :states 'insert
   :keymaps '(eshell-mode-map)
   "M-p" #'eshell-previous-input
   "M-n" #'eshell-next-input))

(add-hook 'eshell-mode-hook #'me/eshell-rc)

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
         "\n"
         (abbreviate-file-name (eshell/pwd))
         "\n❯ ")))
(setq eshell-prompt-regexp "^❯ ")

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
;; (require 'em-smart)
;; (setq eshell-where-to-jump 'begin)
;; (setq eshell-review-quick-commands nil)
;; (setq eshell-smart-space-goes-to-end t)
;; (add-hook 'eshell-mode-hook 'eshell-smart-initialize)

(provide 'shell-config)
