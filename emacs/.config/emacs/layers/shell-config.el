;; -*- lexical-binding: t; -*-

(require 'package-config)
(require 'evil-config)

;; (setq
;;  display-buffer-alist
;;  `(("\\*\\(?:shell\\|compilation\\|.*vterm\\)\\*"
;;     display-buffer-in-direction
;;     ;; display-buffer-in-previous-window
;;     (window-height . 20)
;;     (preserve-size . (nil . nil))
;;     (window-parameters . ((no-other-window . nil)
;;                           (no-delete-other-windows . nil))))))

;; (setq
;;  display-buffer-alist
;;  `(("\\*\\(?:shell\\|compilation\\|.*vterm\\)\\*"
;;     display-buffer-in-side-window
;;     (side . bottom)
;;     (slot . -1)
;;     (background-color . "red")
;;     (window-height . 20)
;;     (preserve-size . (nil . t))
;;     (window-parameters . ((no-other-window . nil)
;;                           (no-delete-other-windows . nil))))))

;; slow
;; (use-package fish-completion
;;   :init
;;   (when (and (executable-find "fish")
;;              (require 'fish-completion nil t))
;;     ;; (setq shell-file-name "/usr/local/bin/fish")
;;     (global-fish-completion-mode)))

(setq shell-prompt-pattern "^❯ ")

(eval-after-load "term"
  '(define-key term-raw-map (kbd "s-v") 'term-paste))

(setq explicit-shell-file-name "/bin/zsh")

(add-hook 'shell-mode-hook (lambda () (setq comint-process-echoes t)))
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
;; (add-to-list 'comint-output-filter-functions 'ansi-color-process-output)

(defun me/comint-clear-buffer-and-show-history ()
  "Clear the buffer and pull up history"
  (interactive)
  (comint-clear-buffer)
  (consult-history))

(define-key comint-mode-map (kbd "s-h") #'me/comint-clear-buffer-and-show-history)
(define-key comint-mode-map (kbd "s-k") #'comint-clear-buffer)

(setq eshell-buffer-maximum-lines (* 1024 10))
(add-hook 'eshell-output-filter-functions #'eshell-truncate-buffer)

(defun eshell-clear ()
  "Clear the eshell buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))

(defalias 'e #'find-file-other-window)
(defalias 'v #'eshell-exec-visual)

(defun me/eshell-clear-buffer-and-show-history ()
  "Clear the buffer and pull up history"
  (interactive)
  (eshell-clear)
  (consult-history))

(defun me/eshell-rc ()
  (setenv "PAGER" "cat")
  (setenv "MANPAGER" "cat")
  (local-set-key (kbd "s-k") #'eshell-clear)
  (local-set-key (kbd "s-h") #'me/eshell-clear-buffer-and-show-history)

  (general-define-key
   :states 'insert
   :keymaps '(eshell-mode-map)
   "M-p" #'eshell-previous-input
   "M-n" #'eshell-next-input
   "s-h" #'me/eshell-clear-buffer-and-show-history))

(add-hook 'eshell-mode-hook #'me/eshell-rc)

(when (require 'ansi-color nil t)
  (defun my-colorize-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (message "enabling ansi colors")
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  (add-hook 'compilation-filter-hook 'my-colorize-compilation-buffer))

(use-package with-editor
  :hook ((shell-mode . with-editor-export-editor)
	 ;; (term-mode . with-editor-export-editor)
	 ;; (vterm-mode . with-editor-export-editor)
	 (eshell-mode . with-editor-export-editor)))

(use-package vterm
  :hook (vterm-mode . me/term-background-color)
  :general
  (states 'insert :keymaps '(vterm-mode-map)
          "s-[" #'vterm-send-escape)

  :init
  (defun me/term-background-color ()
    ;; (setq-local header-line-format '(" "))
    (linum-mode -1)
    (let ((bg accent-theme--inverted-background-medium))
      (face-remap-add-relative 'default (list :background bg))
      ;; (face-remap-add-relative 'header-line (list :background bg))
      (face-remap-add-relative 'linum (list :background bg))
      (setq-local header-line-format
                  '((:eval (propertize "☰ vterm" 'face 'font-lock-keyword-face))))
      ))

  (setq vterm-always-compile-module t
        vterm-clear-scrollback-when-clearing t))

;;  prompt
(setq eshell-prompt-function
      (lambda ()
        (concat
         "\n"
         (abbreviate-file-name (eshell/pwd))
         "\n❯ ")))
(setq eshell-prompt-regexp "^❯ ")

(add-hook 'dired-mode-hook
	  (lambda ()
            (require 'dired-x)
            (dired-omit-mode)))

;; brew install coreutils
(setq insert-directory-program "/usr/local/bin/gls")
(setq eshell-ls-use-in-dired t)
(setq eshell-highlight-prompt t)
(setq eshell-banner-message "")
(setq eshell-cd-shows-directory nil)

(run-with-idle-timer 5 nil (lambda () (require 'eshell)))

;; (require 'em-smart)
;; (setq eshell-where-to-jump 'begin)
;; (setq eshell-review-quick-commands nil)
;; (setq eshell-smart-space-goes-to-end t)
;; (add-hook 'eshell-mode-hook 'eshell-smart-initialize)

(provide 'shell-config)
