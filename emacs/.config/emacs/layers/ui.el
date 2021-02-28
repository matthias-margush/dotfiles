(global-set-key [wheel-right] (lambda () (interactive) (scroll-left 1)))
(global-set-key [wheel-left] (lambda () (interactive) (scroll-right 1)))

;; History
(savehist-mode 1) ;; minibuffer history
(recentf-mode)

;; (setq header-line-format (me/tab-echo))
;; (setq header-line-format (projectile-project-name))
;; (set-face-attribute 'header-line nil :underline (face-foreground 'default))

;; (defun me/header-line ()
;;   (concat " . " (which-function)))

(defun me/project-to-buffer-name ()
  ;; (file-relative-name buffer-file-truename (cdr-safe (project-current)))
  (let* ((name (or buffer-file-truename (buffer-name)))
         (project (cdr-safe (project-current)))
         (name (file-relative-name name project)))
    (combine-and-quote-strings
     (split-string name "/+")
     " ❯ ")))

(setq-default mode-line-buffer-identification '(:eval (me/project-to-buffer-name)))

(require 'subr-x)

(defun show-file-name ()
  "Show the full path file name."
  (interactive)
  (message "[%s] %s" (line-number-at-pos) (buffer-file-name)))

(global-set-key (kbd "C-S-G") #'show-file-name)

(setq initial-major-mode 'fundamental-mode)

(use-package editorconfig :config (editorconfig-mode))

(use-package default-text-scale
    :bind (("s-=" . default-text-scale-increase)
           ("s-+" . default-text-scale-increase)
           ("s--" . default-text-scale-decrease)
           ("s-0" . default-text-scale-reset))
    :config (default-text-scale-mode t))

(use-package rainbow-mode)

(require 'dired-x)
(add-hook 'dired-load-hook
          (lambda ()
            (dired-omit-mode)
            (require 'dired-x)))
(setq dired-omit-mode t)


;;; unbind annoying keys
(unbind-key (kbd "s-m"))
(unbind-key (kbd "s-l"))
(unbind-key (kbd "s-p"))

(use-package helpful
    :init
  (setq counsel-describe-function-function #'helpful-callable)
  (setq counsel-describe-variable-function #'helpful-variable))

(use-package adaptive-wrap
    :hook (visual-line-mode . adaptive-wrap-prefix-mode)
    :config
    (setq-default adaptive-wrap-extra-indent 2))
