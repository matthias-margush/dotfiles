(tool-bar-mode -1)
(desktop-save-mode t)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

(defun show-file-name ()
  "Show the full path file name."
  (interactive)
  (message "[%s] %s" (line-number-at-pos) (buffer-file-name) ))

(global-set-key (kbd "C-S-G") #'show-file-name)

(setq initial-major-mode 'fundamental-mode)

(use-package construction-paper-theme
  :straight (construction-paper-theme :type git :host github :repo "matthias-margush/construction-paper-emacs")

  :init
  (defun me/modeline-style-line (&optional arg)
    "Style the mode line a simple line."
    (set-face-attribute 'mode-line nil :background nil :foreground nil :underline t :box nil)
    (set-face-attribute 'mode-line nil :inherit 'default :underline t)
    (set-face-attribute 'mode-line-inactive nil :background nil :foreground nil :underline t :box nil)
    (set-face-attribute 'mode-line-inactive nil :inherit 'default :underline t))

  (setq-default mode-line-format '(""))
  (advice-add 'enable-theme :after #'me/modeline-style-line)
  (add-to-list 'after-make-frame-functions #'me/modeline-style-line)

  :config
  (require 'construction-paper-theme)
  (construction-paper-theme-light))

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

(unbind-key (kbd "s-m"))
(unbind-key (kbd "s-l"))

(use-package helpful
  :bind ("C-h k" . helpful-key)

  :init
  (setq counsel-describe-function-function #'helpful-callable)
  (setq counsel-describe-variable-function #'helpful-variable))
