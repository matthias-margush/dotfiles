;; -*- mode: emacs-lisp; lexical-binding: t; -*-

(require 'package-config)

(unless (display-graphic-p)
  (menu-bar-mode -1))

(global-set-key (kbd "s-}") #'ns-prev-frame)
(global-set-key (kbd "s-{") #'ns-next-frame)
(global-set-key (kbd "s-t") #'make-frame)

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

(use-package construction-paper-theme
  :custom
  (window-divider-default-right-width 100)

  :straight (construction-paper-theme :type git :host github :repo "matthias-margush/construction-paper-emacs")

  :init
  (defun me/modeline-style-line (&optional arg)
    "Style the mode line a simple line."
    (let ((box-color "#BC7C49"))

      (set-face-attribute
       'window-divider nil
       :background box-color
       :foreground box-color
       :underline nil
       :box `(:line-width 1 :color ,box-color) :height 0.1)


      (set-face-attribute
       'mode-line nil
       :inherit 'default
       :underline t
       :foreground box-color
       :box `(:line-width 1 :color ,box-color) :height 0.1)

      (set-face-attribute
       'mode-line-inactive nil
       :background nil
       :foreground box-color
       :underline t
       :box `(:line-width 1 :color ,box-color) :height 0.1)

      (set-face-attribute
       'mode-line-inactive nil
       :inherit 'default
       :foreground box-color
       :underline t
       :box `(:line-width 1 :color ,box-color) :height 0.1)))

  (setq-default mode-line-format '(""))
  (advice-add 'enable-theme :after #'me/modeline-style-line)
  (add-to-list 'after-make-frame-functions #'me/modeline-style-line t)

  :config
  (require 'construction-paper-theme)
  (construction-paper-theme-light))

(defface header-line-path
  '((t :inherit variable-pitch))
  "Face for the header line path.")

(which-func-mode)

(setq-default
 header-line-format
 '((:propertize "⧉" face bold)
   " "
   (:propertize mode-line-buffer-identification face header-line-path)
   (:propertize (:eval (me/echo-which-func)) face which-func)))

(defun me/echo-which-func ()
  "Which function string for display."
  (if-let ((fn (which-function)))
      (concat "   λ " fn)))

(defun me/project-to-buffer-name ()
  (if buffer-file-truename
      (let* ((name buffer-file-truename)
             (project (cdr-safe (project-current)))
             (name (file-relative-name name project)))
        (combine-and-quote-strings
         (split-string name "/+")
         " ⧸ "))
    (buffer-name)))

(setq-default mode-line-buffer-identification '(:eval (me/project-to-buffer-name)))

(provide 'theme-config)
