;; -*- mode: emacs-lisp; lexical-binding: t; -*-

(require 'package-config)

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
