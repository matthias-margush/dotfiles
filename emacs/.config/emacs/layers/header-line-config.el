;; -*- mode: emacs-lisp; lexical-binding: t; -*-

(defface header-line-path
  '((t :inherit variable-pitch))
  "Face for the header line path.")

(which-func-mode)

(defun mode-line-render (left right)
  (let* ((available-width (- (window-width) (length left) )))
    (format "%s%s" left right)))

(add-hook 'after-init-hook
          (lambda ()
            (setq-default
             header-line-format
             `((:eval (mode-line-render
                       (format-mode-line
                        (list
                         (propertize "⊷" 'face '(:weight bold :height 2.0))
                         " "
                         (propertize (me/project-to-buffer-name) 'face 'header-line-path)))
                       (format-mode-line
                        (list
                         ""
                         mode-line-process
                         ;; (propertize "⊷ " 'face '(:weight bold :height 2.0))
                         (propertize (me/echo-which-func) 'face 'which-func)
                         ;; (propertize " ⊷" 'face '(:weight bold :height 2.0))
                         ))))))))

(setq line-spacing 0.1)

(defun me/echo-which-func ()
  "Which function string for display."
  (if-let ((fn (which-function)))
      (concat "   λ " fn)
    ""))

(defun me/project-to-buffer-name ()
  (when-let ((filename (or buffer-file-truename default-directory)))
    (let* ((name filename)
           (project (cdr-safe (project-current)))
           (name (file-relative-name name project)))
      (combine-and-quote-strings
       (split-string name "/+")
       " ⧸ "))
    (if-let (prompt (minibuffer-prompt))
        prompt
      (buffer-name))))

(setq-default mode-line-buffer-identification '(:eval (me/project-to-buffer-name)))

(provide 'header-line-config)
