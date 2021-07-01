;; -*- mode: emacs-lisp; lexical-binding: t; -*-

(defface header-line-path
  '((t :inherit header-line :height 1.0))
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
                         ;; (propertize "◉" 'face '(:weight bold :height 1.0))
                         "  "
                         ;; (propertize (me/project-to-buffer-name) 'face '(:face header-line-path :weight normal))
                         (me/project-to-buffer-name)))
                       (format-mode-line
                        (list
                         ""
                         mode-line-process
                         ;; (propertize "⊷ " 'face '(:weight bold :height 2.0))
                         (propertize (me/echo-which-func) 'face 'which-func)
                         (propertize (me/narrowing-status) 'face 'which-func)
                         ;; (propertize " ⊷" 'face '(:weight bold :height 2.0))
                         ))))))))

;; (setq line-spacing 0.1)

(defun me/narrowing-status ()
  "Narrowing status text for header line."
  (if (buffer-narrowed-p)
    " [narrowed]"
    ""))

(defun me/echo-which-func ()
  "Which function string for display."
  (let ((fn (which-function)))
    (if (and fn (not (minibuffer-prompt)))
        (concat "   λ " fn)
      "")))

(defun me/project-to-buffer-name ()
  (when-let ((filename (or buffer-file-truename default-directory)))
    (let* ((name filename)
           (project (cdr-safe (project-current)))
           (path (file-relative-name name project))
           (display-name path)
           (parts (split-string path "/+"))
           (name (car (last parts)))
           (parts (butlast parts))
           (display-name (concat "  " (combine-and-quote-strings parts " ⊸ ") " ⊸ " name)))
      (if-let (prompt (minibuffer-prompt))
          prompt
        display-name))))

(setq-default mode-line-buffer-identification '(:eval (me/project-to-buffer-name)))

(provide 'header-line-config)
