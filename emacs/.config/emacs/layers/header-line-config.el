;; -*- mode: emacs-lisp; lexical-binding: t; -*-

(defface header-line-path
  '((t :inherit variable-pitch))
  "Face for the header line path.")

(which-func-mode)

(setq-default
 header-line-format
 '((:propertize "⧉" face bold)
   " "
   (:propertize mode-line-buffer-identification face header-line-path)
   mode-line-process
   (:propertize (:eval (me/echo-which-func)) face which-func)))

(defun me/echo-which-func ()
  "Which function string for display."
  (when-let ((fn (which-function)))
    (concat "   λ " fn)))

(defun me/project-to-buffer-name ()
  (if buffer-file-truename
      (let* ((name buffer-file-truename)
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
