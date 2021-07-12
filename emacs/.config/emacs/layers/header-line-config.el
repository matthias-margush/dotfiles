;; -*- mode: emacs-lisp; lexical-binding: t; -*-

(defface header-line-path
  '((t (:inherit header-line :weight normal)))
  "Face for the header line path.")

(defface header-line-separator
  '((t (:inherit header-line :slant italic)))
  "Face for the header line separator.")

(which-func-mode)

;; (defun mode-line-render (left right)
;;   (let* ((available-width (- (window-width) (length left) )))
;;     (format "%s%s" left right)))

(defun mode-line-render (left right)
  "Return a string of `window-width' length.
Containing LEFT, and RIGHT aligned respectively."
  (let ((available-width
         (/ (- (window-total-width)
               (+ (length (format-mode-line left))
                  (length (format-mode-line right))))
            1)))
    ;; (message "available-width: %s" available-width)
    (append left
            (list (format (format "%%%ds" available-width) ""))
            right)))

(add-hook 'after-init-hook
          (lambda ()
            (setq-default
             header-line-format
             `((:eval (mode-line-render
                       (list
                        "☰ "
                        mode-line-process
                        (me/project-to-buffer-name)
                        (me/echo-which-func)
                        ;; (propertize "⊷ " 'face '(:weight bold :height 2.0))
                        ;; (propertize " ⊷" 'face '(:weight bold :height 2.0))
                        )
                       (list
                        ;; (propertize "◉" 'face '(:weight bold :height 1.0))
                        ""
                        ;; (propertize (me/project-to-buffer-name) 'face '(:face header-line-path :weight normal))
                        (me/narrowing-status))))))))

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
        (concat (propertize (concat " λ " fn) 'face 'which-func)
                ;; (propertize " in " 'face 'header-line-separator)
                )
      "")
    ))

(setq x-underline-at-descent-line t)

(defun me/project-to-buffer-name ()
  (when-let ((filename (or buffer-file-truename default-directory)))
    (let* ((name filename)
           (project (or (cdr-safe (project-current)) (file-name-directory name)))
           (path (file-relative-name name project))
           (display-name path)
           (parts (split-string (concat project path) "/+"))
           (name (car (last parts)))
           (first-parts (butlast parts (- (length parts) 3)))
           (last-parts (nthcdr (- (length parts) 3) parts))
           (all-parts (butlast parts)))
      (propertize name 'face 'header-line-path)
      ;; (if (not (string= name ""))
      ;;     (if (= (length parts) (+ (length first-parts) (length last-parts)))
      ;;         (concat ""
      ;;                 (propertize name 'face 'header-line-path)
      ;;                 (propertize " in " 'face 'header-line-separator)
      ;;                 (propertize (combine-and-quote-strings all-parts "/") 'face 'header-line-path)
      ;;                 "/")
      ;;       (concat ""
      ;;               (propertize name 'face 'header-line-path)
      ;;               (propertize " in " 'face 'header-line-separator)
      ;;               (propertize (combine-and-quote-strings first-parts "/") 'face 'header-line-path)
      ;;               "/.../"
      ;;               (propertize (combine-and-quote-strings last-parts "/") 'face 'header-line-path)
      ;;               "/"))
      ;;   "")
      ;; (if-let (prompt (minibuffer-prompt)) ; doesn't work, loses minibuffer focus
      ;;     prompt
      ;;   display-name)
      )))

(setq-default mode-line-buffer-identification '(:eval (me/project-to-buffer-name)))

(provide 'header-line-config)
