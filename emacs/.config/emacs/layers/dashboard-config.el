;; -*- mode: emacs-lisp; lexical-binding: t; -*-

(require 'package-config)
(require 'org-config)

(defun me/dashboard ()
  "Startup file"
  (let ((projects (expand-file-name "~/project.org")))
    (if projects
        (progn
          (find-file projects))
        "*scratch*")))

(setq initial-buffer-choice #'me/dashboard)

;; (me/dashboard)

;; (add-hook 'after-init-hook #'me/dashboard)

(provide 'dashboard-config)
