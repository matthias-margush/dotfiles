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

(provide 'dashboard-config)
