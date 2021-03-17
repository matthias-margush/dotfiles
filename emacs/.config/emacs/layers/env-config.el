;; -*- mode: emacs-lisp; lexical-binding: t; -*-

(require 'package-config)

(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))

(setenv "PATH" (concat (getenv "PATH") ":" (expand-file-name "~/bin")))
(setq exec-path (append exec-path `(,(expand-file-name "~/bin"))))

(setenv "PATH" (concat (getenv "PATH") ":" (expand-file-name "~/go/bin")))
(setq exec-path (append exec-path `(,(expand-file-name "~/go/bin"))))

(use-package envrc
  :config (envrc-global-mode))

(provide 'env-config)
