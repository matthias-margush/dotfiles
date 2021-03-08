;; -*- mode: emacs-lisp; lexical-binding: t; -*-

(require 'package-config)

(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))

(use-package envrc
  :config (envrc-global-mode))

;; (use-package exec-path-from-shell
;;   :demand t
;;   :config (exec-path-from-shell-initialize))

(provide 'env-config)
