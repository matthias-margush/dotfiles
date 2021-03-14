;; -*- mode: emacs-lisp; lexical-binding: t; -*-

(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))

(mm/package 'envrc)
(envrc-global-mode)

(provide 'configure-env)
