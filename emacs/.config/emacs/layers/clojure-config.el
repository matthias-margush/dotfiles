;; -*- mode: emacs-lisp; lexical-binding: t; -*-

(require 'package-config)

(use-package cider
  :defer t
  :general
  (:states 'normal
           :keymaps '(clojure-mode)
           ",xd" #'cider-xref-fn-deps-select
           ",xD" #'cider-xref-fn-deps
           ",xu" #'cider-xref-fn-refs-select
           ",xU" #'cider-xref-fn-refs
           ",ee" #'cider-eval-last-sexp
           ",eb" #'cider-load-all-project-ns
           ",ef" #'cider-eval-defun-at-point)
  :init
  (setq cider-clojure-cli-global-options "-A:dev"
        cider-repl-display-help-banner nil
        cider-repl-pop-to-buffer-on-connect nil))

(provide 'clojure-config)
