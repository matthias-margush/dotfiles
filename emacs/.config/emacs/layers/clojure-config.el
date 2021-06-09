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
           )
  :init
  (setq cider-clojure-cli-global-options "-A:dev"))

(provide 'clojure-config)
