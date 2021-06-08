;; -*- mode: emacs-lisp; lexical-binding: t; -*-

(require 'package-config)

(use-package cider
  :defer t
  :init
  (setq cider-clojure-cli-global-options "-A:dev"))

(provide 'clojure-config)
