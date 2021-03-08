;; -*- mode: emacs-lisp; lexical-binding: t; -*-

(require 'evil-config)

(general-define-key
 :states '(normal visual)
 :prefix leader
 "n" narrow-map)

(provide 'keys-config)
