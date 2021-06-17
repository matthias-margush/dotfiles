;; -*- mode: emacs-lisp; lexical-binding: t; -*-

(require 'package-config)
(require 'languages-config)

(defadvice cider-clojuredocs
    (after me/cider-clojuredocs-after activate)
  (adoc-mode)
  (read-only-mode))

(use-package clojure-mode
  :mode
  ("\\.\\(clj\\|dtm\\|edn\\)\\'" . clojure-mode)
  ("\\.cljc\\'" . clojurec-mode)
  ("\\.cljs\\'" . clojurescript-mode)
  ("\\(?:build\\|profile\\)\\.boot\\'" . clojure-mode)

  :general
  (:states 'normal
           :keymaps '(cider-mode-map)
           ",xd" #'cider-xref-fn-deps-select
           ",xD" #'cider-xref-fn-deps
           ",xu" #'cider-xref-fn-refs-select
           ",xU" #'cider-xref-fn-refs
           ",ee" #'cider-eval-last-sexp
           ",eb" #'cider-load-buffer
           ",eB" #'cider-load-all-project-ns
           ",ef" #'cider-eval-defun-at-point)
  :config
  ;; (general-define-key
  ;;  :keymaps '(cider-mode-map)
  ;;  :states '(normal visual)
  ;;  "K" #'cider-clojuredocs)

  ;; (general-define-key
  ;;  :states '(normal visual)
  ;;  :prefix local-leader
  ;;  "," #'narrow-or-widen-dwim)
  )

(use-package cider
  :init
  (setq cider-clojure-cli-global-options "-A:dev"
        cider-prompt-for-symbol nil
        cider-save-file-on-load t
        cider-repl-display-help-banner nil
        cider-repl-pop-to-buffer-on-connect nil))

(provide 'clojure-config)
