;; -*- mode: emacs-lisp; lexical-binding: t; -*-

(require 'package-config)
(require 'languages-config)

(defadvice cider-clojuredocs
    (after me/cider-clojuredocs-after activate)
  (adoc-mode)
  (read-only-mode))

(use-package flymake-joker)

(use-package clj-refactor
  :config
  (cljr-add-keybindings-with-prefix "C-c r")
  )

(use-package clojure-mode
  :hook ((clojure-mode . flymake-joker-clj-enable)
         (clojurescript-mode . flymake-joker-cljs-enable)
         (clojure-mode . flymake-mode)
         (clojure-mode . clj-refactor-mode))

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

  :init
  (setq clojure-align-forms-automatically nil)

  :config
  (evil-add-command-properties #'cider-find-var :jump t)
  (add-to-list 'clojure-align-binding-forms "let")

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
  :commands (cider-jack-in
             cider-jack-in-clj
             cider-jack-in-cljs
             cider-connect
             cider-connect-clj
             cider-connect-cljs)
  ;; :generaelect
  ;; (:states 'normal :keymaps '(cider--debug-mode-map)
  ;;          "i" #'cider-debug-move-here)
  :init
  (setq cider-clojure-cli-global-options "-A:dev"
        cider-prompt-for-symbol nil
        cider-save-file-on-load t
        cider-repl-display-help-banner nil
        cider-repl-pop-to-buffer-on-connect nil))


(provide 'clojure-config)
