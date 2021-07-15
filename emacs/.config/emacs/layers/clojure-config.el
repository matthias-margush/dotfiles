;; -*- mode: emacs-lisp; lexical-binding: t; -*-

(require 'package-config)
(require 'languages-config)

(use-package flymake-joker)

(use-package clj-refactor
  :config
  (cljr-add-keybindings-with-prefix "C-c r")
  )

(use-package clojure-mode
  :hook ((clojure-mode . flymake-joker-clj-enable)
         (clojurescript-mode . flymake-joker-cljs-enable)
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
           ",en" #'cider-ns-reload
           ",eN" #'cider-ns-reload-all
           ",ep" #'cider-load-all-project-ns
           ",ef" #'cider-eval-defun-at-point
           ",fb" #'cider-format-buffer)

  :init
  (setq clojure-align-forms-automatically nil)

  :config
  ;; (dolist (c (string-to-list ":_-?!#*"))
  ;;   (modify-syntax-entry c "w" clojure-mode-syntax-table)
  ;;   (modify-syntax-entry c "w" emacs-lisp-mode-syntax-table))

  (evil-add-command-properties #'cider-find-var :jump t)
  (add-to-list 'clojure-align-binding-forms "let")

  (defadvice cider-clojuredocs
      (after me/cider-clojuredocs-after activate)
    (adoc-mode)
    (read-only-mode)))

(use-package cider
  :hook
  ((cider--debug-mode . evil-normalize-keymaps)
   (cider-mode . evil-normalize-keymaps))

  :general
  (:states 'normal
           :keymaps '(cider-mode-map)
           ",td" #'cider-debug-defun-at-point)
  :commands (cider-jack-in
             cider-jack-in-clj
             cider-jack-in-cljs
             cider-connect
             cider-connect-clj
             cider-connect-cljs)

  :custom
  (cider-debug-prompt-commands '((?c "continue" "continue")
                                 (?C "continue-all" nil)
                                 (?n "next" "next")
                                 (?i "in" "in")
                                 (?o "out" "out")
                                 (?O "force-out" nil)
                                 (?H "here" "here")
                                 (?e "eval" "eval")
                                 (?p "inspect" "inspect")
                                 (?P "inspect-prompt" nil)
                                 (?L "locals" "locals")
                                 (?J "inject" "inject")
                                 (?s "stacktrace" "stacktrace")
                                 (?t "trace" "trace")
                                 (?q "quit" "quit")))
  :config
  (evil-define-minor-mode-key 'normal 'cider--debug-mode
    "b" 'cider-debug-defun-at-point
    "c" 'cider-debug-mode-send-reply
    "C" 'cider-debug-mode-send-reply
    "n" 'cider-debug-mode-send-reply
    "i" 'cider-debug-mode-send-reply
    "o" 'cider-debug-mode-send-reply
    "O" 'cider-debug-mode-send-reply
    "H" 'cider-debug-move-here
    "e" 'cider-debug-mode-send-reply
    "p" 'cider-debug-mode-send-reply
    "P" 'cider-debug-mode-send-reply
    "L" 'cider-debug-mode-send-reply
    "J" 'cider-debug-mode-send-reply
    "s" 'cider-debug-mode-send-reply
    "t" 'cider-debug-mode-send-reply
    "q" 'cider-debug-mode-send-reply)

  (setq cider-clojure-cli-global-options "-A:dev"
        cider-debug-prompt 'overlay
        cider-debug-use-overlays t
        cider-prompt-for-symbol nil
        cider-repl-display-help-banner nil
        cider-repl-pop-to-buffer-on-connect nil
        cider-save-file-on-load t
        cider-test-default-exclude-selectors '("integration")))



(provide 'clojure-config)
