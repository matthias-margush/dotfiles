;; -*- mode: emacs-lisp; lexical-binding: t; -*-

;; (use-package tree-sitter
;;              :config
;;              (global-tree-sitter-mode))

;; (use-package tree-sitter-langs)

(mm/package 'lsp-mode)
(add-hook 'go-mode-hook #'lsp)

(setq lsp-headerline-breadcrumb-enable nil
      lsp-completion-show-detail t
      lsp-completion-show-kind t
      lsp-file-watch-threshold 3000)
(add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\vendor\\'" )

;; markdown
(mm/package 'markdown-mode)
(add-hook 'markdown-mode-hook #'visual-line-mode)

(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . gfm-mode))

(setq markdown-header-scaling t
      markdown-fontify-code-blocks-natively t
      markdown-hide-markup t
      markdown-hide-urls t
      markdown-header-scaling t
      markdown-display-remote-images t
      markdown-make-gfm-checkboxes-buttons t
      markdown-asymmetric-header t)

(add-to-list 'markdown-code-lang-modes '("yaml" . yaml-mode))
(add-to-list 'markdown-gfm-additional-languages '("yaml" . yaml-mode))

;; go
(mm/package 'go-mode)
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))
(add-hook 'go-mode-hook #'me/go-editor-settings)

(defun me/go-editor-settings ()
  ;; (setq flycheck-checker 'gometalinter)
  (add-hook 'before-save-hook #'lsp-organize-imports t t)
  (add-hook 'before-save-hook #'lsp-format-buffer -10 t)
  (electric-pair-mode))

(eval-after-load "go-mode" '(setq go-mode-map (make-sparse-keymap)))

(mm/package 'gotest
  ;; :general
  ;; (:states '(normal) :keymaps 'go-mode-map :prefix local-leader "tf" #'go-test-current-file)
  ;; (:states '(normal) :keymaps 'go-mode-map :prefix local-leader "tt" #'go-test-current-test)
  ;; (:states '(normal) :keymaps 'go-mode-map :prefix local-leader "tl" #'go-test-current-test-cache)
  ;; (:states '(normal) :keymaps 'go-mode-map :prefix local-leader "tp" #'go-test-current-project)
  ;; (:states '(normal) :keymaps 'go-mode-map :prefix local-leader "tr" #'go-run)
  )

;; yaml
(mm/package 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(add-hook 'yaml-mode #'hs-minor-mode)

;; asciidoc
(mm/package 'adoc-mode)
(add-to-list 'auto-mode-alist '("\\.adoc\\'" . adoc-mode))

;; vimrc
(mm/package 'vimrc-mode)
(add-to-list 'auto-mode-alist '("\\.vim\\'" . vimrc-mode))

;; sql
;; (use-package sql-indent)
(mm/package 'sqlformat)
(add-hook 'sql-mode-hook #'sqlformat-on-save-mode)
(setq sqlformat-command 'pgformatter)

;; gherkin / features
(mm/package 'feature-mode)

;; dockerfiles
(mm/package 'dockerfile-mode)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

;; fish
(mm/package 'fish-mode)
(add-to-list 'auto-mode-alist '("\\.fish\\'" . fish-mode))

(provide 'configure-languages)
