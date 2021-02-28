;; -*- lexical-binding: t; -*-

;;;  which-func mode
(which-func-mode)
(setq me/which-map (make-sparse-keymap))

(general-define-key
  :states 'normal
  :prefix leader
  "w" me/which-map)

(use-package eglot
  :hook ((go-mode . eglot-ensure))

  :general
  (:states '(normal) :prefix leader "r" #'eglot-rename)
  (:states '(normal) :prefix leader "f" #'eglot-format)
  (:states '(normal) :prefix leader "o" #'eglot-code-action-organize-imports)
  (:states '(normal) :prefix leader "q" #'eglot-code-action-quickfix)
  (:states '(normal) :prefix local-leader "d" #'flymake-show-diagnostics-buffer)
  (:keymaps 'normal
    ;; ",e" #'flymake-show-diagnostics-buffer
    "]e" #'flymake-goto-next-error
    "[e" #'flymake-goto-prev-error)

  :init
  (setq eldoc-echo-area-use-multiline-p 1))

(use-package markdown-mode
  :hook ((markdown-mode . visual-line-mode))
  :mode (("README\\.md\\'" . gfm-mode)
          ("\\.md\\'" . gfm-mode)
          ("\\.markdown\\'" . gfm-mode))
  :general
  (:states '(normal) :keymaps 'markdown-mode-map "s-j" #'markdown-next-link)
  (:states '(normal) :keymaps 'markdown-mode-map "s-k" #'markdown-previous-link)
  (:states '(normal) :keymaps 'markdown-mode-map "s-l" #'markdown-follow-thing-at-point)

  :init
  (setq markdown-header-scaling t
    markdown-fontify-code-blocks-natively t
    markdown-hide-markup t
    markdown-hide-urls t
    markdown-header-scaling t
    markdown-display-remote-images t
    markdown-make-gfm-checkboxes-buttons t
    markdown-asymmetric-header t)
  :config
  (add-to-list 'markdown-code-lang-modes '("yaml" . yaml-mode))
  (add-to-list 'markdown-gfm-additional-languages '("yaml" . yaml-mode)))

(use-package go-mode
  :commands go-mode
  :mode (("\\.go\\'" . go-mode))
  :hook ((go-mode . me/go-editor-settings))
  :init
  (defun me/go-editor-settings ()
    ;; (setq flycheck-checker 'gometalinter)
    (electric-pair-mode))

  (eval-after-load "go-mode" '(setq go-mode-map (make-sparse-keymap))))

(use-package gotest
  :general
  (:states '(normal) :keymaps 'go-mode-map :prefix local-leader "tf" #'go-test-current-file)
  (:states '(normal) :keymaps 'go-mode-map :prefix local-leader "tt" #'go-test-current-test)
  (:states '(normal) :keymaps 'go-mode-map :prefix local-leader "tl" #'go-test-current-test-cache)
  (:states '(normal) :keymaps 'go-mode-map :prefix local-leader "tp" #'go-test-current-project)
  (:states '(normal) :keymaps 'go-mode-map :prefix local-leader "tr" #'go-run))

(use-package yaml-mode
  :commands yaml-mode
  :mode (("\\.yaml\\'" . yaml-mode)
          ("\\.yml\\'" . yaml-mode))
  :hook ((yaml-mode . hs-minor-mode)))

(use-package adoc-mode
  :mode (("\\.adoc\\'" . adoc-mode)))

(use-package vimrc-mode
  :ensure t
  :mode ("\\.vim\\'"))

;; (use-package sql-indent)
(use-package sqlformat
  ;; :hook (sql-mode . sqlformat-on-save-mode)
  :init
  (setq sqlformat-command 'pgformatter))

(use-package feature-mode)
