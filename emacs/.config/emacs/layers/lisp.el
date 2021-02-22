(use-package lispy
  :hook
  ((emacs-lisp-mode . lispy-mode)
   (lisp-mode . lispy-mode))

  :general
  (:states 'normal
           :keymaps 'emacs-lisp-mode-map
           ",eb" #'eval-buffer
           ",ee" #'eval-last-sexp
           ",ef" #'eval-defun)
  (:states 'visual
           :keymaps 'emacs-lisp-mode-map
           ",er" #'eval-region)
  :commands lispy-mode

  :init
  (setq me/lisp-map (make-sparse-keymap))
  (setq lispy-move-after-commenting nil
        lispy-safe-actions-ignore-comments t
        lispy-safe-actions-ignore-strings t
        lispy-compat '(edebug cider)
        lispy-close-quotes-at-end-p t
        lispy-safe-copy t
        lispy-safe-delete t
        lispy-safe-paste t
        lisy-safe-actions-actions-no-pull-delimiters-into-comments t)
  :config
  (lisp-mode)
  (lispy-set-key-theme '(parinfer)))


(use-package lispyville
    :hook
  ((emacs-lisp-mode . lispyville-mode)
   (lisp-mode . lispyville-mode) )

  :init
  (setq
   lispyville-insert-states nil
   lispyville-key-theme '(operators
                          c-w
                          prettify
                          text-objects
                          additional-motions
                          slurp/barf-cp
                          ;; wrap
                          additional
                          additional-insert
                          escape)))
