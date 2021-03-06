(use-package helpful
  :bind
  (:map help-map
        ("f" . helpful-callable)
        ("v" . helpful-variable)
        ("k" . helpful-key)
        ("C" . helpful-command)))

(use-package which-key
  :init
  (setq which-key-show-early-on-C-h t
        which-key-idle-delay 3.0
        which-key-allow-evil-operators t)

  :config
  (which-key-mode))
