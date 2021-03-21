;; -*- lexical-binding: t; -*-

(use-package kubernetes
  :commands (kubernetes-overview))

(use-package kubernetes-evil
  :defer t
  :init
  (add-hook 'kubernetes-overview-mode-hook (lambda () (require 'kubernetes-evil))))

(provide 'kube-config)
