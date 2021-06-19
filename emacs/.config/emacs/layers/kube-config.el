;; -*- lexical-binding: t; -*-

(require 'evil-config)

(use-package kubernetes
  :commands (kubernetes-overview)
  ;; :hook (kubernetes-overview-mode . me/enter-kubernetes-mode)

  :general
  (:states 'normal :prefix leader "k" #'me/kubernetes)
  (:keymaps 'kubernetes-overview-mode-map "s-:" #'execute-extended-command)

  :init
  ;; (defun me/enter-kubernetes-mode ()
  ;;   ;; (setq me/original-frame-title-format frame-title-format
  ;;   ;;       frame-title-format '("kubernetes"))
  ;;   )
  
  ;; (defun me/leave-kubernetes-mode ()
  ;;   (when (eq major-mode 'kubernetes-overview-mode)
  ;;     (setq frame-title-format me/original-frame-title-format)))

  ;; (add-hook 'change-major-mode-hook #'me/leave-kubernetes-mode)

  (defun me/kubernetes ()
    (interactive)
    (kubernetes-overview)))

(use-package kubernetes-evil
  :defer t
  :init
  (add-hook 'kubernetes-overview-mode-hook (lambda () (require 'kubernetes-evil))))

(provide 'kube-config)
