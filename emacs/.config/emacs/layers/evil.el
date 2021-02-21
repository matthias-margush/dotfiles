;; -*- lexical-binding: t; -*-

;(setq leader-map (make-sparse-keymap))
;(setq me/intercept-mode-map (make-sparse-keymap))
;(define-minor-mode me/intercept-mode "doc" :global t)
;(me/intercept-mode)

;  :straight ((sacred-theme :type git :host github :repo "matthias-margush/sacred-theme" :branch "new-branch"))
(use-package evil
 :demand t
; :straight ((evil :type git :host github :repo "emacs-evil/evil" :branch "master"))
  :init
  (setq evil-want-keybinding nil)

  :config
  (evil-mode))


  ;(unbind-key (kbd " ") evil-normal-state-map)

  ;(dolist (state '(normal visual insert))
  ;  (evil-make-intercept-map
  ;   (evil-get-auxiliary-keymap me/intercept-mode-map state t t)
  ;   state))

  ;(evil-define-key 'normal me/intercept-mode-map (kbd "SPC") leader-map)
  ;(define-key leader-map (kbd "w") evil-window-map)
  ;(define-key leader-map (kbd "h") help-map)
  ;(define-key leader-map (kbd "SPC") #'ido-switch-buffer)
  ;(define-key leader-map (kbd "j") #'imenu)

;(define-key evil-motion-state-map (kbd "gt") #'tab-bar-switch-to-next-tab)
;(define-key evil-motion-state-map (kbd "gT") #'tab-bar-switch-to-prev-tab)
;; (define-key evil-motion-state-map (kbd "g`") #'tab-line)

;  (evil-mode -1)
;  (evil-mode))

;; (use-package annalist)
(use-package evil-collection
  :demand t
  :after evil
; :hook (evil-collection-setup . me/prefix-translations)
  :init
;  (defun me/prefix-translations (_mode mode-keymaps &rest _rest)
;    (evil-collection-translate-key
;      'normal mode-keymaps
;      "gt" nil
;      "gT" nil))
  (setq evil-collection-setup-minibuffer t
        evil-collection-company-use-tng nil
        evil-collection-term-sync-state-and-mode-p t
        evil-collection-setup-debugger-keys t
        ;evil-collection-key-blacklist '("SPC")
	)
  :config
  (evil-collection-init))

(use-package evil-commentary
  :config
  (evil-commentary-mode))
