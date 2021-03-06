; -*- mode: emacs-lisp; lexical-binding: t -*-

(tab-bar-history-mode)

(defface spacebar-active
  '((t :inherit variable-pitch))
  "Face for active spacebar tab.")

(defface spacebar-inactive
  '((t :inherit variable-pitch))
  "Face for inactive spacebar tabs.")

(setq-default cursor-in-non-selected-windows nil)

(defun me/tab-echo (&optional _)
  "Echo the tabs."
  (interactive "P")
  (unless (or cursor-in-echo-area (active-minibuffer-window))
    (let ((msg (propertize "| " 'face 'spacebar-inactive))
	  (name (format "*me/tabbar - %s*" (selected-frame))))
      (dolist (tab (funcall tab-bar-tabs-function))
	(let* ((details (assq 'name tab))
	       (which (car tab))
	       (name (cdr details)))
	  (if (eq which 'current-tab)
	      (setq msg (concat
			 msg
			 (propertize name 'face 'spacebar-active)
			 (propertize " | " 'face 'spacebar-inactive)))
	    (setq msg
		  (concat
		   msg
		   (propertize name 'face 'spacebar-inactive)
		   (propertize " | " 'face 'spacebar-inactive))))))
      (format msg))))

(defvar tab-current-msg "")

(defadvice eldoc--message
    (around eldoc--message-default-tabs)
  "Display tab line above the message."
  (unwind-protect
      (let ((msg (ad-get-arg 0)))
	(if (string= nil msg)
	    (ad-set-arg 0 (me/tab-echo)))))
  ad-do-it)

(ad-activate 'eldoc--message)
;; (ad-deactivate 'eldoc--message)

(defalias 'message-plain (symbol-function 'message))

(defun me/tabs-message-with-tabs (&optional msg)
  (concat (propertize "   " 'face 'spacebar-active) msg))

;; transform inputs to (message):
(setq set-message-function #'me/tabs-message-with-tabs)

(defun me/tabs-refresh ()
  (interactive)
  (unless (current-message)
    (message (me/tab-echo))))

(add-hook 'after-init-hook
	  (lambda ()
	    (me/tabs-refresh)
	    (add-hook 'post-command-hook #'me/tabs-refresh)))

(defun me/tab-new (_)
  "Create a new tab"
  (interactive "P")
  (tab-bar-new-tab)
  (me/tabs-refresh))

(defun me/tab-next (_)
  "Switch to the next tab."
  (interactive "P")
  (tab-bar-switch-to-next-tab)
  (me/tabs-refresh))

(defun me/tab-prev (_)
  "Switch to the previous tab."
  (interactive "P")
  (tab-bar-switch-to-prev-tab)
  (me/tabs-refresh))

(defun me/tab-close (_)
  "Switch to the previous tab."
  (interactive "P")
  (tab-bar-close-tab)
  (me/tabs-refresh))

(defun me/tab-first (_)
  "Switch to the previous tab."
  (interactive "P")
  (tab-bar-select-tab 1)
  (me/tabs-refresh))

(defun me/tab-last ()
  (interactive "P")
  (tab-bar-select-tab (length (funcall tab-bar-tabs-function)))
  (me/tabs-refresh))

(evil-define-key 'normal 'global (kbd "gt") #'me/tab-next)
(evil-define-key 'normal 'global (kbd "gT") #'me/tab-prev)
(evil-define-key 'normal 'global (kbd "g SPC") #'me/tabs-refresh)
(define-key evil-window-map (kbd "C-t") #'me/tab-new)
(define-key evil-window-map (kbd "C-q") #'me/tab-close)

;; (define-key evil-motion-state-map (kbd "g`") #'eyebrowse-last-window-config)
(evil-ex-define-cmd "tabn[ext]" #'me/tab-next)
(evil-ex-define-cmd "tabp[revious]" #'me/tab-prev)
(evil-ex-define-cmd "tabN[ext]" #'me/tab-prev)
(evil-ex-define-cmd "tabr[ewind]" #'me/tab-first)
(evil-ex-define-cmd "tabf[irst]" #'me/tab-first)
(evil-ex-define-cmd "tabl[ast]" #'me/tab-last)
(evil-ex-define-cmd "tabnew" #'me/tab-new)
(evil-ex-define-cmd "tabe[dit]" #'me/tab-new)
(evil-ex-define-cmd "tabc[lose]" #'me/tab-close)
