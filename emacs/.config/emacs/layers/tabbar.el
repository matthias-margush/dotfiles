; -*- mode: emacs-lisp; lexical-binding: t -*-

(use-package posframe)

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
  (let ((msg (propertize "| " 'face 'spacebar-inactive)))
    (dolist (tab (funcall tab-bar-tabs-function))
      (let* ((details (assq 'name tab))
	     (which (car tab))
	     (name (cdr details)))
	;(message "%s" tab)
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
    (posframe-show
     (format " *me/tabbar - %s *" (selected-frame))
     :string msg
     :position `(,(frame-fringe-width) . 1)
     :posframe-width (frame-width))))

;(add-hook 'after-init-hook
;	  (lambda ()
;	    (run-with-idle-timer 1 t #'me/tab-echo)))

;; (defun me/tabs-new-frame (frame)
;;   (with-selected-frame frame (me/tab-echo)))

;; (add-hook 'after-make-frame-functions #'me/tabs-new-frame)

;; (add-hook 'after-focus-change-function #'me/tab-echo)

;; (add-hook 'after-init-hook
;; 	  (lambda ()
;; 	    (add-to-list 'after-make-frame-functions #'me/tab-echo t)))



(defun me/tab-new (_)
  "Create a new tab"
  (interactive "P")
  (tab-bar-new-tab)
  (me/tab-echo))

(defun me/tab-next (_)
  "Switch to the next tab."
  (interactive "P")
  (tab-bar-switch-to-next-tab)
  (me/tab-echo))

(defun me/tab-prev (_)
  "Switch to the previous tab."
  (interactive "P")
  (tab-bar-switch-to-prev-tab)
  (me/tab-echo))

(defun me/tab-close (_)
  "Switch to the previous tab."
  (interactive "P")
  (tab-bar-close-tab)
  (me/tab-echo))

(evil-define-key 'normal 'global (kbd "gt") #'me/tab-next)
(evil-define-key 'normal 'global (kbd "gT") #'me/tab-prev)
(evil-define-key 'normal 'global (kbd "g SPC") #'me/tab-echo)
(define-key evil-window-map (kbd "C-t") #'me/tab-new)
(define-key evil-window-map (kbd "C-q") #'me/tab-close)

;; (define-key evil-motion-state-map (kbd "g`") #'eyebrowse-last-window-config)
(evil-ex-define-cmd "tabn[ext]" #'me/tab-next)
(evil-ex-define-cmd "tabp[revious]" #'tab-prev)
(evil-ex-define-cmd "tabN[ext]" #'me/tab-prev)
(evil-ex-define-cmd "tabr[ewind]" (lambda () (interactive) (tab-bar-select-tab 1)))
(evil-ex-define-cmd "tabf[irst]" (lambda () (interactive) (tab-bar-select-tab 1)))
(evil-ex-define-cmd "tabl[ast]" (lambda () (interactive) (tab-bar-select-tab (length (funcall tab-bar-tabs-function)))))
(evil-ex-define-cmd "tabnew" #'me/tab-new)
(evil-ex-define-cmd "tabe[dit]" #'me/tab-new)
(evil-ex-define-cmd "tabc[lose]" #'me/tab-close)
