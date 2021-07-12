(require 'package-config)

(use-package org-jira
  :general
  (:states 'normal :prefix leader "J" me/org-jira-map)

  :commands
  (org-jira-get-projects
   org-jira-browse-issue
   org-jira-get-issues
   org-jira-get-issues-from-custom-jql
   org-jira-get-issues-headonly
   org-jira-update-issue
   org-jira-progress-issue
   org-jira-progress-issue-next
   org-jira-assign-issue
   org-jira-refresh-issue
   org-jira-refresh-issues-in-buffer
   org-jira-create-issue
   org-jira-copy-current-issue-key
   org-jira-create-subtask
   org-jira-get-subtasks
   org-jira-add-comment
   org-jira-update-comment
   org-jira-update-worklogs-from-org-clocks
   org-jira-todo-to-jira
   org-jira-get-issues-by-fixversion)

  ;; :init
  ;; (make-directory "~/.org-jira")
  ;; (setq jiralib-url "https://PROJECT.atlassian.net")
  :init
  (setq me/org-jira-map (make-sparse-keymap))
  (define-key me/org-jira-map (kbd "pg") 'org-jira-get-projects)
  (define-key me/org-jira-map (kbd "ib") 'org-jira-browse-issue)
  (define-key me/org-jira-map (kbd "ig") 'org-jira-get-issues)
  (define-key me/org-jira-map (kbd "ij") 'org-jira-get-issues-from-custom-jql)
  (define-key me/org-jira-map (kbd "ih") 'org-jira-get-issues-headonly)
  (define-key me/org-jira-map (kbd "iu") 'org-jira-update-issue)
  (define-key me/org-jira-map (kbd "iw") 'org-jira-progress-issue)
  (define-key me/org-jira-map (kbd "in") 'org-jira-progress-issue-next)
  (define-key me/org-jira-map (kbd "ia") 'org-jira-assign-issue)
  (define-key me/org-jira-map (kbd "ir") 'org-jira-refresh-issue)
  (define-key me/org-jira-map (kbd "iR") 'org-jira-refresh-issues-in-buffer)
  (define-key me/org-jira-map (kbd "ic") 'org-jira-create-issue)
  (define-key me/org-jira-map (kbd "ik") 'org-jira-copy-current-issue-key)
  (define-key me/org-jira-map (kbd "sc") 'org-jira-create-subtask)
  (define-key me/org-jira-map (kbd "sg") 'org-jira-get-subtasks)
  (define-key me/org-jira-map (kbd "cc") 'org-jira-add-comment)
  (define-key me/org-jira-map (kbd "cu") 'org-jira-update-comment)
  (define-key me/org-jira-map (kbd "wu") 'org-jira-update-worklogs-from-org-clocks)
  (define-key me/org-jira-map (kbd "tj") 'org-jira-todo-to-jira)
  (define-key me/org-jira-map (kbd "if") 'org-jira-get-issues-by-fixversion)
  )

(provide 'jira-config)
