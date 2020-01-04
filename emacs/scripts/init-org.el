(use-package org
  :init
  (setq org-log-done t)
  (setq org-agenda-files '("~/org"))
  :bind ("\C-ca" . org-agenda)
  :config
  (add-hook 'after-init-hook 'org-todo-list)
  ;; Try to minimize org sync conflicts (https://christiantietze.de/posts/2019/03/sync-emacs-org-files/)
  (add-hook 'auto-save-hook 'org-save-all-org-buffers) ;; enable autosaves
  )

(defun org-toggle-link-display ()
  "Toggle the literal or descriptive display of links."
  (interactive)
  (if org-descriptive-links
      (progn (org-remove-from-invisibility-spec '(org-link))
         (org-restart-font-lock)
         (setq org-descriptive-links nil))
    (progn (add-to-invisibility-spec '(org-link))
       (org-restart-font-lock)
       (setq org-descriptive-links t))))


(setq org-image-actual-width nil)

(use-package org-super-agenda)

(use-package org-zotxt
  :ensure zotxt
  :after org
  :init (add-hook 'org-mode-hook #'org-zotxt-mode)
)

(use-package org-recur
  :hook ((org-mode . org-recur-mode)
         (org-agenda-mode . org-recur-agenda-mode))
  :demand t
  :config
  (define-key org-recur-mode-map (kbd "C-c d") 'org-recur-finish)

  ;; Rebind the 'd' key in org-agenda (default: `org-agenda-day-view').
  (define-key org-recur-agenda-mode-map (kbd "d") 'org-recur-finish)
  (define-key org-recur-agenda-mode-map (kbd "C-c d") 'org-recur-finish)

  (setq org-recur-finish-done t
        org-recur-finish-archive t))

(provide 'init-org)
