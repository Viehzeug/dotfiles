(use-package org
  :init
  (setq org-log-done t)
  (setq org-agenda-files '("~/org"))
  :bind ("\C-ca" . org-agenda)
  :config (add-hook 'after-init-hook 'org-todo-list))

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

(use-package org-zotxt
  :ensure zotxt
  :after org
  :init (add-hook 'org-mode-hook #'org-zotxt-mode)
)

(provide 'init-org)
