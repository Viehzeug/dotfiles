(use-package f) ;; tools used in the following function
(use-package pyvenv)

(defvar conda-home "~/miniconda3" "Home dir used for python/conda.")
(defvar conda-home-envs (concat (file-name-as-directory conda-home) "envs") "Dir which includes defined virtualenvs.")

(defun set-conda-env (path)
  "Set the current venv to the conda enve of the given PATH."
  (setenv "WORKON_HOME" path)
  (pyvenv-workon ".")
  (message (concat "Setting virtualenv to " path))
  )

;; base on http://rakan.me/emacs/python-dev-with-emacs-and-pyenv/
(defun pyvenv-python-version-file ()
  "Automatically activates pyvenv if .python-version file exists."
  (interactive)
  (let ((python-version-directory (locate-dominating-file (buffer-file-name) ".python-version")))
    (if python-version-directory
        (let* ((pyenv-version-path (f-expand ".python-version" python-version-directory))
               (pyenv-current-version (s-trim (f-read-text pyenv-version-path 'utf-8))))
          (set-conda-env pyenv-current-version)
          ))))

(defun set-pyvenv ()
  "Set pyvenv matching the project name."
  (let ((project (downcase (projectile-project-name))))
    (if (member project (directory-files conda-home-envs)) ;; if we are in projectile and it matches a setup conda env -- use that
        (set-conda-env (concat (file-name-as-directory conda-home-envs) project))
      (pyvenv-python-version-file) ;; else see if there is a config file
      )))

(use-package pyvenv)

(use-package elpy
  :init (elpy-enable)
  :after (pyvenv projectile)
  :config
  (set-conda-env conda-home)
  (setq elpy-rpc-python-command "python")
  (setq python-shell-interpreter "python"
        python-shell-interpreter-args "-i")
  (setq elpy-rpc-backend "jedi")
  (add-hook 'elpy-mode-hook 'set-pyvenv))

(provide 'init-python)
