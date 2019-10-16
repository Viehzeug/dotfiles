;; Taken/inspired by https://github.com/kaushalmodi/.emacs.d/blob/master/setup-files/setup-spell.el

(use-package ispell
  :if (not (bound-and-true-p disable-pkg-ispell))
  :defer 15
  :ensure f
  :config
  (setq ispell-program-name "aspell")
  (setq ispell-extra-args   '("--sug-mode=ultra"
                              "--lang=en_US"))
  ;; Save a new word to personal dictionary without asking
  (setq ispell-silently-savep t))
  
(use-package flyspell
  :ensure f
  :after ispell
  :init
  (progn
    ;; Below variables need to be set before `flyspell' is loaded.
    (setq flyspell-use-meta-tab nil)
    ;; Binding for `flyspell-auto-correct-previous-word'.
    (setq flyspell-auto-correct-binding (kbd "<S-f12>")))
  :config
  (progn
    (add-hook 'prog-mode-hook #'flyspell-prog-mode)
    (with-eval-after-load 'auto-complete
      (ac-flyspell-workaround))
    ;; https://github.com/larstvei/dot-emacs#flyspell
    (add-hook 'text-mode-hook #'turn-on-flyspell)
    (add-hook 'org-mode-hook  #'turn-on-flyspell)
    (add-hook 'TeX-mode-hook  #'turn-on-flyspell)
    ;; Flyspell signals an error if there is no spell-checking tool is
    ;; installed. We can advice `turn-on-flyspell' and `flyspell-prog-mode'
    ;; to try to enable flyspell only if a spell-checking tool is available.
    (defun modi/ispell-not-avail-p (&rest args)
      "Return `nil' if `ispell-program-name' is available; `t' otherwise."
      (not (executable-find ispell-program-name)))
    (advice-add 'turn-on-flyspell   :before-until #'modi/ispell-not-avail-p)
    (advice-add 'flyspell-prog-mode :before-until #'modi/ispell-not-avail-p)
    ))

(use-package flyspell-correct
  :after flyspell)

(defun fd-switch-dictionary()
  (interactive)
  (let* ((dic ispell-current-dictionary)
         (change (if (string= dic "deutsch8") "english" "deutsch8")))
    (ispell-change-dictionary change)
    (message "Dictionary switched from %s to %s" dic change)
    ))

;; https://github.com/d12frosted/flyspell-correct
(use-package flyspell-correct-ivy
  :after flyspell-correct
  :bind
  (("<f12>" . flyspell-correct-word-generic)
   ("<f8>" .   'fd-switch-dictionary)))


(provide 'init-spellchecking)
