;; Enable Debugging
;; Run with debugging enabled during ~init.el~. We disable it at the end.


;; only debug during init; disabled at end.
(setq debug-on-error t)
(setq debug-on-quit t)

;; Load ~use-package~


;; the package manager
(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/"))
      package-archive-priorities '(("melpa-stable" . 1)))
;; package-check-signature 'nil) ;; neccesary if cert is broken again

(package-initialize)

;; load use-package
(when (not package-archive-contents)
  (package-refresh-contents) (package-install 'use-package))
(require 'use-package)
;; make sure that packages are installed
(setq use-package-always-ensure t)

;; Garbage Collector
;; From https://github.com/MatthewZMD/.emacs.d:
;; - Defer garbage collection further back in the startup process,
;;   according to hlissner.
;;   - The GC eats up quite a bit of time, easily doubling startup
;;     time. The trick is to turn up the memory threshold as early as
;;     possible.
;; [[https://www.reddit.com/r/emacs/comments/eewwyh/officially_introducing_memacs/][However]] (user /u/eli-zaretskii):
;; - The threshold should be determined by each user, by starting from
;;   the default and doubling the value until they feel Emacs is fast
;;   enough. There's no single value that will satisfy everyone.


(setq gc-cons-threshold 100000000)

;; Fixes
;; Fix small things that cause warnings and errors. Especially on Mac OS.


;; disable warnings for evil and magit
(setq ad-redefinition-action 'accept)



;; Ensure that on OS X the correct paths from the shell config are loaded.


;; fix shell
;;(setq exec-path-from-shell-arguments '("-l"))
(use-package exec-path-from-shell
 :config (when (memq window-system '(mac ns x))
 (exec-path-from-shell-initialize)))

;; fix tramp with zsh
(eval-after-load 'tramp '(setenv "SHELL" "/bin/bash"))

;; Tools
;; Load tools that are required for other parts of the config.

;; Package used for hiding modeline info. This is what is called by
;; ~:diminish~.

(use-package diminish)



;; String, file and list libraries.

(use-package s)
(use-package f)
(use-package dash)

 (use-package hydra)



;; Nice icons. *You need to run* ~M-x all-the-icons-install-fonts~. The
;; script checks if the font is installed and else runs the command.


(use-package all-the-icons
    :init
    (unless (member "all-the-icons" (font-family-list))
    (all-the-icons-install-fonts t))) ;; see https://github.com/domtronn/all-the-icons.el/issues/120
(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

;; Auto-Update

(use-package auto-package-update
  :config
  ;; Delete residual old versions
  (setq auto-package-update-delete-old-versions t)
  ;; Do not bother me when updates have taken place.
  (setq auto-package-update-hide-results t)
  ;; Update installed packages at startup if there is an update pending.
  (auto-package-update-maybe))

;; Personal Setup

(setq user-full-name "Marc Fischer")
(setq user-mail-address "mail@marcfischer.at")

;; Backup

;; disable auto-save files & backups
(setq auto-save-default nil
      auto-save-list-file-prefix nil
      make-backup-files nil)
;; but in case soemthing goes wrong still place them in the .emacs.d
(defvar backup-dir (expand-file-name "~/.emacs.d/emacs_backup/"))
(defvar autosave-dir (expand-file-name "~/.emacs.d/autosave/"))

;; org mode
;; org mode is very early in the config to ensure that org from the org
;; elpa rather than melpa is loaded.


(use-package org
  :ensure org ;;org-plus-contrib ;; currently seems broken
  :pin org ; only download orgmode from the org server
  :init
  (setq org-agenda-files '("~/org/") ;;'("~/org/todo.org" "~/org/in.org" "~/org/projects.org")
	      org-catch-invisible-edits 'show
	      org-confirm-babel-evaluate nil ;; run without confirmation
	      org-src-preserve-indentation t ;; preserve indentation at export
	      org-highlight-latex-and-related '(latex)

        ;; Make org and org-recur work nicely
        ;; Log time a task was set to Done.
        org-log-done (quote time)
        ;; Don't log the time a task was rescheduled or redeadlined.
        org-log-redeadline nil
        org-log-reschedule nil
        org-read-date-prefer-future 'time
        )

  :bind ("\C-ca" . org-agenda)
  :config

  ;; Allow the :ignore: to ignore headers in exporing
  ;; wait until org-plus is not broken
  ;;(require 'ox-extra)
  ;;(ox-extras-activate '(ignore-headlines))

  ;; manual workaround
  (add-to-list 'load-path "~/dotfiles/emacs/elpa/org-contrib/")
  (require 'ox-extra)
  (ox-extras-activate '(ignore-headlines))

  ;; refile setup
  (setq org-refile-targets '((org-agenda-files :maxlevel . 2)) ;; show two levels of headings
        org-refile-allow-creating-parent-nodes 'confirm        ;; allow to create new nodes
        org-refile-use-outline-path 'file                      ;; allow to file to top level of files
        org-outline-path-complete-in-steps nil                 ;; present all possilbe paths at once
  )

  ;; make org play well with org reccur
  ;; Refresh org-agenda after rescheduling a task.
  (defun org-agenda-refresh ()
    "Refresh all `org-agenda' buffers."
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (derived-mode-p 'org-agenda-mode)
          (org-agenda-maybe-redo)))))

  (defadvice org-schedule (after refresh-agenda activate)
  "Refresh org-agenda."
  (org-agenda-refresh))

  ;; Try to minimize org sync conflicts by autosaving (https://christiantietze.de/posts/2019/03/sync-emacs-org-files/)
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

(use-package org-noter
    :after org
    :config
    (setq org-noter-always-create-frame nil
          org-noter-insert-note-no-questions t
          org-noter-separate-notes-from-heading t
          org-noter-auto-save-last-location t))

(use-package org-super-agenda
  :after org
  :config
  (org-super-agenda-mode))

(use-package org-noter
:after org)

(use-package org-zotxt
  :ensure zotxt
  :pin melpa ;; use newest verions
  :diminish
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

;; Theme

;; Theme
(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-solarized-light t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config)

  ;; set font
  (set-face-attribute 'default nil :family "Ubuntu Mono" :height 150)
  
)

;; show linenumbers
(global-linum-mode t)
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; show the matching parenthesis when the cursor is above one of them.
(setq show-paren-delay 0)
(setq show-paren-style 'mixed)
(show-paren-mode t)
(use-package smartparens
 :diminish
 :config
 (progn
   (require 'smartparens-config)
   (smartparens-global-mode 1)
   (show-paren-mode t)))

;; highlight the current line
(global-hl-line-mode t)

;; Don't clutter startup
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)

;; disable toolbars
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

; display a small wave after the cursor when jumping around
(use-package beacon
   :config (beacon-mode +1))

;; disable bell sound
(setq ring-bell-function 'ignore
;;       visible-bell 1 ;; we already have the doom-theme bell setup
)

;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; modeline


(use-package doom-modeline
      :ensure t
      :hook (after-init . doom-modeline-mode)
      :custom
      (inhibit-compacting-font-caches t)
      (doom-modeline-minor-modes t)
      (doom-modeline-icon t)
      (doom-modeline-major-mode-color-icon t)
      (doom-modeline-height 15))

;; Text width (Fill-mode)
;; ~M-q~ (~fill-paragraph~) justifies paragraphs and automatically breaks
;; them. Here we set the set standard text width.

(setq-default fill-column 70) ;; 70 fits nicly on half a 13'' macbook
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'org-mode-hook 'turn-on-auto-fill)

;; Counsel, Swiper and Ivy
;; Setup the ivy auto-complete package along with swiper
;; (ivy-text-search) and counsel (ivy-M-x).

(use-package ivy
  :diminish
  :config
  (setq ivy-use-virtual-buffers t) :init (ivy-mode 1))

(use-package counsel
  :after ivy)

(use-package swiper
  :after counsel
  :bind
  ("M-x" . counsel-M-x)
  ("\C-s" . swiper)
  ("M-s" . swiper-all)
  ("C-c C-r" . ivy-resume)
  ("C-c p" . counsel-git)
  ("C-c r" . counsel-rg)
  ("C-x C-f" . counsel-find-file)
  (("M-y" . counsel-yank-pop)
  :map ivy-minibuffer-map
  ("M-y" . ivy-next-line)) ;; multiple pressed cycles through choices; taken from http://pragmaticemacs.com/emacs/counsel-yank-pop-with-a-tweak/
)

;; Search

(use-package ripgrep
  :bind
  ("C-c C-r" . ripgrep-regexp))
(use-package ag) ;; currently not used but frequently experimented with

;; Buffers


;; use ibuffer by default
(defalias 'list-buffers 'ibuffer)

;; Indentation

;; Always stay indented: Automatically have blocks reindented after every change.
(use-package aggressive-indent
  :config (global-aggressive-indent-mode t))

;; spaces instead of tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

;; Make tab key do indent first then completion.
(setq-default tab-always-indent 'complete)

;; Parenthesis and Whitespace

(use-package whitespace
  :init
  :diminish
  :bind
  ("<f11>" . whitespace-mode)
  ("C-c w" . delete-trailing-whitespace))

;; comments

(global-set-key (kbd "M-;") 'comment-region)
(global-set-key (kbd "C-M-;") 'uncomment-region)

;; UTF-8


;; make sure that UTF-8 is used everywhere.
(set-terminal-coding-system  'utf-8)
(set-keyboard-coding-system  'utf-8)
(set-language-environment    'utf-8)
(set-selection-coding-system 'utf-8)
(setq locale-coding-system   'utf-8)
(prefer-coding-system        'utf-8)
(set-input-method nil)

;;fancy uft-8
(global-prettify-symbols-mode 1)

;; Help & Documentation
;; Display possible keys after a partial commands is entered.

(use-package which-key
  :diminish
  :config (which-key-mode)
          (setq which-key-idle-delay 0.05))



;; My own cheatsheet. Based on [[https://github.com/darksmile/cheatsheet/blob/master/cheatsheet.el][cheatsheet.el]].

(defun cheatsheet-show ()
  "Create buffer and show cheatsheet."
  (interactive)
  ;;(switch-to-buffer-other-window "*cheatsheet*")
  ;;(erase-buffer)
  (find-file (concat user-emacs-directory "CheatSheet.pdf"))
  (rename-buffer "*cheatsheet*")
  (setq buffer-read-only t))

(defun cheatsheet-toggle()
  (interactive)
  (if (get-buffer "*cheatsheet*")
    (kill-buffer "*cheatsheet*")
    (cheatsheet-show)))

(global-set-key (kbd "C-<f1>") 'cheatsheet-toggle)

;; Spellchecking

;; Taken/inspired by https://github.com/kaushalmodi/.emacs.d/blob/master/setup-files/setup-spell.el

(use-package ispell
  :if (not (bound-and-true-p disable-pkg-ispell))
  :ensure f
  :config
  (setq ispell-program-name "aspell")
  (setq ispell-extra-args   '("--sug-mode=ultra"
                              "--lang=en_US"))

;; Save a new word to personal dictionary without asking
(setq ispell-silently-savep t))

(use-package flyspell
  :ensure f
  :diminish
  :after ispell
  :init
  (progn
    ;; Below variables need to be set before `flyspell' is loaded.
    (setq flyspell-use-meta-tab nil)
    ;; Binding for `flyspell-auto-correct-previous-word'.
    (setq flyspell-auto-correct-binding (kbd "<S-f12>")))
  :hook ((prog-mode . flyspell-prog-mode)
           (org-mode . flyspell-mode)
           (text-mode . flyspell-mode))
)

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
  (("<f12>" . flyspell-correct-at-point)
   ("<f8>" .   'fd-switch-dictionary)))

;; Writegood

(use-package writegood-mode
  :hook (text-mode org-mode)
  :diminish
  :config
  (--map (push it writegood-weasel-words) ;; some words form https://github.com/alhassy/emacs.d#cosmetics
         '("some" "simple" "simply" "easy" "often" "easily" "probably" "really"
           "clearly"               ;; Is the premise undeniably true?
           "experience shows"      ;; Whose? What kind? How does it do so?
           "may have"              ;; It may also have not!
           "it turns out that")))  ;; How does it turn out so?

;; Subword
;; In CamelCase treat all words as words.


(global-subword-mode 1)
(diminish  'subword-mode)

;; Syntax Checking

(use-package flycheck
  :diminish
  :init (global-flycheck-mode)
  :custom (flycheck-display-errors-delay .3))

;; Revert Buffers

(global-set-key [f5] '(lambda () (interactive) (revert-buffer nil t nil)))

;; Server

;; Start server mode
(server-start)

;; Files

(use-package dired
  :ensure nil
  :custom
  ;; Auto revert
  (auto-revert-use-notify nil)
  (auto-revert-interval 3))

;; Auto complete

(use-package company
  :diminish
  :config
  (global-company-mode 1)
  (setq ;; Only 2 letters required for completion to activate.
        company-minimum-prefix-length 2

        ;; Search other buffers for compleition candidates
        company-dabbrev-other-buffers t
        company-dabbrev-code-other-buffers t

        ;; Allow (lengthy) numbers to be eligible for completion.
        company-complete-number t

        ;; M-⟪num⟫ to select an option according to its number.
        company-show-numbers t

        ;; Edge of the completion list cycles around.
        company-selection-wrap-around t

        ;; Do not downcase completions by default.
        company-dabbrev-downcase nil

        ;; Even if I write something with the ‘wrong’ case,
        ;; provide the ‘correct’ casing.
        company-dabbrev-ignore-case t

        ;; Immediately activate completion.
        company-idle-delay 0))

;; Projectile
;; Currently unused, but still here as dependency for some features (see
;; Python section).

   (use-package projectile
     :diminish
     :config
     (projectile-global-mode +1))

   (use-package counsel-projectile
     :diminish 
     :config
     (counsel-projectile-mode))

;; Block movement of regions
;; Move code regions up and down with ~C-S-<up>~ and ~C-S-<down>~ (similar to Eclipse).

(use-package move-text
 ;; :init (move-text-default-bindings)
 :bind
 (("C-S-<up>" . move-text-up)
  ("C-S-<down>" . move-text-down))
)

;; Expand Region

(use-package expand-region
  :bind
  ("C-@" . er/expand-region)
  ("C-=" . er/expand-region)
)

;; Movement

(use-package windmove
  :bind
  ("C-c <up>" . windmove-up)
  ("C-c <down>" . windmove-down)
  ("C-c <left>" . windmove-left)
  ("C-c <right>" . windmove-right))

(use-package ace-window
  :init
  (progn
    (global-set-key (kbd "M-o") 'ace-window)
    (global-set-key (kbd "<f9>") 'ace-window))
  :config
    (set-face-attribute
     'aw-leading-char-face nil
     :foreground "deep sky blue"
     :weight 'bold
     :height 3.0)
    (set-face-attribute
     'aw-mode-line-face nil
     :inherit 'mode-line-buffer-id
     :foreground "lawn green")
    (setq aw-keys '(?a ?s ?d ?f ?j ?k ?l)
          aw-dispatch-always t
          aw-dispatch-alist
          '((?x aw-delete-window "Ace - Delete Window")
            (?c aw-swap-window "Ace - Swap Window")
            (?n aw-flip-window)
            (?v aw-split-window-vert "Ace - Split Vert Window")
            (?h aw-split-window-horz "Ace - Split Horz Window")
            (?m delete-other-windows "Ace - Maximize Window")
            (?g delete-other-windows)
            (?b balance-windows)))

    (defhydra hydra-window-size (:color red)
         "Windows size"
         ("h" shrink-window-horizontally "shrink horizontal")
         ("j" shrink-window "shrink vertical")
         ("k" enlarge-window "enlarge vertical")
         ("l" enlarge-window-horizontally "enlarge horizontal"))
    (defhydra hydra-window-frame (:color red)
         "Frame"
         ("f" make-frame "new frame")
         ("x" delete-frame "delete frame"))
    (defhydra hydra-window-scroll (:color red)
         "Scroll other window"
         ("n" joe-scroll-other-window "scroll")
         ("p" joe-scroll-other-window-down "scroll down"))
       (add-to-list 'aw-dispatch-alist '(?w hydra-window-size/body) t)
       (add-to-list 'aw-dispatch-alist '(?o hydra-window-scroll/body) t)
       (add-to-list 'aw-dispatch-alist '(?\; hydra-window-frame/body) t)
     (ace-window-display-mode t))

;; make C-a move to the beginning of the line on first press; on further presses go to beginning of code
;; same for C-e and end
(use-package mwim
  :bind
  ("C-a" . mwim-beginning)
  ("C-e" . mwim-end))

;; Unbind unneeded keys
(global-set-key (kbd "C-z") nil)
(use-package avy
  :bind
  (("C-z c" . avy-goto-char-timer)
   ("C-z l" . avy-goto-line))
  :custom
    (avy-timeout-seconds 0.3)
    (avy-style 'pre)
  :custom-face
    (avy-lead-face ((t (:background "#51afef" :foreground "#870000" :weight bold)))));

;; Undo
;; Copied from https://github.com/alhassy/emacs.d

;; Allow tree-semantics for undo operations.
(use-package undo-tree
  :diminish                       ;; Don't show an icon in the modeline
  :config
    ;; Always have it on
    (global-undo-tree-mode)

    ;; Each node in the undo tree should have a timestamp.
    (setq undo-tree-visualizer-timestamps t)

    ;; Show a diff window displaying changes between undo nodes.
    (setq undo-tree-visualizer-diff t))

;; Execute (undo-tree-visualize) then navigate along the tree to witness
;; changes being made to your file live!

;; Ledger

;; ledger mode
(use-package ledger-mode)

;; git

(use-package magit)

;; python

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
        python-shell-interpreter-args "-i"
        python-indent-offset 4)
  (setq elpy-rpc-backend "jedi")
  (add-hook 'elpy-mode-hook 'set-pyvenv))

;; pdf

;; Disable line numbers when in pdf mode.


(use-package pdf-tools
  :config (pdf-tools-install)
  :init (add-hook 'pdf-view-mode-hook (lambda() (linum-mode -1))))

;; latex


(use-package flymake)

(use-package latex
  :ensure auctex
  :after flymake
  :config
  (setq auto-mode-alist (cons '("\\.tex$" . latex-mode) auto-mode-alist)
        TeX-auto-save t
        TeX-parse-self t
        TeX-save-query nil
        TeX-electric-sub-and-superscript t   ; Automatically insert
                                             ; braces after sub- and
                                             ; superscripts in math
                                             ; mode
        TeX-source-correlate-mode t
        TeX-source-correlate-method 'synctex
        ispell-program-name "aspell"
        ispell-dictionary "english"
        LaTeX-section-hook
        '(LaTeX-section-heading
          LaTeX-section-title
          LaTeX-section-toc
          LaTeX-section-section
          LaTeX-section-label))


  (add-hook 'LaTeX-mode-hook 'flymake-mode)
  (add-hook 'LaTeX-mode-hook 'flyspell-mode)
  (add-hook 'LaTeX-mode-hook 'flyspell-buffer)

  ;; The following defadvice and defun make C-c C-s (insert section)
  ;; behave nicly.

  (defadvice LaTeX-section (after LaTeX-section-after activate)
    "After LaTeX-section delte the unecessarily inserted newline."
    (delete-char -1))

  (defun LaTeX-section-label()
    (let ((is-sec (<= level 4)))
      (progn
        (delete-char -1)
        (if is-sec (LaTeX-label name 'section))
        (insert " \%\n")
        (if is-sec (insert "\n"))
        )
      ))
  )

(use-package auctex-latexmk
  :after latex
  :config (auctex-latexmk-setup)
  )


;;   https://emacs.stackexchange.com/questions/21755/use-pdfview-as-default-auctex-pdf-viewer/21764
;;   (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
;;         TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
;;         TeX-source-correlate-start-server t)
;;   (add-hook 'TeX-after-compilation-finished-functions
;;             #'TeX-revert-document-buffer)

;;   ;; (add-hook 'after-save-hook
;;   ;;           (lambda ()
;;   ;;             (when (string= major-mode 'latex-mode)
;;   ;;               (TeX-run-latexmk
;;   ;;                "LaTex"
;;   ;;                (format "latexmk -synctex=1 -xelatex %s" (buffer-file-name))
;;   ;;                (file-name-base (buffer-file-name))))))

(use-package reftex                     ; TeX/BibTeX cross-reference management
  :after latex
  :init (add-hook 'LaTeX-mode-hook #'reftex-mode)
  :config
  (setq  reftex-plug-into-AUCTeX t)
  ;; Make cref work -- https://tex.stackexchange.com/questions/119253/cleveref-auctex-and-reftex-set-up/119273#119273
  (TeX-add-style-hook
   "cleveref"
   (lambda ()
     (if (boundp 'reftex-ref-style-alist)
         (add-to-list
          'reftex-ref-style-alist
          '("Cleveref" "cleveref"
            (("\\cref" ?c) ("\\Cref" ?C) ("\\cpageref" ?d) ("\\Cpageref" ?D)))))
     (reftex-ref-style-activate "Cleveref")
     (TeX-add-symbols
      '("cref" TeX-arg-ref)
      '("Cref" TeX-arg-ref)
      '("cpageref" TeX-arg-ref)
      '("Cpageref" TeX-arg-ref))))
  :diminish reftex-mode)

;; Disable debugging

;; Disable debugging
(setq debug-on-error nil)
(setq debug-on-quit nil)
