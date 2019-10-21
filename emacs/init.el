;; only debug during init; disabled at end.
(setq debug-on-error t)
(setq debug-on-quit t)

(setq gc-cons-threshold 50000000)

;; the package manager
(require 'package) (setq
 package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                    ("org" . "https://orgmode.org/elpa/") ("melpa"
                    . "https://melpa.org/packages/") ("melpa-stable"
                    . "https://stable.melpa.org/packages/"))
 package-archive-priorities '(("melpa-stable" . 1)))
;; package-check-signature 'nil)

(package-initialize)

;; load use-package
(when (not package-archive-contents)
  (package-refresh-contents) (package-install 'use-package))
(require 'use-package)
;; make sure that packages are installed
(setq use-package-always-ensure t)


;;---------------------------------------------
;; Global Setup; mostly UI
;;---------------------------------------------

(defun move-text-internal (arg)
   (cond
    ((and mark-active transient-mark-mode)
     (if (> (point) (mark))
            (exchange-point-and-mark))
     (let ((column (current-column))
              (text (delete-and-extract-region (point) (mark))))
       (forward-line arg)
       (move-to-column column t)
       (set-mark (point))
       (insert text)
       (exchange-point-and-mark)
       (setq deactivate-mark nil)))
    (t
     (beginning-of-line)
     (when (or (> arg 0) (not (bobp)))
       (forward-line)
       (when (or (< arg 0) (not (eobp)))
            (transpose-lines arg))
       (forward-line -1)))))

(defun move-text-down (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines down."
   (interactive "*p")
   (move-text-internal arg))

(defun move-text-up (arg)
   "Move region (transient-mark-mode active) or current line
  arg lines up."
   (interactive "*p")
   (move-text-internal (- arg)))

(global-set-key (kbd "C-S-<up>") 'move-text-up)
(global-set-key (kbd "C-S-<down>") 'move-text-down)


;; set default text width
(setq-default fill-column 70) ;; 70 fits nicly on half a 13'' macbook

;; disable warnings for evil and magit
(setq ad-redefinition-action 'accept)

;; fix shell
(setq exec-path-from-shell-arguments '("-l"));;
(use-package
exec-path-from-shell
  :config (when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize)))


(use-package ivy
  :diminish (ivy-mode . "")  ;; does not display ivy in the modeline
  :config
  (setq ivy-use-virtual-buffers t) :init (ivy-mode 1))

(use-package counsel
  :bind ("M-x" . counsel-M-x) )

(use-package swiper
  :bind
  ("\C-s" . swiper)
  ("C-c C-r" . ivy-resume)
  ("C-x C-f" . counsel-find-file))

(use-package neotree
  :bind
  ("C-§" . neotree-toggle)
  ("<f10>" . neotree-toggle))

(use-package flycheck
  :defer 1
  :init (global-flycheck-mode))

;; (eval-after-load 'flycheck
;;   '(progn
;;      (require 'flycheck-hdevtools)))

;; TODO flycheck modes

;;(use-package evil)

(use-package auto-complete
  :config (ac-config-default))

;; File search
(use-package find-file-in-project)

;; Theme
(use-package color-theme-solarized)
(setq frame-background-mode 'light)
;; (setq solarized-termcolors 256)

(if (daemonp)
(add-hook 'after-make-frame-functions
          '(lambda (f)
             (with-selected-frame f
               (when (window-system f) (load-theme 'solarized t)))))
(load-theme 'solarized t))

;; show linenumbers
(global-linum-mode t)
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; spaces instead of tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

;; Make tab key do indent first then completion.
(setq-default tab-always-indent 'complete)

;; show the matching parenthesis when the cursor is above one of them.
(setq show-paren-delay 0)
(show-paren-mode t)

;; highlight the current line
(global-hl-line-mode 1)

;; Don't clutter startup
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)

;; backward-kill-word as alternative to Backspace:
;; Kill the entire word instead of hitting Backspace key several
;; times. To do this will bind the =backward-kill-region= function to the
;; =C-w= key combination
;; (global-set-key "\C-w" 'backward-kill-word)
 ;; now we reasigne the original binding to that combination to a new one
;; (global-set-key "\C-x\C-k" 'kill-region)
;; (global-set-key "\C-c\C-k" 'kill-region)

;; use ibuffer by default
(defalias 'list-buffers 'ibuffer)

 ;; make sure that UTF-8 is used everywhere.
(set-terminal-coding-system  'utf-8)
(set-keyboard-coding-system  'utf-8)
(set-language-environment    'utf-8)
(set-selection-coding-system 'utf-8)
(setq locale-coding-system   'utf-8)
(prefer-coding-system        'utf-8)
(set-input-method nil)

(use-package which-key
  :defer 10
  :diminish which-key-mode
  :config
  (which-key-mode 1))


;; UI setup

(use-package whitespace
  :init
  ;; (dolist (hook '(prog-mode-hook text-mode-hook))
  ;;   (add-hook hook #'whitespace-mode))
  (add-hook 'before-save-hook #'whitespace-cleanup)
  :config
  (setq whitespace-style '(space-mark tab-mark))
  :bind
  ("<f11>" . whitespace-mode))

(use-package smartparens
  :defer 2
  :diminish smartparens-mode
  :config
  (require 'smartparens-config)
  (setq-default sp-autoinsert-pair nil)
  (smartparens-global-mode 't)
;;  (smartparens-strict-mode 't)
)

(use-package discover-my-major)

(menu-bar-mode +1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(use-package beacon
  :config (beacon-mode +1))

;; disable bell sound
(setq ring-bell-function 'ignore)

;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; Start server mode
(server-start)

;; make sure we find binaries; mainly sbt for scala
(when (memq window-system '(mac ns x))
 (add-to-list 'exec-path "/usr/local/bin"))


(use-package smart-mode-line
  :init
  (setq sml/theme 'respectful)
  (setq sml/no-confirm-load-theme t)
  (sml/setup)
  )


(use-package projectile
  :defer 1
  :config
  (projectile-global-mode +1))

(use-package counsel-projectile
  :defer 1
  :config
  (counsel-projectile-mode))

(use-package expand-region
  :bind
  ("C-@" . er/expand-region)
  ("C-=" . er/expand-region)
  )

(use-package windmove
  :bind
  ("C-c <up>" . windmove-up)
  ("C-c <down>" . windmove-down)
  ("C-c <left>" . windmove-left)
  ("C-c <right>" . windmove-right))

;;TODO:  Leger

;;fancy uft-8
(global-prettify-symbols-mode 1)
;;(setq prettify-symbols-unprettify-at-point t)
;;  (add-hook 'prog-mode-hook
;;            (lambda ()
;;              (push '("\lambda" . ?λ) prettify-symbols-alist)))

;; TODO fixme mode

; TODO maybe fully disable
(defvar backup-dir (expand-file-name "~/.emacs.d/emacs_backup/"))
(defvar autosave-dir (expand-file-name "~/.emacs.d/autosave/"))
(setq backup-directory-alist (list (cons ".*" backup-dir)))
(setq auto-save-list-file-prefix autosave-dir)
(setq auto-save-file-name-transforms `((".*" ,autosave-dir t)))
;; disable auto-save files & backups
(setq auto-save-default nil
      auto-save-list-file-prefix nil
      make-backup-files nil)


; fix tramp with zsh
 (eval-after-load 'tramp '(setenv "SHELL" "/bin/bash"))



(add-to-list 'load-path (expand-file-name "scripts" user-emacs-directory))
;;(require 'init-csv)
(require 'init-org)
(require 'init-git)
(require 'init-python)
;;(require 'init-markdown)
;;(require 'init-haskell)
;;(require 'init-web) ;; HTML, CSS, JS
(require 'init-latex)
;;(require 'init-agda)
;;(require 'init-scala)
(require 'init-spellchecking)
(require 'init-cheatsheet)

;; ;; easy keys to split window. Key based on ErgoEmacs keybinding
;; (global-set-key (kbd "M-3") 'delete-other-windows) ; expand current pane
;; (global-set-key (kbd "M-4") 'split-window-below) ; split pane top/bottom
;; (global-set-key (kbd "M-2") 'delete-window) ; close current pane
;; (global-set-key (kbd "M-s") 'other-window) ; cursor to other pane

;;better automatic window split
;;from https://stackoverflow.com/questions/2081577/setting-emacs-to-split-buffers-side-by-side

;; (defun display-new-buffer (buffer force-other-window)
;;   "If BUFFER is visible, select it.
;; If it's not visible and there's only one window, split the
;; current window and select BUFFER in the new window. If the
;; current window (before the split) is more than 100 columns wide,
;; split horizontally(left/right), else split vertically(up/down).
;; If the current buffer contains more than one window, select
;; BUFFER in the least recently used window.
;; This function returns the window which holds BUFFER.
;; FORCE-OTHER-WINDOW is ignored."
;;   (or (get-buffer-window buffer)
;;     (if (one-window-p)
;;         (let ((new-win
;;                (if (> (window-width) 100)
;;                    (split-window-horizontally)
;;                  (split-window-vertically))))
;;           (set-window-buffer new-win buffer)
;;           new-win)
;;       (let ((new-win (get-lru-window)))
;;         (set-window-buffer new-win buffer)
;;         new-win))))
;; ;; use display-buffer-alist instead of display-buffer-function if the following line won't work
;; (setq display-buffer-function 'display-new-buffer)

(set-face-attribute 'default nil :height 150 :family "Ubuntu Mono" :foreground "#657b83")
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Disable debugging
(setq debug-on-error nil)
(setq debug-on-quit nil)
;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(custom-safe-themes
;;    (quote
;;     ("a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" default)))
;;  '(package-selected-packages
;;    (quote
;;     (latex pyenv-mode which-key use-package smartparens smart-mode-line neotree magit haskell-mode flyspell-correct-ivy flycheck f expand-region exec-path-from-shell evil elpy ein discover-my-major csv-mode counsel-projectile color-theme-solarized cheatsheet beacon auctex))))
