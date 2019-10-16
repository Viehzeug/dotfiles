(use-package flymake)

(defun flymake-get-tex-args (file-name)
(list "pdflatex"
(list "-file-line-error" "-draftmode" "-interaction=nonstopmode" file-name)))

(defun turn-on-outline-minor-mode ()
(outline-minor-mode 1))

(use-package auctex
  :after flymake, ispell
  :defer t
  :config (
           (setq TeX-auto-save t)
           (setq TeX-parse-self t)
           (setq TeX-save-query nil)
           (add-hook 'LaTeX-mode-hook 'flymake-mode)
           (setq ispell-program-name "aspell") ; could be ispell as well, depending on your preferences
           (setq ispell-dictionary "english") ; this can obviously be set to any language your spell-checking program supports
           (add-hook 'LaTeX-mode-hook 'flyspell-mode)
           (add-hook 'LaTeX-mode-hook 'flyspell-buffer)
           (add-hook 'latex-mode-hook 'turn-on-outline-minor-mode)
           (setq outline-minor-mode-prefix "\C-c \C-o") ; Or something else
           (require 'tex-site)
           (autoload 'reftex-mode "reftex" "RefTeX Minor Mode" t)
           (autoload 'turn-on-reftex "reftex" "RefTeX Minor Mode" nil)
           (autoload 'reftex-citation "reftex-cite" "Make citation" nil)
           (autoload 'reftex-index-phrase-mode "reftex-index" "Phrase Mode" t)
           (add-hook 'latex-mode-hook 'turn-on-reftex) ; with Emacs latex mode
           (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
           (setq LaTeX-eqnarray-label "eq"
                 LaTeX-equation-label "eq"
                 LaTeX-figure-label "fig"
                 LaTeX-table-label "tab"
                 LaTeX-myChapter-label "chap"
                 TeX-auto-save t
                 TeX-newline-function 'reindent-then-newline-and-indent
                 TeX-parse-self t
                 LaTeX-section-hook
                 '(LaTeX-section-heading
                   LaTeX-section-title
                   LaTeX-section-toc
                   LaTeX-section-section
                   LaTeX-section-label)))
)

;; ;; from https://emacs.stackexchange.com/questions/13314/install-pdf-tools-on-emacs-macosx/22591

;; ;;; first `brew tap dunn/emacs` and `brew install pdf-tools --HEAD`

;; ;;; Install epdfinfo via 'brew install pdf-tools' and then install the
;; ;;; pdf-tools elisp via the use-package below. To upgrade the epdfinfo
;; ;;; server, just do 'brew upgrade pdf-tools' prior to upgrading to newest
;; ;;; pdf-tools package using Emacs package system. If things get messed
;; ;;; up, just do 'brew uninstall pdf-tools', wipe out the elpa
;; ;;; pdf-tools package and reinstall both as at the start.
;; (use-package pdf-tools
;;   :config
;;   (custom-set-variables
;;     '(pdf-tools-handle-upgrades nil)) ; Use brew upgrade pdf-tools instead.
;;   (setq pdf-info-epdfinfo-program "/usr/local/bin/epdfinfo")
;;   (pdf-tools-install)
;;   ;; from https://github.com/politza/pdf-tools/issues/189
;;   (add-hook 'pdf-view-mode-hook (lambda() (linum-mode -1)))
;;   )


;; ;; from https://github.com/lunaryorn/old-emacs-configuration/blob/2dcb6bb4b0a50bd6b8b02b0070463ed95096d70e/init.el#L1639-L1789

;; (use-package validate)                   ; Validate options)

;; ;;; LaTeX with AUCTeX
;; (use-package auctex
;;   :defer t
;;   :ensure t)                   ; AUCTeX initialization

;; (use-package tex                        ; TeX editing/processing
;;   :defer t
;;   :config
;;   (validate-setq
;;    TeX-parse-self t                     ; Parse documents to provide completion
;;                                         ; for packages, etc.
;;    TeX-auto-save t                      ; Automatically save style information
;;    TeX-electric-sub-and-superscript t   ; Automatically insert braces after
;;                                         ; sub- and superscripts in math mode
;;    ;; TeX-electric-math '("\\(" . "\\)")
;;    ;; Don't insert magic quotes right away.
;;    TeX-quote-after-quote t
;;    ;; Don't ask for confirmation when cleaning
;;    TeX-clean-confirm nil
;;    ;; Provide forward and inverse search with SyncTeX
;;    TeX-source-correlate-mode t
;;    TeX-source-correlate-method 'synctex)
;;   (setq-default TeX-master nil          ; Ask for the master file
;;                 TeX-engine 'luatex      ; Use a modern engine
;;                 ;; Redundant in 11.88, but keep for older AUCTeX
;;                 TeX-PDF-mode t)

;;   ;; Move to chktex
;;   (setcar (cdr (assoc "Check" TeX-command-list)) "chktex -v6 %s"))

;; (use-package tex-buf                    ; TeX buffer management
;;   :defer t
;;   ;; Don't ask for confirmation when saving before processing
;;   :config (validate-setq TeX-save-query nil))

;; (use-package tex-style                  ; TeX style
;;   :defer t
;;   :config
;;   ;; Enable support for csquotes
;;   (validate-setq LaTeX-csquotes-close-quote "}"
;;                  LaTeX-csquotes-open-quote "\\enquote{"))

;; (use-package tex-fold                   ; TeX folding
;;   :defer t
;;   :init (add-hook 'TeX-mode-hook #'TeX-fold-mode))

;; (use-package tex-mode                   ; TeX mode
;;   :defer t
;;   :config
;;   (font-lock-add-keywords 'latex-mode
;;                           `((,(rx "\\"
;;                                   symbol-start
;;                                   "fx" (1+ (or (syntax word) (syntax symbol)))
;;                                   symbol-end)
;;                              . font-lock-warning-face))))

;; (use-package latex                      ; LaTeX editing
;;   :defer t
;;   :config
;;   ;; Teach TeX folding about KOMA script sections
;;   (validate-setq
;;    TeX-outline-extra `((,(rx (0+ space) "\\section*{") 2)
;;                        (,(rx (0+ space) "\\subsection*{") 3)
;;                        (,(rx (0+ space) "\\subsubsection*{") 4)
;;                        (,(rx (0+ space) "\\minisec{") 5))
;;    ;; No language-specific hyphens please
;;    LaTeX-babel-hyphen "")
;;   (add-hook 'LaTeX-mode-hook #'LaTeX-math-mode) ; Easy math input
;;   ;; https://emacs.stackexchange.com/questions/21755/use-pdfview-as-default-auctex-pdf-viewer/21764
;;   (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
;;         TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
;;         TeX-source-correlate-start-server t)
;;   (add-hook 'TeX-after-compilation-finished-functions
;;             #'TeX-revert-document-buffer)
;;   ;; https://tex.stackexchange.com/questions/50827/a-simpletons-guide-to-tex-workflow-with-emacs
;;   (add-hook 'TeX-mode-hook 'flyspell-mode); Enable Flyspell mode for TeX modes such as AUCTeX. Highlights all misspelled words.

;;   )

  
;; (use-package auctex-latexmk             ; latexmk command for AUCTeX
;;   :after tex-site
;;   :config (auctex-latexmk-setup)
;;   )

;;   ;; (add-hook 'after-save-hook
;;   ;;           (lambda ()
;;   ;;             (when (string= major-mode 'latex-mode)
;;   ;;               (TeX-run-latexmk
;;   ;;                "LaTex"
;;   ;;                (format "latexmk -synctex=1 -xelatex %s" (buffer-file-name))
;;   ;;                (file-name-base (buffer-file-name))))))

;; (use-package bibtex                     ; BibTeX editing
;;   :defer t
;;   :config
;;   ;; Run prog mode hooks for bibtex
;;   (add-hook 'bibtex-mode-hook (lambda () (run-hooks 'prog-mode-hook)))

;;   ;; Use a modern BibTeX dialect
;;   (bibtex-set-dialect 'biblatex))

;; (defun lunaryorn-reftex-find-ams-environment-caption (environment)
;;   "Find the caption of an AMS ENVIRONMENT."
;;   (let ((re (rx-to-string `(and "\\begin{" ,environment "}"))))
;;     ;; Go to the beginning of the label first
;;     (re-search-backward re)
;;     (goto-char (match-end 0)))
;;   (if (not (looking-at (rx (zero-or-more space) "[")))
;;       (error "Environment %s has no title" environment)
;;     (let ((beg (match-end 0)))
;;       ;; Move point onto the title start bracket and move over to the end,
;;       ;; skipping any other brackets in between, and eventually extract the text
;;       ;; between the brackets
;;       (goto-char (1- beg))
;;       (forward-list)
;;       (buffer-substring-no-properties beg (1- (point))))))

;; (use-package reftex                     ; TeX/BibTeX cross-reference management
;;   :defer t
;;   :init (add-hook 'LaTeX-mode-hook #'reftex-mode)
;;   :config
;;   ;; Plug into AUCTeX
;;   (setq
;;    reftex-plug-into-AUCTeX t
;;    ;; Automatically derive labels, and prompt for confirmation
;;    reftex-insert-label-flags '(t t)
;;    reftex-label-alist
;;    '(
;;      ;; Additional label definitions for RefTeX.
;;      ("definition" ?d "def:" "~\\ref{%s}"
;;       lunaryorn-reftex-find-ams-environment-caption
;;       ("definition" "def.") -3)
;;      ("theorem" ?h "thm:" "~\\ref{%s}"
;;       lunaryorn-reftex-find-ams-environment-caption
;;       ("theorem" "th.") -3)
;;      ("example" ?x "ex:" "~\\ref{%s}"
;;       lunaryorn-reftex-find-ams-environment-caption
;;       ("example" "ex") -3)
;;      ;; Algorithms package
;;      ("algorithm" ?a "alg:" "~\\ref{%s}"
;;       "\\\\caption[[{]" ("algorithm" "alg") -3)))

;;   ;; Provide basic RefTeX support for biblatex
;;   (unless (assq 'biblatex reftex-cite-format-builtin)
;;     (add-to-list 'reftex-cite-format-builtin
;;                  '(biblatex "The biblatex package"
;;                             ((?\C-m . "\\cite[]{%l}")
;;                              (?t . "\\textcite{%l}")
;;                              (?a . "\\autocite[]{%l}")
;;                              (?p . "\\parencite{%l}")
;;                              (?f . "\\footcite[][]{%l}")
;;                              (?F . "\\fullcite[]{%l}")
;;                              (?x . "[]{%l}")
;;                              (?X . "{%l}"))))
;;     (validate-setq reftex-cite-format 'biblatex))
;;   :diminish reftex-mode)


(provide 'init-latex)
