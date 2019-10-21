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


(provide 'init-latex)
