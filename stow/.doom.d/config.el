;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Marc Fischer"
      user-mail-address "mail@marcfischer.at")

;; https://tecosaur.github.io/emacs-config/config.html#simple-settings
(setq undo-limit 80000000                         ; Raise undo-limit to 80Mb
      evil-want-fine-undo t                       ; By default while in insert all changes are one big blob. Be more granular
      );;auto-save-default t                         ; Nobody likes to loose work, I certainly don't


;; from https://tecosaur.github.io/emacs-config/config.html#windows
(map! :map evil-window-map
      "SPC" #'rotate-layout
      ;; Navigation
      "<left>"     #'evil-window-left
      "<down>"     #'evil-window-down
      "<up>"       #'evil-window-up
      "<right>"    #'evil-window-right
      ;; Swapping windows
      "C-<left>"       #'+evil/window-move-left
      "C-<down>"       #'+evil/window-move-down
      "C-<up>"         #'+evil/window-move-up
      "C-<right>"      #'+evil/window-move-right)


(setq doom-font (font-spec :family "Ubuntu Mono" :size 17 :weight 'semi-light)
      doom-variable-pitch-font (font-spec :family "Ubuntu" :size 16))


;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/"
      org-roam-directory (concat org-directory "roamv2/"))

(setq doom-theme 'doom-solarized-light)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

(plist-put! +ligatures-extra-symbols
  ;; org
  :name          "»"
  :src_block     "»"
  :src_block_end "«"
  :quote         "“"
  :quote_end     "”"
  ;; Functional
  :lambda        "λ"
  :def           "ƒ"
  :composition   "∘"
  :map           "↦"
  ;; Types
  ;; :null          "∅"
  ;; :true          "𝕋"
  ;; :false         "𝔽"
  ;; :int           "ℤ"
  ;; :float         "ℝ"
  ;; :str           "𝕊"
  ;; :bool          "𝔹"
  ;; :list          "𝕃"
  ;; Flow
  :not           "￢"
  :in            "∈"
  :not-in        "∉"
  :and           "∧"
  :or            "∨"
  :for           "∀"
  :some          "∃"
  :return        "⟼"
  :yield         "⟻"
  ;; Other
  :union         "⋃"
  :intersect     "∩"
  :diff          "∖"
  :tuple         "⨂"
  ;;:pipe          "" ;; FIXME: find a non-private char
  :dot           "•")  ;; you could also add your own if you want

(setq mac-command-modifier      'super
      ns-command-modifier       'super
      mac-option-modifier       'meta
      ns-option-modifier        'meta
      mac-right-option-modifier 'meta
      ns-right-option-modifier  'meta)

(setq +latex-viewers '(pdf-tools))

(after! org
  (setq ;; Make org and org-recur work nicely
   ;; Log time a task was set to Done.
   org-log-done (quote time)
   ;; Don't log the time a task was rescheduled or redeadlined.
   org-log-redeadline nil
   org-log-reschedule nil
   org-read-date-prefer-future 'time

   ;; Setup Agenda
   org-agenda-span 7 ;; show 7 days
   org-agenda-start-on-weekday nil  ;; start from current day (rather than monday)
   org-agenda-start-day "-1d"   ;; show 1 day beforet

   ;; File setup
   org-directory "~/org/"
   org-archive-location (concat org-directory ".archive/%s::")
   org-roam-directory (concat org-directory "/notes")
   org-roam-db-location "~/.org-roam.db" ;; make synced

   ;; Journal
   org-journal-date-format "%d %B %Y"
   org-journal-file-format "%Y%m%d.org")

  )

(use-package! org-recur
  :after org
  :hook ((org-mode . org-recur-mode)
         (org-agenda-mode . org-recur-agenda-mode))
  :init
  (setq org-recur-finish-done t
        org-recur-finish-archive t)
  :config
  (defadvice org-schedule (after refresh-agenda activate))
  (map! :map org-mode-map
      :leader
      :desc "Mark as done (and reshedule if appicalbe)" "d" #'org-recur-finish)
)

;; (after! org-roam
;;   (setq org-roam-capture-templates
;;         '(("d" "default" plain (function org-roam--capture-get-point)
;;            "%?"
;;            :file-name "${slug}"
;;            :head "#+title: ${title}\n#+ROAM_TAGS: ${tag}\n"
;;            :immediate-finish t
;;            :unnarrowed t))
;;         org-roam-capture-ref-templates
;;         '(("r" "ref" plain (function org-roam-capture--get-point)
;;            "%?"
;;            :file-name "${slug}"
;;            :head "#+roam_key: ${ref}
;; #+roam_tags: ${tag}
;; #+title: ${title}
;; - source :: ${ref}"
;;            :unnarrowed t)))
;;   )

(use-package! websocket
    :after org-roam)

(use-package! org-roam-ui
    :after org-roam ;; or :after org
;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;;         a hookable mode anymore, you're advised to pick something yourself
;;         if you don't care about startup time, use
;;  :hook (after-init . org-roam-ui-mode)
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))

;; (use-package! org-roam-server
;;   :after org-roam
;;   :config
;;   (setq org-roam-server-host "127.0.0.1"
;;         org-roam-server-port 6005
;;         org-roam-server-authenticate nil
;;         org-roam-server-export-inline-images t
;;         org-roam-server-serve-files nil
;;         org-roam-server-served-file-extensions '("pdf" "mp4" "ogv")
;;         org-roam-server-network-poll t
;;         org-roam-server-network-arrows nil
;;         org-roam-server-network-label-truncate t
;;         org-roam-server-network-label-truncate-length 60
;;         org-roam-server-network-label-wrap-length 20))


;; (defun org-roam-server-open ()
;;   "Ensure the server is active, then open the roam graph."
;;   (interactive)
;;   (smartparens-global-mode -1)
;;   (org-roam-server-mode 1)
;;   ;;  (browse-url-xdg-open (format "http://localhost:%d" org-roam-server-port))
;;   (smartparens-global-mode 1))

;; ;; automatically enable server-mode
;; (after! org-roam
;;   (smartparens-global-mode -1)
;;   (org-roam-server-mode)
;;   (smartparens-global-mode 1))

(after! org
  (setq org-capture-templates
        '(("w" "work todo" entry (file "~/org/todo.org") "* TODO %? :work: \n SCHEDULED: %^t \n")
          ("p" "private todo" entry (file "~/org/todo.org") "* TODO %? :private: \n SCHEDULED: %^t \n")
          ("c" "cooking" entry (file "~/org/cooking.org") "* %?\n")
          ("d" "date/calendar entry" entry (file "~/org/cal.org") "* %?\n%^T")
          ("r" "reading" entry (file "~/org/read.org") "* %?\n")
          ("m" "media [music, games, movies, recreational books] to consider" entry (file+headline "~/org/media.org" "To check out") "** %? %^g\n")
          ("s" "want (shopping)" entry (file+headline "~/org/shopping.org" "Want") "** %?\n")
          ("o" "quote" entry (file "~/org/quotes.org") "* %^{quote}\n:PROPERTIES:\n:BY: %^{by}\n:FROM: %^{from}\n:END:" :empty-lines 1)))
  )

;; zotero pdf support
;; https://github.com/jkitchin/org-ref/blob/4f26ac56db785b4bff05e75ae7decc44be2ba89e/org-ref.org
(defun my/org-ref-open-pdf-at-point ()
  "Open the pdf for bibtex key under point if it exists."
  (interactive)
  (let* ((results (org-ref-get-bibtex-key-and-file))
         (key (car results))
	 (pdf-file (car (bibtex-completion-find-pdf key))))
    (if (file-exists-p pdf-file)
	(org-open-file pdf-file)
      (message "No PDF found for %s" key))))

(use-package! ivy-bibtex
  ;;:when (featurep! :completion ivy)
  ;;:commands (ivy-bibtex)
  :config
  (setq ivy-bibtex-default-action 'ivy-bibtex-insert-key)
  (add-to-list 'ivy-re-builders-alist '(ivy-bibtex . ivy--regex-plus))
  (when IS-MAC
    (ivy-bibtex-ivify-action bibtex-completion-quicklook ivy-bibtex-quicklook)
    (ivy-add-actions 'ivy-bibtex '(("SPC" ivy-bibtex-quicklook "Quick look")))))





(use-package! org-ref
  :after (org ivy-bibtex)
  :config
  (setq reftex-default-bibliography '("~/org/bibliography/zotero.bib")
        bibtex-completion-bibliography '("~/org/bibliography/zotero.bib")
        org-ref-bibliography-notes "~/org/bibliography/notes.org"
        org-ref-default-bibliography '("~/org/bibliography/zotero.bib")
        org-ref-pdf-directory "~/org/bibliography/pdfs/"
        org-ref-open-pdf-function 'my/org-ref-open-pdf-at-point
        org-ref-completion-library 'org-ref-ivy-cite
        org-ref-get-pdf-filename-function 'org-ref-get-pdf-filename-ivy-bibtex)
  )
(map! :map org-mode-map
      :leader
      :desc "Cite from Zotero" "]" #'org-ref-ivy-insert-cite-link)

(use-package org-roam-bibtex
  :after (org-roam org-ref)
  :hook (org-roam-mode . org-roam-bibtex-mode)
  :config (setq orb-insert-interface 'ivy-bibtex
                orb-preformat-keywords '("citekey" "title" "url" "author-or-editor" "keywords" "file")
	        orb-process-file-field t
	        orb-file-field-extensions "pdf"
                orb-templates '(("r" "ref" plain (function org-roam-capture--get-point) ""
                                 :file-name "${citekey}"
                                 :head "#+TITLE: ${title}\n#+ROAM_KEY: ${ref}\n#+ROAM_TAGS: paper

 - tags ::
 - keywords :: ${keywords}

 * ${title}
 :PROPERTIES:
 :Custom_ID: ${citekey}
 :URL: ${url}
 :AUTHOR: ${author-or-editor}
 :NOTER_DOCUMENT: ${file}
 :NOTER_PAGE:
 :END:"
                                 ))
                ))

(map! :leader
      (:prefix-map ("C" . "additional capture")
       :desc "Journal Entry" "j" #'org-journal-new-entry
       :desc "Paper" "p" #'orb-insert
       :desc "Roam Capture" "r" #'org-roam-find-file
       :desc "Roam Daily" "d" #'org-roam-dailies-find-today
       ))

(use-package! org-super-agenda
  :commands (org-super-agenda-mode))
(after! org-agenda
  (org-super-agenda-mode))

(setq org-agenda-include-deadlines t
      ;;org-agenda-block-separator nil
      org-agenda-skip-scheduled-if-done t
      org-agenda-tags-column 80
      org-agenda-compact-blocks nil)


(setq org-super-agenda-groups
      '((:name "Today"
         :time-grid t
         )
        (:name "Work"
         :tag "work")
        (:name "Private"
         :tag "private")))

(use-package! org-ql
  :after org)

(use-package! d-mode)

(after! elfeed
  (setq elfeed-feeds
        '("https://francisbach.com/feed/"
          "https://akosiorek.github.io/feed.xml"
          "https://www.inference.vc/rss/")))

(after! ispell
  ;; Configure `LANG`, otherwise ispell.el cannot find a 'default
  ;; dictionary' even though multiple dictionaries will be configured
  ;; in next line.
  ;;(setenv "LANG" "en_US")
  ;; Configure German, Swiss German, and two variants of English.
  (setq ispell-dictionary "en");; "de_DE,de_CH,de_AT,en_GB,en_US")
  ;; ispell-set-spellchecker-params has to be called
  ;; before ispell-hunspell-add-multi-dic will work
  ;; (ispell-set-spellchecker-params)
  ;;(ispell-hunspell-add-multi-dic "de_DE,de_CH,de_AT,en_GB,en_US")
  )

;; (defun open-cal()
;;   (interactive)
;;   (cfw:open-calendar-buffer
;;    :contents-sources
;;    (list
;;     (cfw:org-create-file-source "cal" "~/org/cal.org" "Green")
;;     (cfw:org-create-source "Blue")  ; org-agenda source
;;                                         ;(cfw:org-create-file-source "cal" "/path/to/cal.org" "Cyan")  ; other org source
;;                                         ;(cfw:howm-create-source "Blue")  ; howm source
;;                                         ;(cfw:cal-create-source "Orange") ; diary source
;;                                         ;(cfw:ical-create-source "Moon" "~/moon.ics" "Gray")  ; ICS source1
;;                                         ;(cfw:ical-create-source "gcal" "https://..../basic.ics" "IndianRed") ; google calendar ICS
;;     )))

(add-to-list 'tramp-remote-path "/home/marc/miniconda3/bin")
;; (add-to-list 'tramp-remote-path 'tramp-own-remote-path)

;; (after! lsp-mode
;; (lsp-register-client
;;     (make-lsp-client :new-connection (lsp-tramp-connection "pyright")
;;                      :major-modes '(python-mode)
;;                      :remote? t
;;                      :server-id 'pyls-remote)))
