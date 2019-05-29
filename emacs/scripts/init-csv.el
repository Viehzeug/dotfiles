(use-package csv-mode
  :config
  :mode "\\.[Cc][Ss][Vv]\\'"
  :init (setq csv-separators '("," ";" "|" " ")))

(provide 'init-csv)
