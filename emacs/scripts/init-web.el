(use-package web-mode
  :mode ("\\.html?\\'"))


(use-package js2-mode
  :mode ("\\.js\\'")
  :interpreter ("node" . js2-mode))

(use-package css-mode
  :mode "\\.css\\'"
  :config
  (custom-set-variables
   '(css-indent-offset 2)))

(provide 'init-web)

