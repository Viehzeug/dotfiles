(use-package elpy
      :init
      (elpy-enable)
      :config
      (setq elpy-rpc-python-command "/usr/local/bin/python3.6")
      (setq python-shell-interpreter "jupyter"
            python-shell-interpreter-args "console --simple-prompt")
      (setq python-shell-completion-native-enable nil)
      ;;(delete 'elpy-module-highlight-indentation elpy-modules)
      ;;(delete 'elpy-module-flymake elpy-modules)
      (setq elpy-rpc-backend "jedi"))

(provide 'init-python)
