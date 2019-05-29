(use-package cheatsheet
  :bind ("C-<f1>" . cheatsheet-show)
  :config
  (cheatsheet-add :group 'Common
                  :key "C-x C-c"
                  :description "leave Emacs.")
  (cheatsheet-add :group 'Common
                  :key "C-_"
                  :description "undo")
  
  (cheatsheet-add :group 'Move
                  :key "C-f"
                  :description "forward char")
  (cheatsheet-add :group 'Move
                  :key "C-b"
                  :description "backward char")
  (cheatsheet-add :group 'Move
                  :key "M-f"
                  :description "forward word")
  (cheatsheet-add :group 'Move
                  :key "M-b"
                  :description "backward word")
  (cheatsheet-add :group 'Move
                  :key "C-n"
                  :description "forward line")
  (cheatsheet-add :group 'Move
                  :key "C-p"
                  :description "backward line")
  (cheatsheet-add :group 'Move
                  :key "M-e"
                  :description "forward sentence")
  (cheatsheet-add :group 'Move
                  :key "M-a"
                  :description "backward sentence")
  (cheatsheet-add :group 'Move
                  :key "C-M-f"
                  :description "forward expression")
  (cheatsheet-add :group 'Move
                  :key "C-M-b"
                  :description "backward expression")
  (cheatsheet-add :group 'Move
                  :key "M-}"
                  :description "forward paragraph")
  (cheatsheet-add :group 'Move
                  :key "M-{"
                  :description "backward paragraph")

  (cheatsheet-add :group 'Delete
                  :key "C-d"
                  :description "forward char")
  (cheatsheet-add :group 'Delete
                  :key "DEL"
                  :description "backward char")
  (cheatsheet-add :group 'Delete
                  :key "M-d"
                  :description "forward word")
  (cheatsheet-add :group 'Delete
                  :key "M-DEL"
                  :description "backward word (same as C-w)")
  (cheatsheet-add :group 'Delete
                  :key "C-k"
                  :description "forward line")  
  
  (cheatsheet-add :group 'Terminal
                  :key "C-w"
                  :description "cut to previous white space")
  (cheatsheet-add :group 'Terminal
                  :key "C-y"
                  :description "paste last cut text")
  (cheatsheet-add :group 'Terminal
                  :key "M-y"
                  :description "loop through last cut text")
  (cheatsheet-add :group 'Terminal
                  :key "C-r"
                  :description "search as you type (twice to search)")
  (cheatsheet-add :group 'Terminal
                  :key "C-j"
                  :description "end search at current history entry")
  
  (cheatsheet-add :group 'Tmux
                  :key "C-b s"
                  :description "list sessions")
  (cheatsheet-add :group 'Tmux
                  :key "C-b $"
                  :description "name session")
  (cheatsheet-add :group 'Tmux
                  :key "C-b c"
                  :description "create window (tab)")
  (cheatsheet-add :group 'Tmux
                  :key "C-b w"
                  :description "list windows (tabs)")
  (cheatsheet-add :group 'Tmux
                  :key "C-b n"
                  :description "next window (tab)")
  (cheatsheet-add :group 'Tmux
                  :key "C-b p"
                  :description "previous window (tab)")
  (cheatsheet-add :group 'Tmux
                  :key "C-b f"
                  :description "find window (tab)")
  (cheatsheet-add :group 'Tmux
                  :key "C-b ,"
                  :description "name window (tab)")
  (cheatsheet-add :group 'Tmux
                  :key "C-b &"
                  :description "kill window (tab)")
  (cheatsheet-add :group 'Tmux
                  :key "C-b %"
                  :description "vertical split")
  (cheatsheet-add :group 'Tmux
                  :key "C-b \""
                  :description "horizontal split")
  (cheatsheet-add :group 'Tmux
                  :key "C-b x"
                  :description "kill pane")
  (cheatsheet-add :group 'Tmux
                  :key "C-b <SPC>"
                  :description "toggle between layouts")
  (cheatsheet-add :group 'Tmux
                  :key "C-b d"
                  :description "detach")
  
  )

(provide 'init-cheatsheet)
