(use-package general
  :straight t
  :after (evil evil-collection which-key)
  :demand t)

;; Non-leader bindings
(general-def '(normal visual)
  "C-u" #'evil-scroll-up
  "C-s" #'consult-line
  "C-M-s" #'consult-line-multi
  "-" #'save-buffer
  ;; REVIEW redo thsi
  "] d" #'flycheck-next-error
  "[ d" #'flycheck-previous-error)

(general-define-key
 :states '(normal motion)
 :keymaps 'override
 :prefix "SPC"
 :prefix-map 'cloutlu-leader-map)

;; Main leader definer
(general-create-definer cloutlu-leader-def
  :keymaps 'cloutlu-leader-map)

;; Define the general keybindings
(cloutlu-leader-def
  "SPC"   '("M-x" . execute-extended-command)
  ;; Misc
  "t z"   'cloutlu/toggle-zen-mode
  "`" 'evil-buffer
  "X" 'org-capture
  ;; Files
  "f"     (cons "files" (make-sparse-keymap))
  "f f"   'find-file
  "f d"   'dired
  "f r"   'consult-recent-file
  "/"     'consult-ripgrep
  "f F"   'consult-fd
  "o -"   'dired-jump
  ;; Help
  "h"       (cons "help" (make-sparse-keymap))
  "h v"     'describe-variable
  "h f"     'describe-function
  "h k"     'describe-key
  "h P"     'describe-package
  "h m"     'describe-mode
  "h K"     'describe-keymap
  "h x"     'describe-command
  
  "h t"   'consult-theme
  "h m"   'consult-man
  "h i"   'consult-info
  ;; Windows
  "w"       (cons "windows" (make-sparse-keymap))
  "w TAB"   'alternate-window
  "w b"     'switch-to-minibuffer-window
  "w q"     'delete-window
  "w o"     'delete-other-windows
  "w h"     'evil-window-left
  "w j"     'evil-window-down
  "w k"     'evil-window-up
  "w l"     'evil-window-right
  "w v"     'split-window-horizontally
  "w s"     'split-window-vertically
  ;; Buffers/Bookmarks
  "b"   (cons "buffers/bookmarks" (make-sparse-keymap))
  "b b" 'consult-buffer
  "p b" 'consult-project-buffer
  "RET" 'consult-bookmark
  "b r" 'revert-buffer
  "b q" 'kill-buffer
  ;; Search
  "s"   (cons "search" (make-sparse-keymap))
  "s s" 'consult-line
  "s i" 'consult-outline
  "s g" 'consult-git-grep
  "s l" 'consult-focus-lines
  "s t" 'consult-todo
  "s f" 'consult-flycheck
  )

(use-package evil
  :straight t
  :demand t
  :init
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-respect-visual-line-mode t)
  (setq evil-move-line-on-visual-line t)
  (setq evil-want-fine-undo t)
  (setq undo-limit 80000000)
  (setq evil-undo-system 'undo-redo)
  :config
  (evil-mode 1))

(use-package evil-collection
  :straight t
  :after evil
  :config
  (evil-collection-init))

(provide 'cloutlu-bind)
