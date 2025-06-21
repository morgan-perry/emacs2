(use-package general
  :straight t
  :after (evil evil-collection which-key)
  :demand t)

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
  "h t"   'consult-theme
  "t z"   'cloutlu/toggle-zen-mode

;; Files
"f"     (cons "files" (make-sparse-keymap))
"f f"   'find-file
"f d"   'dired
"/"     'consult-ripgrep
"f F"   'consult-fd

;; Help
"h"       (cons "help" (make-sparse-keymap))
"h v"     'describe-variable
"h f"     'describe-function
"h k"     'describe-key
"h P"     'describe-package
"h m"     'describe-mode
"h K"     'describe-keymap
"h x"     'describe-command

;; Windows
"w"       (cons "windows" (make-sparse-keymap))
"w TAB"   'alternate-window
"w b"     'switch-to-minibuffer-window
"w d"     'delete-window
"w o"     'delete-other-windows
"w h"     'evil-window-left
"w j"     'evil-window-down
"w k"     'evil-window-up
"w l"     'evil-window-right
"w v"     'split-window-horizontally
"w s"     'split-window-vertically

;; Buffers/Bookmarks
"b"   (cons "buffers/bookmarks" (make-sparse-keymap))
"`"   'previous-buffer 
"b b" 'consult-buffer
"b B" 'consult-bookmark
"b r" 'revert-buffer

;; Search
"s"   (cons "search" (make-sparse-keymap))
"s s" 'consult-line)

(use-package evil
  :straight t
  :demand t
  :init
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-respect-visual-line-mode t)
  (setq evil-move-line-on-visual-line t)
  :config
  (evil-mode 1))

(use-package evil-collection
  :straight t
  :after evil
  :config
  (evil-collection-init))

(provide 'cloutlu-bind)
