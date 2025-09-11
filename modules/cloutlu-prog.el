(use-package prog-mode
  :hook (prog-mode . display-line-numbers-mode)
  :hook (prog-mode . hs-minor-mode)
  :hook (prog-mode . eldoc-box-hover-mode)
  :hook (prog-mode . (lambda () (when (featurep 'jinx) (require 'cloutlu-tempel))))
  ;; :hook (prog-mode . eglot-ensure)
  :general
  (:keymaps 'prog-mode-map
	    :states '(normal insert)
	    "C-b" #'project-recompile
	    "M-B" #'project-compile
	    "C-k" #'eglot-format
	    "M-o" #'delete-other-windows
	    "M-p" #'other-window
	    "M-j" (lambda () (interactive) (evil-next-visual-line 5))  ; Move down 5 lines  "C-u" #'evil-scroll-up
	    "M-k" (lambda () (interactive) (evil-previous-visual-line 5))  ; Move up 5 lines
	    "M-O" #'split-window-horizontally)
  (:keymaps 'prog-mode-map
   :states 'normal 
	   :prefix "SPC"
	   "p c" #'project-compile
	   "p d" #'project-dired
	   "p f" #'project-find-file))

(use-package eldoc-box
  :straight t
  :after odin)

(use-package glsl-mode
  :straight t
  :hook (glsl-mode . display-line-numbers-mode))

(provide 'cloutlu-prog)
