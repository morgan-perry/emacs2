(use-package cc-mode
  :ensure nil  ; Built-in package
  :hook
  (c-mode . display-line-numbers-mode)
  (c-mode . hs-minor-mode)
  (c-mode . eldoc-box-hover-mode)
  :hook (odin-mode . (lambda () (when (featurep 'jinx) (require 'cloutlu-tempel))))
  (c-mode . eglot-ensure)
  (c-mode . flycheck-mode)
  :general
  (:keymaps 'c-mode-map
	    :states '(normal insert)
	    "C-b" #'compile  ; Adapted from odin-run-project; use compile for C projects
	    "C-k" #'eglot-format
	    "M-o" #'delete-other-windows
	    "M-O" #'split-window-horizontally))
	    ;; :prefix "SPC"
	    ;; "s i" #'consult-eglot-symbols))

(provide 'cloutlu-c)
