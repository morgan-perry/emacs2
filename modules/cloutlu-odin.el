(straight-use-package
 '(odin-mode :host nil :type git :repo "https://git.sr.ht/~mgmarlow/odin-mode"))

(use-package odin-mode
  :hook (odin-mode . display-line-numbers-mode)
  :hook (odin-mode . hs-minor-mode)
  :hook (odin-mode . eldoc-box-hover-mode)
  :hook (odin-mode . (lambda () (when (featurep 'jinx) (require 'cloutlu-tempel))))
  :hook (odin-mode . eglot-ensure)
  :general
  (:keymaps 'odin-mode-map
	    :states '(normal insert)
	    "C-b" #'odin-run-project
	    "C-k" #'clang-format
	    "M-o" #'delete-other-windows))

(use-package eldoc-box
  :straight t
  :after odin)

(use-package glsl-mode
  :straight t
  :hook (glsl-mode . display-line-numbers-mode))

(provide 'cloutlu-odin)
