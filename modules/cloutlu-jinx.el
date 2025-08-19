;; Spell Checker
(use-package jinx
  :straight t
  :hook (emacs-startup . global-jinx-mode)
  :general
  ("M-$" #'jinx-correct)
  (:state 'normal
	  "C-;" #'jinx-correct)
  :config
  (setq jinx-languages "en-custom")) ;; From a custom dictionary

(provide 'cloutlu-jinx)
