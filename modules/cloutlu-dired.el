  ;;; Dired file manager and prot-dired.el extras (not actually lol)
(use-package dired
  :ensure nil
  :commands (dired)
  :config
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq delete-by-moving-to-trash t))

(provide 'cloutlu-dired)
