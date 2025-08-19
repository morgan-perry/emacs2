(use-package tempel
  :straight t
  :custom
  (tempel-path (concat user-emacs-directory "tempels"))
  :init
  ;; Setup completion at point
  (defun tempel-setup-capf ()
    ;; Add the Tempel Capf to `completion-at-point-functions'.
    ;; `tempel-expand' only triggers on exact matches. Alternatively use
    ;; `tempel-complete' if you want to see all matches, but then you
    ;; should also configure `tempel-trigger-prefix', such that Tempel
    ;; does not trigger too often when you don't expect it. NOTE: We add
    ;; `tempel-expand' *before* the main programming mode Capf, such
    ;; that it will be tried first.
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))
  :general
  ("M-*" #'tempel-insert)
  (:states '(normal insert emacs)
	   "M-+" #'tempel-complete))

(provide 'cloutlu-tempel)
