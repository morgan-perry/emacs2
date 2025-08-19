(use-package corfu
  :straight t
  ;; Optional customizations
  :config
  (setq corfu-auto t
        corfu-auto-delay 0.24
        corfu-auto-prefix 2
        global-corfu-modes
        '((not erc-mode
               circe-mode
               help-mode
               gud-mode
               vterm-mode)
          t)
        corfu-cycle t
        corfu-preselect 'prompt
        corfu-count 16
        corfu-max-width 120
        corfu-on-exact-match nil
        ;; corfu-quit-at-boundary (if (or (modulep! :completion vertico)
        ;;                                (modulep! +orderless))
        ;;                            'separator t)
        corfu-quit-no-match corfu-quit-at-boundary
        tab-always-indent 'complete

	text-mode-ispell-word-completion nil

	read-extended-command-predicate #'command-completion-default-include-p)
  :init
  (global-corfu-mode)
  ;; Enable optional extension modes:
  (corfu-history-mode)
  (corfu-popupinfo-mode))

(use-package cape
  :straight t
  :init
  (add-hook! 'completion-at-point-functions #'cape-dabbrev)
  (add-hook! 'completion-at-point-functions #'cape-file)
  (add-hook! 'completion-at-point-functions #'cape-elisp-block))

(provide 'cloutlu-corfu)
