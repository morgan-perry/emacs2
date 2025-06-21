;;; cloutlu-zen.el --- Distraction-free writing environment -*- lexical-binding: t; -*-

;;;; Variables

(defvar cloutlu-zen-mixed-pitch-modes '(adoc-mode rst-mode markdown-mode org-mode)
  "Major modes in which to enable `mixed-pitch-mode` when zen mode is active.")

(defvar cloutlu-zen-text-scale 2
  "The text-scaling level for zen mode.")

;;;; Helper Functions (Translated from Doom's config)

(defun cloutlu-zen--enable-text-scaling-h ()
  "Hook to apply text scaling when entering/leaving writeroom-mode."
  (when (/= cloutlu-zen-text-scale 0)
    (text-scale-set (if writeroom-mode cloutlu-zen-text-scale 0))
    ;; visual-fill-column is not part of your config, so we omit the adjust call
    ))

(defun cloutlu-zen--enable-mixed-pitch-h ()
  "Hook to enable `mixed-pitch-mode` in specified major modes."
  (when (apply #'derived-mode-p cloutlu-zen-mixed-pitch-modes)
    (mixed-pitch-mode (if writeroom-mode +1 -1))))

;;;; Package Configurations

(use-package writeroom-mode
  :straight t
  :commands (writeroom-mode cloutlu/toggle-zen-mode))

;; We use `after!` from your cloutlu-core-helpers.el to ensure writeroom-mode
;; is loaded before we configure it.
(after! writeroom-mode
  ;; Make writeroom-mode buffer-local, not global.
  (setq writeroom-global-effects nil)
  (setq writeroom-maximize-window nil)

  ;; Add our custom text-scaling hook.
  (add-hook 'writeroom-mode-hook #'cloutlu-zen--enable-text-scaling-h))

(use-package mixed-pitch
  :straight t
  ;; This hook will call our helper function when writeroom-mode is toggled.
  :hook (writeroom-mode . cloutlu-zen--enable-mixed-pitch-h)
  :config
  ;; Add faces that should remain fixed-pitch even when mixed-pitch is on.
  (dolist (face
           '(solaire-line-number-face ; For solaire-mode, if you use it
             org-date
             org-footnote
             org-special-keyword
             org-property-value
             org-ref-cite-face
             org-tag
             org-todo
             org-done
             font-lock-comment-face))
    (add-to-list 'mixed-pitch-fixed-pitch-faces face)))

;;;; User-Facing Command

(defun cloutlu/toggle-zen-mode ()
  "Toggle a distraction-free 'zen' writing environment."
  (interactive)
  (writeroom-mode 'toggle))


(provide 'cloutlu-zen)
;;; cloutlu-zen.el ends here
