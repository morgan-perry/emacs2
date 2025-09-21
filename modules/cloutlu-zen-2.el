;;; cloutlu-zen-2.el --- Standalone Doom-like zen mode -*- lexical-binding: t; -*-

(require 'cl-lib)  ; For cl-letf, etc.

;;;; Package Dependencies
(use-package spacious-padding
  :straight t
  :custom
  (spacious-padding-widths
   '(:internal-border-width 30      ; Wide margins for centering
     :right-divider-width 1         ; Thin dividers
     :fringe-width 0                ; Hide fringes
     :header-line-width 0
     :mode-line-width 4
     :scroll-bar-width 0)))         ; Hide scrollbars

(use-package fontaine :straight t)  ; Assumes already loaded from essentials

(use-package writeroom-mode
  :straight t
  :custom
  (writeroom-global-effects nil)    ; Buffer-local only
  (writeroom-maximize-window nil))  ; Defer to custom fullscreen

(use-package mixed-pitch
  :straight t
  :hook (writeroom-mode . mixed-pitch-mode))  ; Variable-pitch for prose

;;;; Variables
(defvar cloutlu-zen-2--original-state nil
  "Alist storing original state before enabling zen-2 mode.")

(defvar cloutlu-zen-2-text-steps 1
  "Number of text scale steps for zen-2 mode (each step is ~1.2x; default: 1).")

(defvar cloutlu-zen-2-window-divider-size 20
  "Window border thickness in zen-2 mode.")

;;;; Core Toggle Function
(defun cloutlu/toggle-zen-2-mode (&optional fullscreen)
  "Toggle standalone zen-2 mode for distraction-free editing.
Centers buffer, hides UI, scales text, and uses variable-pitch.
With FULLSCREEN non-nil, also maximize frame and delete other windows."
  (interactive "P")
  (if writeroom-mode
      ;; Deactivate
      (progn
        (writeroom-mode -1)
        (mixed-pitch-mode -1)
        (spacious-padding-mode -1)
        (when (alist-get 'text-steps cloutlu-zen-2--original-state)
          (text-scale-set (alist-get 'text-steps cloutlu-zen-2--original-state))
          (when (zerop (alist-get 'text-steps cloutlu-zen-2--original-state))
            (text-scale-mode -1)))
        (when (alist-get 'window-divider-width cloutlu-zen-2--original-state)
          (set-window-parameter nil 'window-divider-width (alist-get 'window-divider-width cloutlu-zen-2--original-state)))
        (when fullscreen
          (toggle-frame-fullscreen))
        (setq cloutlu-zen-2--original-state nil)
        (message "Zen-2 mode disabled"))
    ;; Activate
    (unless cloutlu-zen-2--original-state
      (setq cloutlu-zen-2--original-state
            `((text-steps . ,(if (boundp 'text-scale-mode-amount)
                                 text-scale-mode-amount
                               0))
              (window-divider-width . ,(window-parameter nil 'window-divider-width)))))
    (writeroom-mode 1)
    (spacious-padding-mode 1)
    (text-scale-set cloutlu-zen-2-text-steps)
    (set-window-parameter nil 'window-divider-width cloutlu-zen-2-window-divider-size)
    (when fullscreen
      (delete-other-windows)
      (toggle-frame-fullscreen))
    (message "Zen-2 mode enabled%s" (if fullscreen " (fullscreen)" ""))))

;;;; Fullscreen Variant
(defun cloutlu/toggle-zen-2-fullscreen ()
  "Toggle zen-2 mode in fullscreen."
  (interactive)
  (cloutlu/toggle-zen-2-mode t))

;;;; Advice for Text Scaling (Optional: Adjusts margins if using visual-fill-column later)
(advice-add #'text-scale-adjust :after
            (lambda (&rest _)
              (when (fboundp 'visual-fill-column-adjust)
                (visual-fill-column-adjust))))

(provide 'cloutlu-zen-2)
;;; cloutlu-zen-2.el ends here
