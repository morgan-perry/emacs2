;;; cloutlu-zen.el --- Distraction-free writing environment -*- lexical-binding: t; -*-

;;;; Package Dependencies
;; We declare the packages this module depends on directly within it.

(use-package spacious-padding
  :straight t
  :custom
  ;; These settings are chosen to mimic the default appearance of writeroom-mode.
  (spacious-padding-widths
   '(:internal-border-width 30  ; Creates wide side margins, like writeroom's border
     :right-divider-width 1     ; Makes vertical window dividers nearly invisible
     :fringe-width 10           ; A reasonable default for fringe indicators
     :header-line-width 0       ; No extra padding on the header line
     :mode-line-width 4         ; A tight, clean mode-line padding
     :scroll-bar-width 0)))     ; Ensure no space is allocated for scroll bars

(use-package fontaine :straight t)

;;;; Variables

(defvar cloutlu-zen-mode-active-p nil
  "A boolean to track if Zen mode is currently active.")

(defvar cloutlu-zen--original-font-preset nil
  "Internal variable to store the font preset before activating Zen mode.")

(defvar cloutlu-zen--original-mode-line-format nil
  "Internal variable to store the modeline format before activating Zen mode.")

(defvar cloutlu-zen-font-preset 'large
  "The font preset to use when activating Zen mode.
You can change this to 'presentation or any other preset you have defined.")

(defvar cloutlu-zen-mode-line-format
  '(:eval (propertize " %b %* " 'face 'mode-line))
  "The minimalist modeline format to use in Zen mode.
Set to nil to hide the modeline completely.")

;;;; User-Facing Command

(defun cloutlu/toggle-zen-mode ()
  "Toggle a distraction-free 'zen' mode."
  (interactive)
  (if cloutlu-zen-mode-active-p
      ;; --- DEACTIVATE ZEN MODE ---
      (progn
        (spacious-padding-mode -1)
        ;; Restore the original font preset
        (when cloutlu-zen--original-font-preset
          (fontaine-set-preset cloutlu-zen--original-font-preset))
        ;; Restore the original modeline format
        (when cloutlu-zen--original-mode-line-format
          (setq-local mode-line-format cloutlu-zen--original-mode-line-format))
        (setq cloutlu-zen-mode-active-p nil)
        (message "Zen mode disabled"))
    ;; --- ACTIVATE ZEN MODE ---
    (progn
      ;; Store the current state before changing it
      (setq cloutlu-zen--original-font-preset fontaine-current-preset)
      (setq cloutlu-zen--original-mode-line-format mode-line-format)
      ;; Activate the Zen mode features
      (spacious-padding-mode 1)
      (fontaine-set-preset cloutlu-zen-font-preset)
      (setq-local mode-line-format cloutlu-zen-mode-line-format)
      (setq cloutlu-zen-mode-active-p t)
      (message "Zen mode enabled: using '%s' font preset" cloutlu-zen-font-preset))))

;;;; Finalization

(provide 'cloutlu-zen)
;;; cloutlu-zen.el ends here
