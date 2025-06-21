;;; cloutlu-org-extra.el --- Extra helper functions for Org mode -*- lexical-binding: t; -*-

(defun cloutlu/org--get-foldlevel ()
  "Get the current maximum fold level in the visible window."
  (let ((max 1))
    (save-restriction
      (narrow-to-region (window-start) (window-end))
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (org-next-visible-heading 1)
          (when (memq (get-char-property (line-end-position)
                                         'invisible)
                      '(outline org-fold-outline))
            (let ((level (org-outline-level)))
              (when (> level max)
                (setq max level))))))
      max)))

(defun cloutlu/org-show-next-fold-level (&optional count)
  "Decrease the fold-level of the visible area of the buffer.
This unfolds another level of headings on each invocation.
With COUNT, unfold that many levels."
  (interactive "p")
  (let ((new-level (+ (cloutlu/org--get-foldlevel) (or count 1))))
    (outline-hide-sublevels new-level)
    (message "Folded to level %s" new-level)))

(defun cloutlu/org-hide-next-fold-level (&optional count)
  "Increase the global fold-level of the visible area of the buffer.
This folds another level of headings on each invocation.
With COUNT, fold that many levels."
  (interactive "p")
  (let ((new-level (max 1 (- (cloutlu/org--get-foldlevel) (or count 1)))))
    (outline-hide-sublevels new-level)
    (message "Folded to level %s" new-level)))

(defun cloutlu/org-open-all-folds (&optional level)
  "Open all folds in the buffer (or up to LEVEL)."
  (interactive "P")
  (if (integerp level)
      (outline-hide-sublevels level)
    (outline-show-all)))

(defun cloutlu/org-close-all-folds (&optional level)
  "Close all folds in the buffer (or to LEVEL)."
  (interactive "p")
  (outline-hide-sublevels (or level 1)))

(defun cloutlu/org-open-fold ()
  "Open the current fold (but not its children)."
  (interactive)
  (let ((org-cycle-subtree-status 'subtree))
    (org-cycle-internal-local)))

(defun cloutlu/org-close-fold ()
  "Close the current fold."
  (interactive)
  (outline-hide-subtree))

(provide 'cloutlu-org-extra)
;;; cloutlu-org-extra.el ends here
