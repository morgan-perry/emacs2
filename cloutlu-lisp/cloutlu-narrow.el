  ;;; cloutlu-narrow.el --- Indirect narrowing and widening functions -*- lexical-binding: t; -*-

(defvar cloutlu--narrowed-base-buffer nil
  "The original buffer that was indirectly narrowed.")

(defun cloutlu/narrow-buffer-indirectly (beg end)
  "Restrict editing in this buffer to the current region, indirectly.

  This recursively creates indirect clones of the current buffer so that the
  narrowing doesn't affect other windows displaying the same buffer. Call
  `cloutlu/widen-indirectly-narrowed-buffer' to undo it (incrementally)."
  (interactive (if (region-active-p)
                   (list (region-beginning) (region-end))
                 (list (line-beginning-position) (line-end-position))))
  (deactivate-mark)
  (let ((orig-buffer (current-buffer)))
    (with-current-buffer (switch-to-buffer (clone-indirect-buffer nil nil))
      (narrow-to-region beg end)
      (setq-local cloutlu--narrowed-base-buffer orig-buffer))))

(defun cloutlu/widen-indirectly-narrowed-buffer (&optional arg)
  "Widens narrowed buffers.

  This command will incrementally kill indirect buffers (under the assumption they
  were created by `cloutlu/narrow-buffer-indirectly') and switch to their base
  buffer.

  If ARG, then kill all indirect buffers, return the base buffer and widen it.

  If the current buffer is not an indirect buffer, it is `widen'ed."
  (interactive "P")
  (unless (buffer-narrowed-p)
    (user-error "Buffer isn't narrowed"))
  (let ((orig-buffer (current-buffer))
        (base-buffer cloutlu--narrowed-base-buffer))
    (cond ((or (not base-buffer)
               (not (buffer-live-p base-buffer)))
           (widen))
          (arg
           (let ((buffer orig-buffer)
                 (buffers-to-kill (list orig-buffer)))
             (while (setq buffer (buffer-local-value 'cloutlu--narrowed-base-buffer buffer))
               (push buffer buffers-to-kill))
             (switch-to-buffer (buffer-base-buffer))
             (mapc #'kill-buffer (remove (current-buffer) buffers-to-kill))))
          ((switch-to-buffer base-buffer)
           (kill-buffer orig-buffer)))))

(defun cloutlu/smart-narrow ()
  "Narrow intelligently based on the current mode.
  In Org mode, narrow to the current subtree.
  Otherwise, use `cloutlu/narrow-buffer-indirectly` on the
  current region or line."
  (interactive)
  (if (derived-mode-p 'org-mode)
      (org-narrow-to-subtree)
    (call-interactively #'cloutlu/narrow-buffer-indirectly)))

(provide 'cloutlu-narrow)
  ;;; cloutlu-narrow.el ends here
