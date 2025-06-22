(setq debug-on-warning t)
(use-package org
  :ensure nil
  :init
  (setq org-directory (expand-file-name "~/doc/share/org")
	org-archive-location (concat org-directory "/archive.org::")
	org-imenu-depth 7
	org-src-preserve-indentation t)
  ;; Use :general to define high-precedence, buffer-local keys for Org mode.
  ;; These keys do NOT use the SPC leader.
  :general

  ;; General <SPC s> heading
  (:keymaps 'org-mode-map
	    :states 'normal
	    :prefix "SPC"
	    "o a" (cons "org archiving/attachments" (make-sparse-keymap))
	    "o a A" #'org-archive-subtree-default
	    "o a a" #'org-attach
	    "o a d" #'org-attach-delete-one
	    "o a D" #'org-attach-delete-all
	    "o a n" #'org-attach-new
	    "o a o" #'org-attach-open
	    "o a O" #'org-attach-open-in-emacs
	    "o a r" #'org-attach-reveal
	    "o a R" #'org-attach-reveal-in-emacs
	    "o a u" #'org-attach-url
	    )

  ;; A standard Org binding
  (:states 'normal
	   :keymaps 'org-mode-map
	   [remap evil-open-folds]    #'cloutlu/org-hide-next-fold-level
	   [remap evil-close-folds]    #'cloutlu/org-show-next-fold-level
	   "z n" #'cloutlu/smart-narrow
	   "z N" #'cloutlu/widen-indirectly-narrowed-buffer
	   "z i" #'org-toggle-inline-images)
  (:states '(insert normal)
	   :keymaps 'org-mode-map
	   "C-<tab>" 'org-cycle-list-bullet)
)


(use-package evil-org
  :straight t
  :after (org evil)
  :hook (org-mode . evil-org-mode))

(straight-use-package '(org :type built-in))
(use-package org-modern
  :straight t
  :after org
  :config
  (global-org-modern-mode))

(use-package org-appear
  :straight t
  :after org
  :config
  (setq org-hide-emphasis-markers t
        org-appear-autoemphasis t
	org-appear-autolinks t
	org-appear-autoentities t
	org-appear-autosubmarkers t
	org-appear-autokeywords t
	org-appear-delay 0.75)
  (add-hook 'org-mode-hook 'org-appear-mode))

(use-package org-tidy
  :straight t
  :after org
  :general
  (:keymaps 'org-mode-map
	    :states 'normal
	    :prefix "SPC"
	    "t t" #'org-tidy-toggle)
  :hook
  (org-mode . org-tidy-mode))

(use-package org-roam
  :straight t
  :after org
  :general
  (:keymaps 'org-mode-map
	    :states 'normal
	    :prefix "SPC"
	    "n r"   (cons "org-roam" (make-sparse-keymap))
	    "n r f" #'org-roam-node-find
	    "n r i" #'org-roam-node-insert
	    "n r n" #'org-roam-capture
	    "n r r" #'org-roam-buffer-toggle
	    "n r R" #'org-roam-buffer-display-dedicated
	    "n r s" #'org-roam-db-sync)
  :custom
  (org-roam-directory (concat org-directory "/roam"))
  :config
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  )

(use-package nerd-icons
  :straight t
  ;; :custom
  ;; The Nerd Font you want to use in GUI
  ;; "Symbols Nerd Font Mono" is the default and is recommended
  ;; but you can use any other Nerd Font if you want
  ;; (nerd-icons-font-family "Symbols Nerd Font Mono")
  )

(use-package citar
  :straight t
  :general
  (:states 'normal
	   :prefix "SPC"
	   "n b" #'citar-open-notes)
  
  :init
  (setq org-cite-insert-processor 'citar
        org-cite-follow-processor 'citar
        org-cite-activate-processor 'citar)
  :config
  (setq citar-bibliography '("~/doc/share/org/ref.bib"))
  (setq citar-library-paths '("~/doc/share/org/references/"))
  (setq citar-notes-paths '("~/doc/share/org/roam"))
  (defvar citar-indicator-files-icons
    (citar-indicator-create
     :symbol (nerd-icons-faicon
              "nf-fa-file_o"
              :face 'nerd-icons-green
              :v-adjust -0.1)
     :function #'citar-has-files
     :padding "  " ; need this because the default padding is too low for these icons
     :tag "has:files"))
  (defvar citar-indicator-links-icons
    (citar-indicator-create
     :symbol (nerd-icons-faicon
              "nf-fa-link"
              :face 'nerd-icons-orange
              :v-adjust 0.01)
     :function #'citar-has-links
     :padding "  "
     :tag "has:links"))
  (defvar citar-indicator-notes-icons
    (citar-indicator-create
     :symbol (nerd-icons-codicon
              "nf-cod-note"
              :face 'nerd-icons-blue
              :v-adjust -0.3)
     :function #'citar-has-notes
     :padding "    "
     :tag "has:notes"))
  (defvar citar-indicator-cited-icons
    (citar-indicator-create
     :symbol (nerd-icons-faicon
              "nf-fa-circle_o"
              :face 'nerd-icon-green)
     :function #'citar-is-cited
     :padding "  "
     :tag "is:cited"))
  (setq citar-indicators
        (list citar-indicator-files-icons
              citar-indicator-links-icons
              citar-indicator-notes-icons
              citar-indicator-cited-icons)))

(use-package citar-org-roam
  :straight t
  :init
  (after! (citar org-roam)
    (citar-org-roam-mode)))

(after! oc
  (setq org-cite-global-bibliography
        (ensure-list
         (or (bound-and-true-p citar-bibliography)
             (bound-and-true-p bibtex-completion-bibliography)))
        ;; Setup export processor; default csl/citeproc-el, with biblatex for
        ;; latex
        org-cite-export-processors '((latex biblatex) (t csl))
        org-support-shift-select t)

  (require 'oc-biblatex))

;; oc-csl requires citeproc, which requires the top-level org, so loading oc-csl
;; after oc interferes with incremental loading of Org
(after! org (require 'oc-csl))
