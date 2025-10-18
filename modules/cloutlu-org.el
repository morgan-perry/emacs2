(use-package org
  :straight nil
  :defer t
  :init
  (require 'org-tempo) ; Adds nice abbrevs like '<s' or '<q'
  (on-linux (setq org-directory (expand-file-name "~/org")))
  (on-windows (setq org-directory "C:/Users/moogly/org"))
  (setq org-fontify-whole-heading-line nil) ;; apparently speeds loading
  :config

  (setq org-archive-location (concat org-directory "/archive.org::")
	org-imenu-depth 7
        org-src-preserve-indentation t
	org-capture-bookmark nil
        org-pretty-entities t)
  (setq org-todo-keywords
        '((sequence
           "TODO(t)"  ; A task that needs doing & is ready to do
           "PROJ(p)"  ; A project, which usually contains other tasks
           "LOOP(r)"  ; A recurring task
           "STRT(s)"  ; A task that is in progress
           "WAIT(w)"  ; Something external is holding up this task
           "HOLD(h)"  ; This task is paused/on hold because of me
           "IDEA(i)"  ; An unconfirmed and unapproved task or notion
           "|"
           "DONE(d)"  ; Task successfully completed
           "KILL(k)") ; Task was cancelled, aborted, or is no longer applicable
          (sequence
           "[ ](T)"   ; A task that needs doing
           "[-](S)"   ; Task is in progress
           "[?](W)"   ; Task is being held up or paused
           "|"
           "[X](D)")  ; Task was completed
          (sequence
           "|"
           "OKAY(o)"
           "YES(y)"
           "NO(n)"))
        org-todo-keyword-faces
        '(("[-]"  . +org-todo-active)
          ("STRT" . +org-todo-active)
          ("[?]"  . +org-todo-onhold)
          ("WAIT" . +org-todo-onhold)
          ("HOLD" . +org-todo-onhold)
          ("PROJ" . +org-todo-project)
          ("NO"   . +org-todo-cancel)
          ("KILL" . +org-todo-cancel)))

  ;; (defun org-add-my-extra-fonts ()
  ;;   "Add custom font-lock keywords for question emphasis '?text?'."
  ;;   (add-to-list 'org-font-lock-extra-keywords
  ;; 		 '(;; This is the corrected regular expression for the '?' marker.
  ;; 		   ;; Note the extra '\\' before each '?' to escape it.
  ;; 		   "\\(\\?\\)\\([^\n\r\t]+?\\)\\(\\?\\)"

  ;; 		   ;; This part applies the faces.
  ;; 		   ;; I've used a generic 'warning' face as an example.
  ;; 		   ;; You can replace it with your own face, like 'my-org-question-face'.
  ;; 		   (1 '(face warning invisible t)) ; Group 1 (opening '?') is made invisible
  ;; 		   (2 'warning t)                  ; Group 2 (the text) gets the color
  ;; 		   (3 '(face warning invisible t))) ; Group 3 (closing '?') is made invisible
  ;; 		 t))

  ;; (add-hook 'org-font-lock-set-keywords-hook #'org-add-my-extra-fonts)

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
	    "o a m" #'org-attach-attach-mv
	    "o a c" #'org-attach-attach-cp

	    "o a r" #'org-refile
	    "o a R" #'org-refile-copy
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

(setq org-capture-templates
      `(("i" "Inbox" entry
         (file+headline
          ,(expand-file-name "notes.org" org-directory)
          "Inbox")
         "* [%<%Y-%m-%d %a>]  %?\n\n%i\n%a")))

(defun org-add-my-extra-fonts ()
  "Add custom font-lock keywords for question emphasis '?text?'."
  (add-to-list 'org-font-lock-extra-keywords
    '("\\(\\?\\)\\([^\n\r\t]+?\\)\\(\\?\\)"
      (1 '(face warning invisible t))
      (2 'warning t)
      (3 '(face warning invisible t)))
    t))

(add-hook 'org-font-lock-set-keywords-hook #'org-add-my-extra-fonts)

(straight-use-package '(org :type built-in))
(use-package org-modern
  :straight t
  :after org
  :hook (org-mode . org-modern-mode))

(use-package org-expose-emphasis-markers
  :straight t
  :config
  ;; 1. make sure `org-hide-emphasis-markers' is true
  (setq org-hide-emphasis-markers t)

  ;; 2. (optional) set the exposing scope, default value is 'item
  (setq org-expose-emphasis-markers-type 'item)

  ;; 3. turn on the mode
  (add-hook 'org-mode-hook (lambda () (org-expose-emphasis-markers-mode t))))

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
  :defer t
  :after org
  :commands (org-roam-node-find org-roam-node-insert org-roam-capture)
  :init
  (setq org-roam-directory (concat org-directory "/roam"))
  :config
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  :general
  (:states 'normal :prefix "SPC"
           "r f" #'org-roam-node-find
           "r i" #'org-roam-node-insert
           "r c" #'org-roam-capture)
  (:keymaps 'org-mode-map
            :states 'normal
            "TAB" #'org-roam-complete-link-at-point))

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
  :defer t
  :general
  (:states 'normal
	   :prefix "SPC"
	   "n b" #'citar-open-notes)
  
  :init
  (setq org-cite-insert-processor 'citar
        org-cite-follow-processor 'citar
        org-cite-activate-processor 'citar)
  :config
  (setq citar-bibliography `(,(concat org-directory "/ref.bib")))
  (setq citar-library-paths `(,(concat org-directory "/references")))
  (setq citar-notes-paths `(,(concat org-directory "/roam")))
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

;; Autoload all these commands that org-attach doesn't autoload itself
(use-package org-attach
  :commands (org-attach-delete-one
	     org-attach-delete-all
	     org-attach-new
	     org-attach-open
	     org-attach-open-in-emacs
	     org-attach-reveal-in-emacs
	     org-attach-url
	     org-attach-set-directory
	     org-attach-sync)
  :config
  ;; Centralized attachments directory by default
  (setq org-attach-id-dir (expand-file-name ".attach/" org-directory))

  (setq org-attach-store-link-p 'attached     ; store link after attaching files
	org-attach-use-inheritance t) ; inherit properties from parent nodes
  (after! projectile
    (add-to-list 'projectile-globally-ignored-directories org-attach-id-dir))

;; Add inline image previews for attachment links
(org-link-set-parameters "attachment" :image-data-fun #'+org-image-file-data-fn))

(use-package org-download
  :straight t
  :general
  (:keymaps 'org-mode-map
	    :states 'normal
	    :prefix "SPC"
	    "m a c" #'org-download-screenshot
	    "m a y" #'org-download-yank)
  :config
  (setq org-download-image-dir 'org-attach-dir
	org-download-method 'attach
	org-download-screenshot-method
	(cond ((executable-find "maim")  "maim -s %s")
              ((executable-find "scrot") "scrot -s %s")
              ((executable-find "gnome-screenshot") "gnome-screenshot -a -f %s"))
	org-download-heading-lvl nil
	org-download-link-format "[[download:%s]]\n"
	org-download-annotate-function (lambda (_link) "")
	org-download-link-format-function
	(lambda (filename)
          (if (eq org-download-method 'attach)
              (format "[[attachment:%s]]\n"
                      (org-link-escape
                       (file-relative-name filename (org-attach-dir))))
            ;; Handle non-image files a little differently. Images should be
            ;; inserted as normal with previews. Other files, like pdfs or zips,
            ;; should be linked to, with an icon indicating the type of file.
            (format (concat (unless (image-type-from-file-name filename)
                              (concat (+org-attach-icon-for filename)
                                      " "))
                            org-download-link-format)
                    (org-link-escape
                     (funcall org-download-abbreviate-filename-function filename)))))
        org-download-abbreviate-filename-function
	(lambda (path)
          (if (file-in-directory-p path org-download-image-dir)
              (file-relative-name path org-download-image-dir)
            path))))

(use-package org-cliplink
  :straight t
  :general
  (:keymaps 'org-mode-map
	    :states 'normal
	    :prefix "SPC"
	    "m l c" #'org-cliplink
	    "m l C" #'org-cliplink-capture))

(use-package org-ql
  :straight t)

(use-package org-roam-ql
  :after (org-ql org-roam)
  :straight t)

(org-link-set-parameters
 "youtube"
 :follow (lambda (path)
           (browse-url (concat "https://www.youtube.com/watch?v=" path)))
 :help-echo (lambda (window object position)
              (with-current-buffer (window-buffer window)
                (ignore-errors
                  (save-excursion
                    (goto-char position)
                    (concat "Open YouTube video: "
                            (org-link-unescape (cadr (org-element-context))))))))
 ;; :store #'org-yt-store-link  ; Optional: if using org-yt for storing links
 :export (lambda (path desc backend)
           (if (eq backend 'html)
               (format "<a href=\"https://www.youtube.com/watch?v=%s\">%s</a>"
                       path (or desc "YouTube Video"))
             (format "[[youtube:%s][%s]]" path (or desc "YouTube Video")))))

(use-package org-transclusion
  :straight t
  :after org
  :init
  :general
  (:states 'normal :prefix "SPC"
           "n t" #'org-transclusion-add))

(provide 'cloutlu-org)
