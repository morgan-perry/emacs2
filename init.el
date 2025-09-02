;; Setup straight package manager
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq package-enable-at-startup nil)
(straight-use-package 'use-package)

(setq recentf-max-menu-items 50        ; keep 50 entries in the menu
      recentf-max-saved-items 100      ; save up to 100 entries
      recentf-auto-cleanup 'idle       ; clean up non-existent files when idle
      recentf-exclude '("\\.gz\\'"     ; don’t record .gz files
                         "/tmp/"        ; or files in /tmp/
                         "/ssh:"))      ; or remote ssh:
(add-hook 'after-init-hook #'recentf-mode)

(defvar evil-want-keybinding nil
  "Set to nil because evil-collection is used for keybindings.")

(global-visual-line-mode t)
(use-package multiple-cursors
  :straight t)

(use-package spacious-padding
  :straight t
  ;; Use the :custom keyword to set variables defined by the package.
  :custom
  ;; The value is a plist. We only need to specify the keys we want to change.
  (spacious-padding-widths
   '(
     :internal-border-width 40      ; Make the main side margins much larger
     :right-divider-width 1         ; Make window dividers invisible
     :fringe-width 15               ; Add a bit more space for fringe indicators
     :header-line-width 0           ; Remove header line padding
     :mode-line-width 4)))          ; Slightly smaller mode-line padding

(use-package doom-modeline
  :straight t
  :config
  (doom-modeline-mode))

;; (defun my-org-indent-maybe-h ()
;;   "Indent or cycle the Org list item at point.
;; Cycle visibility if it has sub-items.
;; If no sub-items, attempt to demote (indent); if that fails (e.g., first item or no previous sibling),
;; promote (outdent) instead to 'cycle' the indent levels.
;; Works anywhere on the current bullet line.
;; Suppresses error messages for boundary conditions."
;;   (interactive)
;;   (when (org-at-item-p)
;;     (save-excursion
;;       (org-beginning-of-item)
;;       (let ((struct (org-list-struct)))
;;         (if (org-list-has-child-p (point) struct)
;;             (let ((org-tab-first-hook (remq 'my-org-indent-maybe-h org-tab-first-hook)))
;;               (org-cycle))
;;           (condition-case nil
;;               (org-metaright)
;;             (user-error (org-metaleft))))))
;;     t))

;; (add-hook 'org-tab-first-hook #'my-org-indent-maybe-h
;; )

(use-package git-auto-commit-mode
  :straight t)

(use-package hl-todo
  :straight t
  :hook (prog-mode . hl-todo-mode)
  :config
  (setq hl-todo-highlight-punctuation ":"
        hl-todo-keyword-faces
        `(("TODO"       warning bold)
          ("FIXME"      error bold)
	  ("XXX"        error bold)
          ("HACK"       font-lock-constant-face bold)
          ("REVIEW"     font-lock-keyword-face bold)
          ("NOTE"       success bold)
          ("DEPRECATED" font-lock-doc-face bold))))

(use-package flycheck
  :straight t
  :hook (prog-mode . hl-todo-mode))

(use-package consult-flycheck
  :straight t)

(use-package consult-todo
  :straight t)

(defmacro on-linux (&rest body)
  "Execute BODY only when running on a Linux system."
  `(when (eq system-type 'gnu/linux)
     ,@body))

(defmacro on-windows (&rest body)
  "Execute BODY only when running on a Windows system."
  `(when (eq system-type 'windows-nt)
     ,@body))

;; This adds all elisp files from the modules and lisp folder
(mapc
 (lambda (string)
   (add-to-list 'load-path (locate-user-emacs-file string)))
 '("modules" "cloutlu-lisp"))

;;  This loads the extra lisp I provide for some extra functionality
(require 'cloutlu-narrow)
(require 'cloutlu-org-extra)
(require 'cloutlu-core-helpers)

(require 'cloutlu-bind) ;; This must go first as other package rely on general for binding
(require 'cloutlu-vertico)
(require 'cloutlu-which-key)
(require 'cloutlu-essentials)
(require 'cloutlu-dired)
(require 'cloutlu-tempel)
(require 'cloutlu-odin)
(require 'cloutlu-c)
(require 'cloutlu-eat)
(require 'cloutlu-corfu)
(on-linux (require 'cloutlu-jinx))
(on-linux (require 'cloutlu-pdf))
(require 'cloutlu-org)
;; (when (window-system)
;;   (require 'cloutlu-zen))
(if (window-system) (require 'cloutlu-zen) (xterm-mouse-mode t))

;; Make native compilation silent and prune its cache.
(when (native-comp-available-p)
  (setq native-comp-async-report-warnings-errors 'silent) ; Emacs 28 with native compilation
  (setq native-compile-prune-cache t)) ; Emacs 29

;; Don't generate backups or lockfiles. While auto-save maintains a copy so long
;; as a buffer is unsaved, backups create copies once, when the file is first
;; written, and never again until it is killed and reopened. This is better
;; suited to version control, and I don't want world-readable copies of
;; potentially sensitive material floating around our filesystem.

(defvar cloutlu-cache-dir nil
  "My cache directory, if linux it should
  be like XDG home dir/..., windows can just deal with it being messy")
(defvar cloutlu-data-dir nil
  "My data directory, if linux it should be like XDG home dir/..., windows can just deal with it being messy")
(defvar cloutlu-state-dir nil
  "My state directory, if linux it should be like XDG home dir/..., windows can just deal with it being messy")

(setq cloutlu-cache-dir "~/.local/cache"
      cloutlu-data-dir  "~/.local/data"
      cloutlu-state-dir "~/.local/state")

(setq create-lockfiles t
      make-backup-files t
      ;; But in case the user does enable it, some sensible defaults:
      version-control t     ; number each backup file
      backup-by-copying t   ; instead of renaming current file (clobbers links)
      delete-old-versions t ; clean up after itself
      kept-old-versions 5
      kept-new-versions 5
      backup-directory-alist (list (cons "." (concat cloutlu-cache-dir "backup/")))
      tramp-backup-directory-alist backup-directory-alist)

;; Disable the damn thing by making it disposable.
(setq custom-file (make-temp-file "emacs-custom-"))

(setq initial-buffer-choice nil)
(setq initial-major-mode 'lisp-interaction-mode)
(setq initial-scratch-message
      (format ";; This is `%s'.  Use `%s' to evaluate and print results.\n\n"
              'lisp-interaction-mode
              (propertize
               (substitute-command-keys "\\<lisp-interaction-mode-map>\\[eval-print-last-sexp]")
               'face 'help-key-binding)))

(use-package magit
  :straight t
  :general
  (:states 'normal
	   :prefix "SPC"
	   "g g" #'magit))

(use-package kkp
  :straight t
  :config
  (global-kkp-mode +1))

;; Custom macro to define system-specific bookmarks
(defmacro define-system-bookmarks (system &rest bookmarks)
  "Define bookmarks for SYSTEM ('gnu/linux or 'windows-nt) with NAMES and PATHS.
Each bookmark in BOOKMARKS is a (NAME PATH) pair. Skips non-existent files and logs them."
  `(when (eq system-type ,system)
     ,@(mapcar
        (lambda (bookmark)
          (let ((name (car bookmark))
                (path (cadr bookmark)))
            `(let ((expanded-path (expand-file-name ,path)))
               (if (file-exists-p expanded-path)
                   (bookmark-store ,name `((filename . ,expanded-path)) nil)
                 (message "Skipping bookmark '%s': File '%s' does not exist" ,name ,path)))))
        bookmarks)))

;; Ensure bookmark package is loaded
(use-package bookmark
  :straight t
  :config
  ;; Enable bookmark persistence
  (setq bookmark-save-flag 1) ;; Save bookmarks after each change
  ;; Use system-specific bookmark files to avoid conflicts
  (on-linux
   (setq bookmark-default-file (concat startup--xdg-config-home-emacs "bookmarks-linux.el")))
  (on-windows
   (setq bookmark-default-file "~/.config/emacs/bookmarks-windows.el"))
  ;; Define system-specific bookmarks
  (define-system-bookmarks 'gnu/linux
    ("notes" "~/org/notes.org")
    ("uni" "~/org/uni.org")
    ("conf" "~/.config/emacs/me.org"))
  (define-system-bookmarks 'windows-nt
    ("notes" "C:/Users/moogly/org/notes.org")
    ("uni" "C:/Users/moogly/org/uni.org")
    ("conf" "~/.config/emacs/me.org")
    ("Downloads" "C:/Users/moogly/Downloads/")
    ("dev" "D:/dev")))
