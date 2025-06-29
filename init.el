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

(use-package magit
  :straight t
  :general
  (:states 'normal
	   :prefix "SPC"
	   "g g" #'magit))

;; Spell Checker
(use-package jinx
  :straight t
  :hook (emacs-startup . global-jinx-mode)
  :general
  ("M-$" #'jinx-correct)
  (:state 'normal
	  "C-;" #'jinx-correct)
  :config
  (setq jinx-languages "en-custom")) ;; From a custom dictionary

(use-package git-auto-commit-mode
  :straight t)
