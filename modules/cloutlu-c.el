;; (use-package c-mode
;;   :ensure nil  ; Built-in package
;;   :hook
;;   (c-mode . display-line-numbers-mode)
;;   (c-mode . hs-minor-mode)
;;   (c-mode . eldoc-box-hover-mode)
;;   :hook (odin-mode . (lambda () (when (featurep 'jinx) (require 'cloutlu-tempel))))
;;   ;; (c-mode . eglot-ensure)
;;   (c-mode . flycheck-mode)
;;   :general
;;   (:keymaps 'c-mode-map
;; 	    :states '(normal insert)
;; 	    "C-b" #'compile  ; Adapted from odin-run-project; use compile for C projects
;; 	    "C-k" #'eglot-format
;; 	    "M-o" #'delete-other-windows
;; 	    "M-p" #'other-window
;; 	    "M-j" (lambda () (interactive) (evil-next-visual-line 5))  ; Move down 5 lines  "C-u" #'evil-scroll-up
;; 	    "M-k" (lambda () (interactive) (evil-previous-visual-line 5))  ; Move up 5 lines
;; 	    "M-O" #'split-window-horizontally))
;; 	    ;; :prefix "SPC"
;; 	    ;; "s i" #'consult-eglot-symbols))


;; (use-package c++-mode
;;   :ensure nil  ; Built-in package
;;   :hook
;;   (c++-mode . display-line-numbers-mode)
;;   (c++-mode . hs-minor-mode)
;;   (c++-mode . eldoc-box-hover-mode)
;;   :hook (odin-mode . (lambda () (when (featurep 'jinx) (require 'cloutlu-tempel))))
;;   ;; (c++-mode . eglot-ensure)
;;   (c++-mode . flycheck-mode)
;;   :general
;;   (:keymaps 'c++-mode-map
;; 	    :states '(normal insert)
;; 	    "C-b" #'compile  ; Adapted from odin-run-project; use compile for C projects
;; 	    "C-k" #'eglot-format
;; 	    "M-o" #'delete-other-windows
;; 	    "M-p" #'other-window
;; 	    "M-j" (lambda () (interactive) (evil-next-visual-line 5))  ; Move down 5 lines  "C-u" #'evil-scroll-up
;; 	    "M-k" (lambda () (interactive) (evil-previous-visual-line 5))  ; Move up 5 lines
;; 	    "M-O" #'split-window-horizontally))
;; 	    ;; :prefix "SPC"
;; 	    ;; "s i" #'consult-eglot-symbols))

(provide 'cloutlu-c)
