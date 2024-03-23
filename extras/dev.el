;;; Contents:
;;;
;;;  - Built-in config for developers
;;;  - Version Control
;;;  - Common file types
;;;  - Eglot, the built-in LSP client for Emacs

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Built-in config for developers
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package emacs
  :defer t
  :config
  ;; Treesitter config

  ;; Tell Emacs to prefer the treesitter mode
  ;; You'll want to run the command `M-x treesit-install-language-grammar' before editing.
  (setq major-mode-remap-alist
        '((yaml-mode . yaml-ts-mode)
          (bash-mode . bash-ts-mode)
          (js2-mode . js-ts-mode)
          (typescript-mode . typescript-ts-mode)
          (json-mode . json-ts-mode)
          (css-mode . css-ts-mode)
          (python-mode . python-ts-mode)))
  :hook
  ;; Auto parenthesis matching
  ((prog-mode . electric-pair-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Version Control
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Magit: best Git client to ever exist
(use-package magit
  :defer t
  :ensure t
  :bind (("C-x g" . magit-status)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Common file types
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package yaml-mode
  :defer t
  :ensure t)

(use-package json-mode
  :defer t
  :ensure t)

(use-package terraform-ts-mode
  :defer t
  :ensure t
  :quelpa (terraform-ts-mode
           :fetcher github
           :repo "kgrotel/terraform-ts-mode"))
  ;; clone it from https://github.com/kgrotel/terraform-ts-mode
  ;; :load-path "~/paquetes/terraform-ts-mode")

(use-package markdown-mode
  :defer t
  :ensure t
  :hook ((markdown-mode . visual-line-mode))
  :config
  (add-to-list 'eglot-server-programs '(markdown-mode . ("marksman"))))

;; Emacs ships with a lot of popular programming language modes. If it's not
;; built in, you're almost certain to find a mode for the language you're
;; looking for with a quick Internet search.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Eglot, the built-in LSP client for Emacs
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Helpful resources:
;;
;;  - https://www.masteringemacs.org/article/seamlessly-merge-multiple-documentation-sources-eldoc

(use-package eglot
  :defer t
  ;; no :ensure t here because it's built-in

  ;; Configure hooks to automatically turn-on eglot for selected modes
  :hook
  (((python-mode json-mode json-ts-mode) . eglot))

  :custom
  (eglot-send-changes-idle-time 0.1)
  (eglot-extend-to-xref t)              ; activate Eglot in referenced non-project files

  :config
  (fset #'jsonrpc--log-event #'ignore)  ; massive perf boost---don't log every event
  ;; Sometimes you need to tell Eglot where to find the language server
  ; (add-to-list 'eglot-server-programs
                                        ;              '(haskell-mode . ("haskell-language-server-wrapper" "--lsp")))
  )


;; Preview via `grip'
;; Install: pip install grip
(use-package grip-mode
  :defer t
  :ensure t
  :defines org-mode-map
  :bind (("C-c p" . grip-mode))
  :init
  (with-eval-after-load 'org
    (bind-key "C-c C-g" #'grip-mode org-mode-map))

  (setq grip-update-after-change nil)
  (when-let ((credential (auth-source-user-and-password "api.github.com")))
    (setq grip-github-user (car credential)
          grip-github-password (cadr credential))))

;; Persistent the scratch buffer
(use-package persistent-scratch
  :defer t
  :ensure t
  :diminish
  :bind (:map persistent-scratch-mode-map
         ([remap kill-buffer] . (lambda (&rest _)
                                  (interactive)
                                  (user-error "Scratch buffer cannot be killed")))
         ([remap revert-buffer] . persistent-scratch-restore)
         ([remap revert-this-buffer] . persistent-scratch-restore))
  :hook ((after-init . persistent-scratch-autosave-mode)
         (lisp-interaction-mode . persistent-scratch-mode))
  :init (setq persistent-scratch-backup-file-name-format "%Y-%m-%d"
              persistent-scratch-backup-directory
              (expand-file-name "persistent-scratch" user-emacs-directory)))

;; load golang configs
(load-file (expand-file-name "extras/go.el" user-emacs-directory))

;; load elisp config
(load-file (expand-file-name "extras/elisp.el" user-emacs-directory))

;; lsp in org-babel files
(load-file (expand-file-name "extras/lsp-org.el" user-emacs-directory))
