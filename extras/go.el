;;; Commentary:
;;
;; Golang configurations.
;;

;;; Code:

;; Golang
(use-package go-mode
  :defer t
  :ensure t
  :functions (go-install-tools exec-path-from-shell-copy-envs)
  :autoload godoc-gogetdoc
  :bind (:map go-mode-map
              ("<f1>" . godoc))
  :hook ((go-mode . eglot-ensure)
         (go.mode . go-ts-mode))
  :init
  (setq godoc-at-point-function #'godoc-gogetdoc)

  ;; Install tools
  (defconst go--tools
    '("golang.org/x/tools/gopls"
      "golang.org/x/tools/cmd/goimports"
      "honnef.co/go/tools/cmd/staticcheck"
      "github.com/go-delve/delve/cmd/dlv"
      "github.com/zmb3/gogetdoc"
      "github.com/josharian/impl"
      "github.com/cweill/gotests/..."
      "github.com/fatih/gomodifytags"
      "github.com/davidrjenni/reftools/cmd/fillstruct")
    "All necessary go tools.")

  (defun go-install-tools ()
    "Install or update go tools."
    (interactive)
    (unless (executable-find "go")
      (user-error "Unable to find `go' in `exec-path'!"))

    (message "Installing go tools...")
    (dolist (pkg go--tools)
      (set-process-sentinel
       (start-process "go-tools" "*Go Tools*" "go" "install" "-v" "-x" (concat pkg "@latest"))
       (lambda (proc _)
         (let ((status (process-exit-status proc)))
           (if (= 0 status)
               (message "Installed %s" pkg)
             (message "Failed to install %s: %d" pkg status)))))))
  :config
  ;; Env vars
  (with-eval-after-load 'exec-path-from-shell
    (exec-path-from-shell-copy-envs '("GOPATH" "GO111MODULE" "GOPROXY")))

  ;; Try to install go tools if `gopls' is not found
  (unless (executable-find "gopls")
    (go-install-tools))

  ;; Misc
  (use-package go-dlv
    :ensure t)
  (use-package go-fill-struct
    :ensure t)
  (use-package go-impl
    :ensure t)

  (use-package go-tag
    :ensure t
    :bind (:map go-mode-map
           ("C-c c a" . go-tag-add)
           ("C-c c r" . go-tag-remove))
    :init (setq go-tag-args (list "-transform" "camelcase")))

  (use-package go-gen-test
    :ensure t
    :bind (:map go-mode-map
           ("C-c c g" . go-gen-test-dwim)))

  (use-package gotest
    :ensure t
    :bind (:map go-mode-map
           ("C-c c f" . go-test-current-file)
           ("C-c c t" . go-test-current-test)
           ("C-c c j" . go-test-current-project)
           ("C-c c b" . go-test-current-benchmark)
           ("C-c c c" . go-test-current-coverage)
           ("C-c c x" . go-run)))
  )

;; Local Golang playground for short snippets
(use-package go-playground
  :defer t
  :ensure t
  :commands go-playground-mode)

(provide 'go)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; go.el ends here
