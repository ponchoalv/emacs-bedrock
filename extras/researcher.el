;;; Contents:
;;;
;;;  - Citation Management
;;;  - Authoring
;;;  - Note Taking: Org-roam
;;;  - Note Taking: Denote

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Critical variables
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; These variables must be set for Citar to work properly!

(setopt citar-bibliography '("~/refs.bib")) ; paths to your bibtex files

;;; These variables are needed for Denote
                                        ;(setopt denote-directory (expand-file-name "~/Docs/denote-notes/"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Citation Management
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package citar
  :defer t
  :ensure t
  :bind (("C-c b" . citar-insert-citation)
         :map minibuffer-local-map
         ("M-b" . citar-insert-preset))
  :custom
  ;; Allows you to customize what citar-open does
  (citar-file-open-functions '(("html" . citar-file-open-external)
                               ;; ("pdf" . citar-file-open-external)
                               (t . find-file))))

;; Optional: if you have the embark package installed, enable the ability to act
;; on citations with Citar by invoking `embark-act'.
                                        ;(use-package citar-embark
                                        ;  :after citar embark
                                        ;  :diminish ""
                                        ;  :no-require
                                        ;  :config (citar-embark-mode))

(use-package citar-org-roam
  :defer t
  :diminish ""
  ;; To get this to work both Citar *and* Org-roam have to have been used
  :after citar org-roam
  :no-require
  :config
  (citar-org-roam-mode)
  (setq citar-org-roam-note-title-template "${author} - ${title}\n#+filetags: ${tags}"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Authoring
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Note Taking: Org-roam
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package org-roam
  :defer t
  :ensure t
  :config
  ;; Make sure the backlinks buffer always shows up in a side window
  (add-to-list 'display-buffer-alist
               '("\\*org-roam\\*"
                 (display-buffer-in-side-window)
                 (side . right)
                 (window-width . 0.4)
                 (window-height . fit-window-to-buffer)))

  (org-roam-db-autosync-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Note Taking: Denote
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package denote
  :defer t
  :ensure t
  :config
  ;; Accept any symbol in a .dir-locals.el file; makes it easier to use silos.
  ;; See "silos" in the manual: https://protesilaos.com/emacs/denote
  (put 'denote-file-type 'safe-local-variable-p 'symbolp)
  )
