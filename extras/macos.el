;; .-..-. .--.  .--.        .--.  .--.    .--.  .--. .-..-..---. .-. .--.
;; : `' :: .; :: .--'      : ,. :: .--'  : .--': ,. :: `: :: .--': :: .--'
;; : .. ::    :: :   _____ : :: :`. `.   : :   : :: :: .` :: `;  : :: : _
;; : :; :: :: :: :__:_____:: :; : _`, :  : :__ : :; :: :. :: :   : :: :; :
;; :_;:_;:_;:_;`.__.'      `.__.'`.__.'  `.__.'`.__.':_;:_;:_;   :_;`.__.'
;;
;; Configuration for common MAC-OS shortcuts

(defun +macos/new-buffer ()
  "Create a new buffer"
  (interactive)
    (let ((buffer (generate-new-buffer "*new*")))
      (set-window-buffer nil buffer)
      (with-current-buffer buffer
        (funcall (default-value 'major-mode)))))

;; Some usefull macos key bindings
(use-package emacs
  :bind (("s-w" . delete-window)
         ("s-W" . delete-frame)
         ("s-n" . +macos/new-buffer)
         ("s-N" . make-frame)
         ("C-s-f" . toggle-frame-fullscreen)
         ("s-l" . goto-line)
         ("s-f" . consult-line)
         ("s-z" . undo)
         ("s-Z" . redo)
         ("s-c" . copy-region-as-kill)
         ("s-v" . yank)
         ("s-s" . save-buffer)
         ("s-x" . execute-extended-command)
         ("s-a" . mark-whole-buffer)
         ("s-/" . (lambda () (interactive) (save-excursion (comment-line 1))))))

;;; Reasonable defaults for macOS

;; Use spotlight search backend as a default for M-x locate (and helm/ivy
;; variants thereof), since it requires no additional setup.
;; (setq locate-command (executable-find "mdfind"))

;;
;;; Compatibilty fixes

;; Curse Lion and its sudden but inevitable fullscreen mode!
;; This is meaningless to railwaycat's emacs-mac build though.
(setq ns-use-native-fullscreen nil)

;; Visit files opened outside of Emacs in existing frame, not a new one
(setq ns-pop-up-frames nil)

(defmacro pushnew! (place &rest values)
  "Push VALUES sequentially into PLACE, if they aren't already present.
This is a variadic `cl-pushnew'."
  (let ((var (make-symbol "result")))
    `(dolist (,var (list ,@values) (with-no-warnings ,place))
       (cl-pushnew ,var ,place :test #'equal))))

(use-package auth-source
  :config
  (pushnew! auth-sources 'macos-keychain-internet 'macos-keychain-generic))
