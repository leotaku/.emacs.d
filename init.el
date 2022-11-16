;;; init.el --- Emacs configuration -*- lexical-binding: t; -*-

;;; Setup:

;; Load `early-init.el' before Emacs 27.0

(unless (featurep 'early-init)
  (message "Early init: Emacs Version < 27.0")
  (load
   (expand-file-name "early-init.el" user-emacs-directory)))

;;; Configuration:

(bk-block no-littering
  :requires .no-littering
  :custom
  (create-lockfiles . nil)
  (auto-save-file-name-transforms
   . `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

(bk-block custom-pre
  :requires .no-littering
  :custom
  (custom-file . (no-littering-expand-etc-file-name "custom.el"))
  :config
  (when (file-exists-p custom-file)
    (load-file custom-file)))

(bk-block* keyfreq
  :config
  (keyfreq-mode)
  (keyfreq-autosave-mode))

;; Load configuration files

(bk-block loads
  :load "lisp/helpers.el"
  :load "lisp/visual.el"
  :load "lisp/basics.el"
  :load "lisp/usability.el"
  :load "lisp/keytheme.el"
  :load "lisp/ide.el"
  :load "lisp/major.el"
  :load "lisp/org-cfg.el"
  :load "lisp/special.el")

;; Execute some simple keybinds

(bk-block sensible-keys
  :bind (("A-j" . next-line)
         ("A-k" . previous-line)
         ("<C-return>" . open-line))
  :config
  (fi-with-gui
   (define-key input-decode-map [?\C-i] [C-i])
   (define-key input-decode-map [?\C-m] [C-m]))
  (define-key key-translation-map (kbd "ESC") (kbd "C-g")))

(bk-block bad-habits
  :bind (("<XF86Forward>" . nil)
         ("<XF86Back>" . nil)
         ("<prior>" . nil)
         ("<next>" . nil)
         ("<insert>" . nil)
         ("C-<prior>" . nil)
         ("C-<next>" . nil))
  :bind (("C-z" . nil)
         ("C-x C-z" . nil)
         ("C-x C-b" . nil)))

(bk-block misc-bindings
  :bind  (("C-x r" . revert-buffer)
          ("C-x f" . find-file)
          ("C-x e" . eval-defun)
          ("C-x s" . save-buffer))
  :bind* (("C-x i" . ibuffer)
          ("C-v" . yank)))

(bk-block window-management
  :requires .ace-window
  :bind (("C-x q" . split-window-left)
         ("C-x w" . split-window-above)
         ("C-x o" . ace-window)
         ("C-x j" . delete-other-windows)
         ("C-x d" . kill-buffer)
         ("C-x k" . delete-window-or-frame))
  :custom (aw-scope . 'visible)
  :config
  (advice-add 'keyboard-quit :around #'advice-keyboard-quit))

(defun advice-keyboard-quit (func)
  (let ((minibuffer (active-minibuffer-window)))
    (if minibuffer
        (with-selected-window minibuffer
          (minibuffer-keyboard-quit))
      (funcall func))))

;; Small tweaks

(bk-block0 local-files
  :at-load
  (defun expand-sync-file (name)
    (expand-file-name name sync-directory))
  :custom
  (sync-directory . "~/sync")
  (todo-file . (expand-sync-file "homework.org"))
  (things-file . (expand-sync-file "things.org"))
  (journal-file . (expand-sync-file "journal.org"))
  (archive-file . (expand-sync-file "archive.org"))
  (diary-file . (expand-sync-file "diary"))
  :custom
  (org-agenda-files . (list todo-file things-file journal-file))
  (org-archive-location . (concat archive-file "::* %s")))

;; Run Emacs startup

(bk-block setup-initial-buffer
  :requires emacs-lisp-mode
  :at-load
  (setq initial-major-mode 'text-mode)
  :config
  (fi-with-gui
   (with-current-buffer "*scratch*"
     (emacs-lisp-mode))))

(elpaca nil
  (bk-register-target 'default-target)
  (bk-reach-target 'default-target)
  (fi-with-gui
   (when (get-buffer "*Warnings*")
     (warn "Warnings were emitted during Emacs startup!"))))

(provide 'init)

;;; init.el ends here
