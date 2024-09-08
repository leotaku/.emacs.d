;;; init.el --- Emacs configuration -*- lexical-binding: t; -*-

;;; Setup:

;; Load Packages

(load-file (locate-user-emacs-file "load-packages.el"))

;;; Configuration:

(bk-block no-littering
  :requires .no-littering
  :custom
  (create-lockfiles . nil)
  (auto-save-file-name-transforms
   . `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  (backup-directory-alist
   . `((".*" . ,(no-littering-expand-var-file-name "backup/")))))

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

(bk-block misc-bindings
  :bind (("C-x m" . execute-extended-command)
         ("C-x r" . revert-buffer)
         ("C-x f" . find-file)
         ("C-x e" . eval-defun)
         ("C-x s" . save-buffer)
         ("C-v" . yank)))

(bk-block window-management
  :requires .ace-window
  :bind (("C-x q" . split-window-horizontally)
         ("C-x w" . split-window-vertically)
         ("C-x o" . ace-window)
         ("C-x j" . delete-other-windows)
         ("C-x d" . kill-buffer)
         ("C-x k" . delete-window-or-frame))
  :custom (aw-scope . 'frame)
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
  (sync-directory . "~/Sync")
  (todo-file . (expand-sync-file "homework.org"))
  (things-file . (expand-sync-file "things.org"))
  (journal-file . (expand-sync-file "journal.org"))
  (archive-file . (expand-sync-file "archive.org"))
  (phone-file . (expand-sync-file "phone.org"))
  :custom
  (org-agenda-files . (list journal-file phone-file todo-file things-file))
  (org-archive-location . (concat archive-file "::* %s")))

;; Run Emacs startup

(bk-block setup-initial-buffer
  :requires emacs-lisp-mode
  :at-load
  (setq initial-buffer-choice (expand-file-name "~/scratch.txt"))
  (setq initial-major-mode 'emacs-lisp-mode))

(unless (sd-access-unit 'default-target)
  (bk-register-target 'default-target)
  (bk-reach-target '.no-littering)
  (bk-reach-target 'default-target)
  (fi-with-gui
   (when (get-buffer "*Warnings*")
     (warn "Warnings were emitted during Emacs startup!"))))

(provide 'init)

;;; init.el ends here
