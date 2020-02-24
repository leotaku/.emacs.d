;;; init.el --- my emacs config for fastiter -*- lexical-binding: t; -*-

;;; Commentary:

;; TODO: ivy sort by shortest
;; TODO: use compdef for local completion settings
;; TODO: investigate handle, hercules
;; TODO: use eglot, maybe request

;;; Setup:

;; Load `early-init.el' before Emacs 27.0

(unless (featurep 'early-init)
  (message "Early init: Emacs Version < 27.0")
  (load
   (expand-file-name "early-init.el" user-emacs-directory)))

;; Load `fi-emacs' dependencies

(prog1 "fi-setup"
  (require 'fi-subr)
  (require 'fi-auto)
  (require 'fi-config)
  (require 'fi-helpers)
  (require 'bk)
  (require 'leaf))

;;; Configuration:

(prog1 "no-littering"
  (require 'no-littering)
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

(bk-block benchmark-init
  :requires .benchmark-init-modes)

;; TODO: Do something about unwanted customization

(bk-block! custom-pre
  :config
  (setq custom-file
        (no-littering-expand-etc-file-name
         "custom.el"))
  (when (file-exists-p custom-file)
    (load-file custom-file)))

(bk-block! dependencies
  :load "lisp/helpers.el")

(bk-block* keyfreq
  :config
  (keyfreq-mode)
  (keyfreq-autosave-mode))

;; Load configuration files

(bk-block! loads
  :load "lisp/visual.el"
  :load "lisp/basics.el"
  :load "lisp/usability.el"
  :load "lisp/ide.el"
  :load "lisp/major.el"
  :load "lisp/org-cfg.el")

;; Load keytheme config

(bk-block! keytheme
  :load "lisp/keytheme.el"
  :custom
  (viper-mode . nil))

;; Execute some simple keybinds

(bk-block! sensible-keys
  :bind (("<insert>" . nil)
         ("H-m" . newline)
         ("A-j" . next-line)
         ("A-k" . previous-line)
         ("<C-return>" . open-line)
         ("C-x m" . counsel-M-x))
  :config
  (fi-configure-gui
   (keyboard-translate ?\C-i ?\H-i)
   (keyboard-translate ?\C-m ?\H-m))
  (leaf-key "ESC" (kbd "C-g") 'key-translation-map))

(bk-block! cua-keys
  :requires .cua-base
  :bind* (("C-v" . yank)))

(bk-block! bad-habits
  :bind (("<XF86Forward>" . nil)
         ("<XF86Back>" . nil)
         ("<prior>" . nil)
         ("<next>" . nil)))

(bk-block! misc-bindings
  :bind (("C-x r" . revert-buffer)
         ("C-x f" . find-file)
         ("C-x e" . eval-defun)
         ("C-x s" . save-buffer)))

(bk-block! window-management
  :bind (("C-x q" . split-window-left)
         ("C-x w" . split-window-above)
         ("C-x o" . ace-window)
         ("C-x c" . make-frame)
         ("C-x j" . delete-other-windows)
         ("C-x d" . kill-buffer)
         ("C-x k" . delete-window-or-frame)))

;; Small tweaks

(bk-block! kill-emacs
  :config
  (defun warn-kill-emacs (func &rest args)
    "Whitelist kill-emacs from being run interactively."
    (if (equal this-command 'kill-emacs)
        (let ((this-command nil))
          (message "Fuck you!"))
      (apply func args)))
  (advice-add 'kill-emacs :around 'warn-kill-emacs))

(bk-block! sensible-errors
  :custom
  (command-error-function . 'command-error-default-function)
  :config
  (defun named-error-function (data context caller)
    (discard-input)
    (ding)
    (minibuffer-message
     "%s%s"
     (if caller (format "%s: " caller) "")
     (error-message-string data))))

(bk-block magit
  :wanted-by delayed-target
  :requires .magit .hl-todo .magit-todos .forge
  :bind (("C-x g" . magit-status)
         (:magit-status-mode-map
          :package magit
          ("<return>" . magit-diff-visit-file-other-window)
          ("j" . magit-next-line)
          ("k" . magit-previous-line)
          ("v" . magit-mark)
          ("C-k" . magit-discard))
         (:magit-todos-section-map
          :package magit-todos
          ("j" . magit-next-line)
          ("k" . magit-previous-line)))
  :config
  (global-hl-todo-mode)
  (magit-todos-mode)
  :config
  (defun magit-mark ()
    (interactive)
    (if (region-active-p)
        (deactivate-mark)
      (set-mark-command nil))))

(bk-block* flymake
  :requires .flymake-diagnostic-at-point
  :config
  (setq flymake-diagnostic-at-point-display-diagnostic-function
        'flymake-diagnostic-at-point-display-minibuffer)
  (add-hook 'flymake-mode-hook #'flymake-diagnostic-at-point-mode))

(bk-block notmuch
  :wanted-by delayed-target
  :requires .notmuch)

(bk-block vterm
  :requires .vterm
  :bind ((:vterm-mode-map
          :package vterm
          ("q" . vterm--self-insert)
          ("<C-backspace>" . vterm-send-backspace))))

;; Execute `bk-block' system startup

(bk-block0 setup-initial-buffer
  :wanted-by gui-target
  :requires lispy .elisp-mode
  :at-load
  (setq initial-major-mode 'text-mode)
  :config
  (with-current-buffer "*scratch*"
    (emacs-lisp-mode)))

;; Register all targets

(bk-register-target 'init-target)
(bk-register-target 'theme-target)
(bk-register-target 'gui-target)
(bk-register-target 'delayed-target)

;; Run Emacs startup

(bk-reach-target 'init-target)
(bk-reach-target 'theme-target)
(run-with-timer
 1 nil
 (lambda ()
   (bk-poll-target
    'gui-target
    (lambda ()
      (bk-poll-target 'delayed-target)))))

(provide 'init)

;;; init.el ends here
