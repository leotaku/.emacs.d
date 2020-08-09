;;; init.el --- Emacs configuration -*- lexical-binding: t; -*-

;;; Setup:

;; Load `early-init.el' before Emacs 27.0

(unless (featurep 'early-init)
  (message "Early init: Emacs Version < 27.0")
  (load
   (expand-file-name "early-init.el" user-emacs-directory)))

;; Load `fi-emacs' dependencies

(prog1 "fi-setup"
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
         ("<C-return>" . open-line))
  :config
  (fi-with-gui
   (keyboard-translate ?\C-i ?\H-i)
   (keyboard-translate ?\C-m ?\H-m))
  (leaf-key "ESC" (kbd "C-g") 'key-translation-map))

(bk-block! bad-habits
  :bind (("<XF86Forward>" . nil)
         ("<XF86Back>" . nil)
         ("<prior>" . nil)
         ("<next>" . nil)))

(bk-block! misc-bindings
  :bind  (("C-x r" . revert-buffer)
          ("C-x f" . find-file)
          ("C-x e" . eval-defun)
          ("C-x s" . save-buffer))
  :bind* (("C-x i" . ibuffer)
          ("C-v" . yank)))

(bk-block! window-management
  :bind (("C-x q" . split-window-left)
         ("C-x w" . split-window-above)
         ("C-x o" . ace-window)
         ("C-x c" . make-frame)
         ("C-x j" . delete-other-windows)
         ("C-x d" . kill-buffer)
         ("C-x k" . delete-window-or-frame))
  :config
  (advice-add 'keyboard-quit :around 'advice-keyboard-quit)
  (defun advice-keyboard-quit (func)
    (let ((minibuffer (active-minibuffer-window)))
      (if minibuffer
          (minibuffer-keyboard-quit)
        (funcall func)))))

;; Small tweaks

(bk-block! kill-emacs
  :config
  (defun advice-kill-emacs (func &rest args)
    "Whitelist kill-emacs from being run interactively."
    (if (equal this-command 'kill-emacs)
        (let ((this-command nil))
          (message "Fuck you!"))
      (apply func args)))
  (advice-add 'kill-emacs :around 'advice-kill-emacs))

(bk-block! sensible-errors
  :custom
  (command-error-function . 'command-error-default-function)
  :config
  (defun named-error-function (data _context caller)
    (discard-input)
    (ding)
    (minibuffer-message
     "%s%s"
     (if caller (format "%s: " caller) "")
     (error-message-string data))))

(bk-block magit
  :wanted-by delayed-target
  :requires .magit .forge .theist-mode
  :bind (("C-x g" . magit-status)
         (:magit-status-mode-map
          :package magit
          ("<return>" . magit-diff-visit-file-other-window)
          ("j" . magit-next-line)
          ("k" . magit-previous-line)
          ("v" . magit-mark)
          ("x" . theist-C-x)
          ("C-k" . magit-discard)))
  :config
  (defun magit-mark ()
    (interactive)
    (if (region-active-p)
        (deactivate-mark)
      (set-mark-command nil))))

(bk-block* flymake
  :requires .flymake .help-at-pt
  :custom
  (help-at-pt-display-when-idle . t)
  :config
  (defun help-at-pt-maybe-display (&rest _)
    (display-local-help t)))

(bk-block notmuch
  :wanted-by delayed-target
  :requires .notmuch)

;; Execute `bk-block' system startup

(bk-block0 setup-initial-buffer
  :wanted-by gui-target
  :requires emacs-lisp-mode
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

(fi-with-gui
 (when (get-buffer "*Warnings*")
   (warn "Warnings were emitted during Emacs startup!")))

(provide 'init)

;;; init.el ends here
