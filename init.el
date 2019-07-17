;;; init.el --- my emacs config for fastiter

;; early-init.el before 27.01

(load
 (expand-file-name "early-init.el" user-emacs-directory))

;;;; fastiter

(prog1 "fi-setup"
  (straight-use-package
   '(fi :type git :host github
	    :repo "leotaku/fi-emacs"
	    :files ("*.el")))
  (straight-use-package 'leaf)
  (straight-use-package 'leaf-keywords)
  ;; (require 'bk-block)
  (require 'fi-subr)
  (require 'fi-auto)
  (require 'fi-config)
  (require 'fi-helpers)
  (require 'leaf)
  (leaf-keywords-init))

;; configuration dependencies

(leaf benchmark-init
  :straight t
  :require benchmark-init-modes)

(leaf no-littering
  :straight t
  :require no-littering
  :config
  (setq custom-file (no-littering-expand-etc-file-name "custom.el"))
  (setq auto-save-file-name-transforms
	`((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

(leaf dependencies
  :straight crux)

(leaf keyfreq
  :straight t
  :config (keyfreq-mode) (keyfreq-autosave-mode))

;; emacs configuration

;; (leaf configuration
;;   :load "lisp/basics.el")
;; (leaf visual
;;   :load "lisp/visual.el")
;; (leaf usability
;;   :load "lisp/usability.el")
;; (leaf major
;;   :load "lisp/major.el")

(load-file (expand-file-name "lisp/basics.el" user-emacs-directory))
(load-file (expand-file-name "lisp/visual.el" user-emacs-directory))
(load-file (expand-file-name "lisp/usability.el" user-emacs-directory))
(load-file (expand-file-name "lisp/major.el" user-emacs-directory))

;; keytheme

(leaf theist-mode
  :straight (theist-mode :type git :host github
                         :repo "leotaku/theist-mode"))

(leaf keytheme
  :config (load-file (expand-file-name "lisp/keytheme.el" user-emacs-directory))
  :custom
  (viper-mode . nil))

;; simple keybindings

(leaf sensible-keys
  :bind (("A-j" . next-line)
         ("A-k" . previous-line))
  :config
  (fi-configure-gui
   (keyboard-translate ?\C-i ?\H-i)
   (keyboard-translate ?\C-m ?\H-m))
  (leaf-key "ESC" (kbd "C-g") 'key-translation-map))

(leaf bad-habits
  :bind (("<XF86Forward>" . nil)
         ("<XF86Back>" . nil)
         ("<prior>" . nil)
         ("<next>" . nil)))

(leaf misc-bindings
  :bind (("C-x r" . revert-buffer)
         ("C-x f" . find-file)
         ("C-x e" . eval-defun)
         ("C-x s" . save-buffer)))

(leaf window-management
  :straight ace-window
  :bind (("C-x o" . ace-window)
         ("C-x c" . make-frame)
         ("C-x j" . delete-other-windows)
         ("C-x d" . kill-buffer)
         ("C-x k" . delete-window-or-frame))
  :config
  (defun delete-window-or-frame ()
    (interactive)
    (unless (ignore-errors (delete-window) t)
      (unless (ignore-errors (delete-frame) t)
        (save-buffers-kill-emacs)))))

;; tweaks

(leaf kill-emacs
  :config
  (defun warn-kill-emacs (func &rest args)
    "Whitelist kill-emacs from being run interactively."
    (if (equal this-command 'kill-emacs)
        (message "Fuck you!")
      (apply func args)))
  (advice-add 'kill-emacs :around 'warn-kill-emacs))

(leaf sensible-errors
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

;; magit

(leaf magit
  :straight t magit-todos
  :leaf-defer t
  :bind (("C-x g" . magit-status)
         (:magit-status-mode-map
          :package magit
          ("<return>" . magit-diff-visit-file-other-window)
          ("j" . magit-next-line)
          ("k" . magit-previous-line)
          ("v" . magit-mark)
          ("C-k" . magit-discard)))
  :config (magit-todos-mode)
  :config
  (defun magit-mark ()
    (interactive)
    (if (region-active-p)
        (deactivate-mark)
      (set-mark-command nil))))
