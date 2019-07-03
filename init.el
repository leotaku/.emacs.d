;;; init.el --- my emacs config for fastiter

;; run early configuration

(push (expand-file-name "etc" user-emacs-directory) load-path)
(require 'early-init)
(require 'boilerplate-init)

;; fastiter

(use-package fi
  :straight (fi :type git :host github
                :repo "leotaku/fi-emacs")
  :leaf-defer nil
  :require fi-auto fi-subr fi-config fi-helpers)

(use-package deps
  :straight crux)

;; simple keybindings

(use-config sensible-keys
  :bind (("A-j" . next-line)
         ("A-k" . previous-line))
  :config
  (fi-configure-gui
   (keyboard-translate ?\C-i ?\H-i)
   (keyboard-translate ?\C-m ?\H-m))
  (leaf-key "ESC" (kbd "C-g") 'key-translation-map))

(use-config bad-habits
  :bind (("<XF86Forward>" . nil)
         ("<XF86Back>" . nil)
         ;; ("<up>" . nil)
         ;; ("<down>" . nil)
         ;; ("<left>" . nil)
         ;; ("<right>" . nil)
         ))

(use-config misc-bindings
  :bind (("C-c r" . revert-buffer)
         ("C-x f" . find-file)))

(use-config window-management
  :straight ace-window
  :bind `(("C-x o" . ace-window)
          ("C-x c" . make-frame)
          ("C-x x" . delete-frame)
          ("C-x j" . delete-other-windows)
          ("C-x k" . ,(defun delete-window-or-frame ()
                        (interactive)
                        (unless (ignore-errors (delete-window) t)
                          (delete-frame))))))

(use-config kill-emacs
  :config
  (defun warn-kill-emacs (func &rest args)
    "Whitelist kill-emacs from being run interactively."
    (if (equal this-command 'kill-emacs)
        (message "Fuck you!")
      (apply func args)))
  (advice-add 'kill-emacs :around 'warn-kill-emacs))

(use-config sensible-errors
  :pre-setq
  (command-error-function . 'named-error-function)
  :config
  (defun named-error-function (data context caller)
    (discard-input)
    (ding)
    (minibuffer-message
     "%s%s"
     (if caller (format "%s: " caller) "")
     (error-message-string data))))

(use-package expand-region
  :straight t
  :bind (("M-m" . er/expand-region)
         ("M-n" . er/contract-region))
  :config
  (setq er/try-expand-list
        (fi-insert-after
         er/try-expand-list
         'er/mark-defun
         'er/mark-text-paragraph))
  :config
  (defun er/mark-line ()
    (interactive)
    (setf (point) (point-at-eol))
    (forward-char)
    (set-mark (point))
    (backward-char)
    (setf (point) (point-at-bol))))

(use-package avy
  :straight t
  :bind* (("C-a" . avy-goto-word-or-subword-1)))

(use-package multiple-cursors
  :straight t
  :bind* (("M-j" . mc/mark-next-lines)))

(use-config ide-backspace
  :bind ((:prog-mode-map
          :package emacs
          ("<backspace>" . ide-backspace)
          ("<C-backspace>" . ide-backspace-word))
         (:text-mode-map
          :package emacs
          ("<backspace>" . ide-backspace)
          ("<C-backspace>" . ide-backspace-word))
         (:lispy-mode-map
          :package lispy
          ("<backspace>" . lispy-delete-backward)))
  :config
  (defun ide-backspace ()
    (interactive)
    (cond
     ((region-active-p)
      (kill-region (region-beginning) (region-end)))
     ((looking-back "^[[:space:]]+")
      (ide-delete-to-previous-line))
     (t
      ;; delete char normally 
      (call-interactively 'backward-delete-char))))
  (defun ide-backspace-word ()
    (interactive)
    (cond
     ((looking-back "^[[:space:]]+")
      (ide-delete-to-previous-line))
     (t
      ;; delete word normally
      (call-interactively 'backward-kill-word))))
  (defun ide-delete-to-previous-line ()
    ;; delete all spaces
    (while (not (looking-back "[\n]"))
      (delete-char -1))
    ;; delete final newline
    (delete-char -1)
    ;; go to indentation
    (when (looking-back "[\n]")
      (indent-according-to-mode))))

(use-package smartparens
  :straight t
  :hook (prog-mode-hook . smartparens-mode)
  :config
  (require 'smartparens-config)
  (defun ide-insert-newlines (&rest _)
    (newline)
    (indent-according-to-mode)
    (forward-line -1)
    (indent-according-to-mode))
  (defun ide-insert-spaces (&rest _)
    (insert " ")
    (backward-char))
  (dolist (paren-type '("(" "[" "{"))
    (sp-local-pair
     'prog-mode paren-type nil
     :post-handlers '((ide-insert-newlines "RET")
                      (ide-insert-spaces "SPC")))))

;; settings

(use-config cursor
  :setq-default
  (cursor-type . '(bar . 2)))

(use-config yes-or-no-query
  :config
  (fset 'yes-or-no-p 'y-or-n-p))

(use-config vc
  :pre-setq
  (vc-follow-symlinks . t))

(use-config backups
  :pre-setq
  (backup-by-copying . t)
  (delete-old-versions . t)
  (kept-new-versions . 6)
  (kept-old-versions . 2)
  (version-control . t))

(use-config truncate-lines
  :setq-default
  (truncate-lines . t))

(use-config show-paren
  :pre-setq
  (show-paren-delay . 0)
  :config
  (show-paren-mode 1))

(use-config tabs
  :setq-default
  (indent-tabs-mode . nil)
  (tab-width . 4))

;; builtin modes

(use-package savehist
  :config
  (savehist-mode 1))

(use-package help
  :bind ((:help-mode-map
          ("j" . next-line)
          ("k" . previous-line)))
  :pre-setq
  (help-window-select . t))

(use-package recentf
  :bind (("C-x l" . counsel-recentf))
  :leaf-defer nil
  :require t
  :pre-setq
  (recentf-max-saved-items . 4000)
  (recentf-max-menu-items . 1000)
  :config
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory))

(use-package dired
  :bind ((:dired-mode-map
          ("j" . next-line)
          ("k" . previous-line)
          ("s" . swiper)
          ("DEL" . dired-up-directory))))

;; major modes

(use-package nix-mode
  :straight t
  :mode "\\.nix\\'"
  :pre-setq
  (nix-indent-function . 'nix-indent-line))

(use-package lua-mode
  :straight t
  :mode "\\.lua\\'")

(use-package rust-mode
  :straight t
  :mode "\\.rs\\'")

(use-package toml-mode
  :straight t
  :mode "\\.toml\\'")

(use-package markdown-mode
  :straight t
  :mode (("\\.md\\'" "\\.markdown\\'") . gfm-mode))

(use-package tex
  :straight (auctex :type git :host github
                    :repo "emacs-straight/auctex")
  :mode ("\\.tex\\'" . TeX-mode)
  :config
  (TeX-PDF-mode)
  (TeX-source-correlate-mode)
  
  (setq TeX-view-program-selection
        (list '(output-pdf "Zathura")))
  
  (add-to-list
   'TeX-expand-list
   '("%sn" (lambda () server-name)))
  
  (setq TeX-view-program-list
        (list '("Zathura"
                ("zathura %o"
                 (mode-io-correlate " --synctex-forward %n:0:%b -x \"emacsclient --socket-name=%sn --no-wait +%{line} %{input}\""))
                "zathura")))

  (defun TeX-view ()
    "Start a viewer without confirmation.
The viewer is started either on region or master file,
depending on the last command issued."
    (interactive)
    (let ((output-file (concat "out/" (TeX-active-master (TeX-output-extension)))))
      (if (file-exists-p output-file)
          (TeX-command
           "View"
           (lambda (&rest _)
             output-file)
           0)))))

(use-package emacs-lisp-mode
  :mode "\\.el\\'"
  :hook
  (emacs-lisp-mode-hook . lispy-mode))

(use-package common-lisp-mode
  :mode ("\\.cl\\'" "\\.lisp\\'")
  :hook
  (lisp-mode-hook . lispy-mode)
  :config
  (setq-mode-local
   lisp-mode lisp-indent-function
   'common-lisp-indent-function))

;; packages

;; FIXME: this is gabage

;; (use-package comint
;;   :bind (:comint-mode-map
;;          ("<up>" . comint-previous-input)
;;          ("<down>" . comint-next-input)))

;; (use-package ielm
;;   :commands ielm
;;   :hook (ielm-mode-hook . lispy-mode)
;;   :bind (:ielm-map
;;          ("<C-return>" . ielm-send-input))
;;   :pre-setq
;;   (ielm-dynamic-return . nil)
;;   :config
;;   (add-hook
;;    'ielm-mode-hook
;;    (lambda ()
;;      (leaf-key "<C-return>" 'ielm-send-input (current-local-map)))))

;; (use-package comint
;;   :fi-mode comint-mode
;;   :fi-parent (erc-mode sly-mrepl-mode)
;;   :fi-bind
;;   ("C-l" . comint-clear-buffer))

(use-package sly
  :straight t
  :commands sly
  :bind ((:sly-mrepl-mode-map
          ("C-l" . comint-clear-buffer)))
  :pre-setq
  (inferior-lisp-program . "sbcl"))

(use-package lispy
  :straight t aggressive-indent
  :hook ((minibuffer-setup-hook . conditionally-enable-lispy)
         (lispy-mode-hook . aggressive-indent-mode))
  :config
  (defun conditionally-enable-lispy ()
    (when (eq this-command 'eval-expression)
      (lispy-mode 1))))

(use-package ispell
  :bind (("C-." . ispell-word))
  :init
  (setq ispell-dictionary "en_US"
        ispell-program-name "aspell"
        ispell-really-hunspell nil
        ispell-silently-savep t))

(use-package which-key
  :straight t
  :config
  (which-key-mode 1))

(use-package swiper
  :straight t
  :bind ((:ivy-minibuffer-map
          ("H-i" . ivy-insert-selection)))
  :config
  (ivy-mode 1)
  (defun ivy-insert-selection ()
    (interactive)
    (ivy-exit-with-action
     (lambda (it)
       (interactive)
       (insert it)
       (signal 'quit nil)))))

(use-package counsel
  :straight t
  :after swiper
  :bind (("C-s" . swiper-isearch)
         (:counsel-describe-map
          ("C-h" . counsel-lookup-symbol)))
  :config
  (defun counsel-lookup-symbol ()
    "Lookup the current symbol in the help docs."
    (interactive)
    (ivy-exit-with-action
     (lambda (x)
       (if (featurep 'helpful)
           (helpful-symbol (intern x))
         (describe-symbol (intern x))
         (signal 'quit nil)))))
  (counsel-mode 1))

(use-package projectile
  :straight t counsel-projectile
  ;; FIXME: better autoloading
  :commands projectile-commander
  :pre-setq
  (projectile-completion-system . 'ivy)
  (projectile-project-root-files-functions . '(projectile-root-top-down))
  (projectile-project-root-files . '(".git" ".bzr" ".svn" ".hg" "_darcs" ".projectile"))
  :preface
  (fi-auto-keymap (kbd "C-x p") 'projectile-command-map 'projectile)
  :config
  (projectile-mode 1)
  (counsel-projectile-mode 1))

(use-package company
  :straight t
  :hook (prog-mode . company-mode)
  :bind (:company-active-map
         ("RET" . nil)
         ("<return>" . nil)
         ("C-h" . nil)
         ("<tab>" . company-complete-common-or-cycle)
         ("<backtab>" . company-select-previous))
  :pre-setq
  (company-minimum-prefix-length . 1)
  (company-idle-delay . 0.2)
  (company-dabbrev-downcase . nil) 
  (company-dabbrev-ignore-case . nil) 
  (company-require-match . nil)
  (company-tooltip-align-annotations . t)
  (company-frontends . '(company-tng-frontend
                         company-pseudo-tooltip-frontend
                         company-echo-metadata-frontend))
  :config
  (global-company-mode 1))

(use-package amx
  :straight t
  :config
  (amx-mode 1))

(use-package undohist
  :straight t
  :require t
  :pre-setq
  (undohist-ignored-files .  '("COMMIT_EDITMSG"))
  `(undohist-directory . ,(no-littering-expand-var-file-name "undohist"))
  :config
  (undohist-initialize))

(use-package yankpad
  :straight t
  :bind (("C-x y" . yankpad-insert)
         ("C-x Y" . yankpad-capture-snippet))
  :pre-setq
  `(yankpad-file . ,(expand-file-name "yankpad.org" "~")))

(use-package magit
  :straight t
  :bind (("C-x g" . magit-status)
         (:magit-status-mode-map
          ("<return>" . magit-diff-visit-file-other-window)
          ("j" . magit-next-line)
          ("k" . magit-previous-line)
          ("v" . magit-mark)
          ("C-k" . magit-discard)))
  :config
  (defun magit-mark ()
    (interactive)
    (if (region-active-p)
        (deactivate-mark)
      (set-mark-command nil))))

(use-package el2org
  :straight t ox-gfm)

(use-config sensible-gui
  :pre-setq
  (frame-resize-pixelwise . t))

(use-package solarized-theme
  :straight t
  :init
  (fi-configure-gui
   (load-theme 'solarized-light)))

(use-config mode-line-other
  :config
  (column-number-mode 1))

(use-package moody
  :straight t
  :config
  (setq x-underline-at-descent-line t)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))

(use-package minions
  :straight t
  :after moody
  :config
  (minions-mode 1))
