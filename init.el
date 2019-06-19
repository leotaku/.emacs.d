;;; init.el --- my emacs config for iterfast

;; run early configuration

(push (expand-file-name "etc" user-emacs-directory) load-path)
(require 'early-init)
(require 'boilerplate-init)

;; fastiter

(use-package fi
  :straight (fi :type git :host github
                :repo "leotaku/fi-emacs"))

;; se(use-package fi :straight (fi :type git :host github :repo "leotaku/fi-emacs")) ()ngs

(use-config objed
  :straight t
  :leaf-defer nil
  :bind (("M-l" . objed-line-object)
         (:objed-map
          ("j" . objed-next-line)
          ("k" . objed-previous-line)
          ("n" . objed-kill)
          ("p" . objed-toggle-side)))
  :init
  ;; (setq objed-cmd-alist nil)
  :config
  (defun objed-vr-replace ()
    (interactive)
    (save-excursion
      (setf (point) (objed--beg))
      (setf (mark) (objed--end))
      (call-interactively 'vr/replace)))
  (defun objed-vr-mark ()
    (interactive)
    (save-excursion
      (setf (point) (objed--beg))
      (setf (mark) (objed--end))
      (call-interactively 'vr/mc-mark)))
  (defun objed-goto-line (arg)
    (interactive "NLine number: ")
    (goto-line arg)
    (objed-activate 'line))
  (defun objed-switch-into-line (char)
    (interactive (list (read-char "char: " t)))
    (avy-with objed-switch-into-line
      (avy-jump
       (if (= 13 char)
           "\n"
         (regexp-quote (string char)))
       :window-flip nil
       :beg (objed--beg)
       :end (objed--end)
       :action (lambda (char)
                 (goto-char char)
                 (objed-quit)
                 (objed-activate 'char)))))
  (defun objed-swiper ()
    (interactive)
    (ignore-errors (swiper))
    (objed-activate 'line))
  (setq objed-mode-map (make-sparse-keymap))
  (objed-mode 1))

(use-config sensible-keys
  :bind (("A-j" . next-line)
         ("A-k" . previous-line))
  :config
  (keyboard-translate ?\C-i ?\H-i)
  (keyboard-translate ?\C-m ?\H-m)
  (leaf-key "ESC" (kbd "C-g") 'key-translation-map))

(use-config keyfreq
  :straight t
  :require t
  :config
  (keyfreq-mode 1))

(use-config cursor
  :setq-default
  (cursor-type . '(bar . 2)))

(use-config bad-habits
  :bind (("<XF86Forward>" . nil)
         ("<XF86Back>" . nil)
         ;; ("<up>" . nil)
         ;; ("<down>" . nil)
         ;; ("<left>" . nil)
         ;; ("<right>" . nil)
         ))

(use-config kill-emacs
  :config
  (defun warn-kill-emacs (func &rest args)
    "Whitelist kill-emacs from being run interactively."
    (if (equal this-command 'kill-emacs)
        (message "Fuck you!")
      (apply func args)))
  (advice-add 'kill-emacs :around 'warn-kill-emacs))

(use-config yes-or-no-query
  :config
  (fset 'yes-or-no-p 'y-or-n-p))

(use-config backups
  :custom
  (backup-by-copying . t)
  (delete-old-versions . t)
  (kept-new-versions . 6)
  (kept-old-versions . 2)
  (version-control . t))

(use-config truncate-lines
  :setq-default
  (truncate-lines . t))

(use-config show-paren
  :custom
  (show-paren-delay . 0)
  :config
  (show-paren-mode 1))

(use-config tabs
  :setq-default
  (indent-tabs-mode . nil)
  (tab-width . 4))

(use-config sensible-errors
  :custom
  (command-error-function . 'named-error-function)
  :config
  (defun named-error-function (data context caller)
    (discard-input)
    (ding)
    (minibuffer-message
     "%s%s"
     (if caller (format "%s: " caller) "")
     (error-message-string data))))

(use-package help
  :bind ((:help-mode-map
          ("j" . next-line)
          ("k" . previous-line)))
  :custom
  (help-window-select . t))

(use-config window-management
  :bind (("C-x c" . make-frame)
         ("C-x x" . delete-frame)))

(use-package ace-window
  :straight t
  :bind (("C-x o" . ace-window)))

(use-package dired
  :bind ((:dired-mode-map
          ("j" . next-line)
          ("k" . previous-line)
          ("s" . swiper)
          ("DEL" . dired-up-directory))))

;; bindings

(use-config revert
  :bind (("C-c r" . revert-buffer)))

(use-config file-access
  :bind (("C-x f" . find-file)))

(use-package savehist
  :config
  (savehist-mode 1))

(use-package recentf
  :bind (("C-x l" . counsel-recentf))
  :custom
  (recentf-max-saved-items . 4000)
  (recentf-max-menu-items . 1000)
  :config
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory)
  (add-hook 'find-file-hook 'recentf-save-list))

;; languages

(use-package nix-mode
  :straight t
  :mode "\\.nix\\'"
  :custom
  (nix-indent-function . 'nix-indent-line))

(use-package rust-mode
  :straight t
  :mode "\\.rs\\'")

(use-package toml-mode
  :straight t
  :mode "\\.toml\\'")

(use-package markdown-mode
  :straight t
  :mode "\\.md\\'" "\\.markdown\\'")

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
  (emacs-lisp-mode-hook . lispy-mode)
  (emacs-lisp-mode-hook . aggressive-indent-mode))

(use-package common-lisp-mode
  :mode ("\\.cl\\'" "\\.lisp\\'")
  :hook
  (lisp-mode-hook . lispy-mode)
  (lisp-mode-hook . aggressive-indent-mode)
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
;;   :custom
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
  :custom
  (inferior-lisp-program . "sbcl"))

(use-package lispy
  :straight t
  :hook ((minibuffer-setup-hook . conditionally-enable-lispy))
  :config
  (defun conditionally-enable-lispy ()
    (when (eq this-command 'eval-expression)
      (lispy-mode 1))))

(use-package aggressive-indent
  :straight t)

(use-package pcre2el
  :straight t)

(use-package visual-regexp-steroids
  :straight t
  :leaf-defer nil
  :require t
  :bind (("C-r" . vr/replace)
         ("H-m" . vr/mc-mark))
  :custom
  (vr/engine . 'pcre2el))

(use-package multiple-cursors
  :straight t
  :after visual-regexp-steroids)

(use-package flycheck
  :straight t
  :commands flycheck-mode)

(use-package flycheck-aspell
  :straight (flycheck-aspell :type git :host github
                             :repo "leotaku/flycheck-aspell")
  :after flycheck
  :init
  (straight-use-package 'async)
  :config
  (add-to-list 'flycheck-checkers 'tex-aspell-dynamic))

(use-package ispell
  :bind (("C-." . ispell-word))
  :init
  (setq ispell-dictionary "en_US"
        ispell-program-name "aspell"
        ispell-really-hunspell nil
        ispell-silently-savep t) 
  :config
  (advice-add
   'ispell-pdict-save :after 'flycheck-maybe-recheck)
  (defun flycheck-maybe-recheck (_)
    (when (bound-and-true-p flycheck-mode)
      (flycheck-buffer))))

(use-package expand-region
  :straight t
  :bind (("M-m" . er/expand-region)
         ("M-n" . er/contract-region)))

(use-package el2org
  :straight t)

(use-package ox-gfm
  :straight t
  :after el2org)

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
  :straight t
  :leaf-defer nil
  :custom
  (projectile-completion-system . 'ivy)
  (projectile-project-root-files-functions . '(projectile-root-top-down))
  (projectile-project-root-files . '(".git" ".bzr" ".svn" ".hg" "_darcs" ".projectile"))
  :config
  (projectile-mode 1)
  (define-key global-map (kbd "C-x p") projectile-command-map))

(use-package counsel-projectile
  :straight t
  :after projectile
  :config
  (counsel-projectile-mode))

(use-package company
  :straight t
  :require t
  :bind (:company-active-map
         ("RET" . nil)
         ("<return>" . nil)
         ("C-h" . nil))
  :custom
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

(use-package magit
  :straight t
  :bind (("C-x g" . magit-status)
         (:magit-status-mode-map
          ("<return>" . magit-diff-visit-file-other-window)
          ("j" . magit-next-line)
          ("k" . magit-previous-line)
          ("v" . set-mark-command)
          ("C-k" . magit-discard))))

(use-package amx
  :straight t
  :config
  (amx-mode 1))

(use-package undohist
  :straight t
  :require t
  :custom
  `(undohist-directory . ,(no-littering-expand-var-file-name "undohist"))
  :config
  (undohist-initialize))

(use-package solarized-theme
  :straight t
  :init
  (add-hook
   'emacs-startup-hook
   (lambda ()
     (load-theme 'solarized-light t)
     (custom-theme-set-faces
      'solarized-light
      '(region ((t . (:inherit hl-line)))))))
  :config
  (let ((line (face-attribute 'mode-line :underline)))
    (set-face-attribute 'mode-line          nil :overline   line)
    (set-face-attribute 'mode-line-inactive nil :overline   line)
    (set-face-attribute 'mode-line-inactive nil :underline  line)
    (set-face-attribute 'mode-line          nil :box        nil)
    (set-face-attribute 'mode-line-inactive nil :box        nil)
    (set-face-attribute 'mode-line-inactive nil :background "#f9f2d9")))

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
