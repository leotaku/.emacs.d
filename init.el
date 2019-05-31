;; init.el --- my emacs config for iterfast

;; run early configuration

(push user-emacs-directory load-path)
(require 'early-init)
(require 'boilerplate-init)
(setq load-path (remove user-emacs-directory load-path))

;; early setup

(use-package recentf
  :bind (("C-x l" . counsel-recentf))
  :init
  (setq recentf-max-saved-items 200
	recentf-max-menu-items 80)
  :config
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory))

(use-package savehist
  :config
  (savehist-mode 1))

;;; fastiter

(use-package fi
  :straight (fi :type git :host github
		:repo "leotaku/fi-emacs"))

;;; settings

(named-progn cursor
  (setq-default cursor-type 'bar))

(named-progn yes-or-no-query
  (fset 'yes-or-no-p 'y-or-n-p))

(named-progn backups
  (setq backup-by-copying t
	delete-old-versions t
	kept-new-versions 6
	kept-old-versions 2
	version-control t))

(named-progn truncate-lines
  (setq-default truncate-lines t))

(named-progn show-paren
  (setq show-paren-delay 0)
  (show-paren-mode 1))

;;; bindings

(use-package bad-habits
  :no-require t
  :bind (("<XF86Forward>" . nil)
	 ("<XF86Back>" . nil)
	 :map key-translation-map
	 ("ESC" . "H-M-C-S-1")))

(use-package revert
  :no-require t
  :bind (("C-c r" . revert-buffer)))

(use-package file-access
  :no-require t
  :bind (("C-x f" . 'find-file)))

;;; languages

(use-package nix-mode
  :straight t
  :defer t)

(use-package rust-mode
  :straight t
  :defer t)

(use-package markdown-mode
  :straight t
  :defer t)

(use-package tex
  :straight (auctex :type git :host github
                    :repo "emacs-straight/auctex")
  :defer t
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

(use-package lispy
  :straight t
  :defer t
  :hook (emacs-lisp-mode . lispy-mode))

;;; packages

(use-package flycheck
  :straight t
  :defer t)

(use-package flycheck-aspell
  :straight (flycheck-aspell :type git :host github
			     :repo "leotaku/flycheck-aspell")
  :after flycheck
  :init
  (straight-use-package 'async)
  :config
  (add-to-list 'flycheck-checkers 'tex-aspell-dynamic))

(use-package ispell
  :defer t
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
  :defer t
  :bind (("M-m" . er/expand-region)
	 ("M-n" . er/contract-region)))

(use-package el2org
  :straight t
  :defer t)

(use-package ox-gfm
  :straight t
  :after el2org)

(use-package which-key
  :straight t
  :config
  (which-key-mode 1))

(use-package swiper
  :straight t
  :defer nil
  :config
  (ivy-mode 1))

(use-package counsel
  :straight t
  :after swiper
  :bind (("C-s" . swiper-isearch))
  :config
  (counsel-mode 1))

(use-package projectile
  :straight t
  :bind (("C-x p" . projectile-switch-project))
  :config
  (setq projectile-completion-system 'ivy)
  (setq projectile-project-root-files-functions
	'(projectile-root-top-down)
	projectile-project-root-files
        '(".git" ".bzr" ".svn" ".hg" "_darcs" ".projectile"))
  (projectile-mode 1))

(use-package counsel-projectile
  :straight t
  :after (projectile counsel)
  :config
  (counsel-projectile-mode))

(use-package magit
  :straight t
  :defer t
  :bind (("C-x g" . magit-status)
	 :map magit-status-mode-map
	 ("<return>" . magit-diff-visit-file-other-window)
	 ("j" . magit-next-line)
	 ("k" . magit-previous-line)
	 ("v" . set-mark-command)))

(use-package amx
  :straight t
  :config
  (amx-mode 1))

(use-package moody
  :straight t
  :config
  (setq x-underline-at-descent-line t)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))

(use-package minions
  :straight t
  :config
  (minions-mode 1))

(named-progn mode-line-other
  (column-number-mode 1))

(keyboard-translate ?\C-i ?\H-i)
(keyboard-translate ?\C-m ?\H-m)

(bind-key
 "H-i" 'ivy-insert-selection
 ivy-minibuffer-map)

(defun ivy-insert-selection ()
  (interactive)
  (ivy-exit-with-action
   (lambda (it)
     (interactive)
     (insert it))))
