;;; load-packages.el --- load package.el packages  -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(setq packages
      '((expand-region-improved
         :vc-backend Git
         :url "git@github.com:leotaku/expand-region-improved")
        (fi
         :vc-backend Git
         :url "git@github.com:leotaku/fi-emacs")
        (flycheck-aspell
         :vc-backend Git
         :url "git@github.com:leotaku/flycheck-aspell")
        (study
         :vc-backend Git
         :url "git@github.com:leotaku/study.el")
        (theist-mode
         :vc-backend Git
         :url "git@github.com:leotaku/theist-mode")
        (doom-themes
         :vc-backend Git
         :url "git@github.com:leotaku/emacs-doom-themes")
        ace-link
        ace-window
        aggressive-indent
        amx
        apheleia
        (auctex :vc-backend None)
        avy
        circe
        company
        company-posframe
        counsel
        counsel-projectile
        deadgrep
        dired-filter
        diredfl
        eglot
        envrc
        (forge :vc-backend None)
        git-modes
        go-mode
        haskell-mode
        hcl-mode
        ledger-mode
        (lispy :vc-backend None)
        lua-mode
        (magit :vc-backend None)
        markdown-mode
        meson-mode
        minions
        modalka
        moody
        multiple-cursors
        nix-mode
        no-littering
        org-reverse-datetree
        pcre2el
        projectile
        rainbow-mode
        rust-mode
        swiper
        terraform-mode
        undo-fu-session
        visual-fill-column
        visual-regexp
        web-mode
        wgrep
        which-key
        worf
        yaml-mode))

(setq package-vc-selected-packages
      (seq-filter
       (lambda (it) (not (eq (plist-get (cdr it) :vc-backend) 'None)))
       (mapcar
        (lambda (it) (ensure-list it)) packages)))

(setq package-selected-packages
      (mapcar (lambda (it) (car (ensure-list it))) packages))

(require 'package)
(require 'package-vc)

(setq custom-file (locate-user-emacs-file "etc/custom.el"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

(package-vc-install-selected-packages)
(package-install-selected-packages t)

(provide 'load-packages)

;;; load-packages.el ends here
