;;; load-packages.el --- load package.el packages  -*- lexical-binding: t; -*-

;;; Commentary:
;;

(require 'package)
(require 'package-vc)

;;; Code:

(setq packages
      '((expand-region-improved
         :vc-backend Git
         :url "git@github.com:leotaku/expand-region-improved")
        (fi
         :vc-backend Git
         :url "git@github.com:leotaku/fi-emacs")
        ;; (flycheck-aspell
        ;;  :vc-backend Git
        ;;  :url "git@github.com:leotaku/flycheck-aspell")
        (study
         :vc-backend Git
         :url "git@github.com:leotaku/study.el")
        (theist-mode
         :vc-backend Git
         :url "git@github.com:leotaku/theist-mode")
        (ace-window
         :vc-backend Git
         :url "git@github.com:leotaku/ace-window")
        (doom-themes
         :vc-backend Git
         :url "git@github.com:leotaku/emacs-doom-themes")
        (worf
         :vc-backend Git
         :url "git@github.com:leotaku/worf"
         :branch "patch-1")
        ace-link
        aggressive-indent
        amx
        apheleia
        auctex
        avy
        circe
        corfu
        counsel
        counsel-projectile
        deadgrep
        dired-filter
        diredfl
        eglot
        envrc
        expand-region
        forge
        git-modes
        go-mode
        haskell-mode
        hcl-mode
        ledger-mode
        lispy
        lua-mode
        magit
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
        yaml-mode
        yasnippet))

(setq package-vc-selected-packages
      (seq-filter
       (lambda (it) (plist-get (cdr it) :vc-backend))
       (mapcar #'ensure-list packages)))

(setq package-selected-packages
      (mapcar #'car (mapcar #'ensure-list packages)))

(setq package-vc-register-as-project nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

(package-vc-install-selected-packages)
(package-install-selected-packages t)

(provide 'load-packages)

;;; load-packages.el ends here
