;;; load-packages.el --- load package.el packages  -*- lexical-binding: t; -*-

;;; Commentary:
;;

(require 'package)
(require 'package-vc)

;;; Code:

(setq packages
      '((expand-region-improved
         :vc-backend Git
         :url "https://github.com/leotaku/expand-region-improved")
        (fi
         :vc-backend Git
         :url "https://github.com/leotaku/fi-emacs")
        (motion
         :vc-backend Git
         :url "https://github.com/leotaku/motion")
        ;; (flycheck-aspell
        ;;  :vc-backend Git
        ;;  :url "https://github.com/leotaku/flycheck-aspell")
        (study
         :vc-backend Git
         :url "https://github.com/leotaku/study.el")
        (theist-mode
         :vc-backend Git
         :url "https://github.com/leotaku/theist-mode")
        (leyline
         :vc-backend Git
         :url "https://github.com/leotaku/leyline")
        (ace-window
         :vc-backend Git
         :url "https://github.com/leotaku/ace-window")
        (doom-themes
         :vc-backend Git
         :url "https://github.com/leotaku/emacs-doom-themes")
        (worf
         :vc-backend Git
         :url "https://github.com/leotaku/worf"
         :branch "patch-1")
        (multiple-cursors
         :vc-backend Git
         :url "https://github.com/magnars/multiple-cursors.el")
        ace-link
        aggressive-indent
        apheleia
        auctex
        avy
        cape
        circe
        corfu
        counsel
        deadgrep
        dired-filter
        diredfl
        eat
        eglot
        eglot-booster
        envrc
        expand-region
        forge
        git-modes
        haskell-mode
        hcl-mode
        ledger-mode
        lispy
        magit
        markdown-mode
        meson-mode
        minions
        modalka
        moody
        nix-mode
        no-littering
        org-reverse-datetree
        pcre2el
        rainbow-mode
        swiper
        terraform-mode
        undo-fu-session
        visual-fill-column
        visual-regexp
        wgrep
        which-key
        yasnippet))

(setq package-vc-selected-packages
      (seq-filter
       (lambda (it) (plist-get (cdr it) :vc-backend))
       (mapcar #'ensure-list packages)))

(setq package-selected-packages
      (mapcar #'car (mapcar #'ensure-list packages)))

(setq package-vc-register-as-project nil)

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(setf (alist-get "gnu" package-archive-priorities) 1)
(setf (alist-get "nongnu" package-archive-priorities) 1)
(setf (alist-get "melpa" package-archive-priorities) 0)

(package-initialize)
(package-vc-install-selected-packages)
(package-install-selected-packages t)

(provide 'load-packages)

;;; load-packages.el ends here
