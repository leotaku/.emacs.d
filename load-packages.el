;;; load-packages.el --- load package.el packages  -*- lexical-binding: t; -*-

;;; Commentary:
;;

(require 'package)
(require 'package-vc)

;;; Code:

(defun package-generate-autoloads (name pkg-dir)
  "Generate autoloads in PKG-DIR for package named NAME."
  (let* ((auto-name (format "%s-autoloads.el" name))
         ;;(ignore-name (concat name "-pkg.el"))
         (output-file (expand-file-name auto-name pkg-dir))
         ;; We don't need 'em, and this makes the output reproducible.
         (autoload-timestamps nil)
         (backup-inhibited t)
         (version-control 'never))
    (loaddefs-generate
     pkg-dir output-file nil
     (prin1-to-string
      '(add-to-list
        'load-path
        ;; Add the directory that will contain the autoload file to
        ;; the load path.  We don't hard-code `pkg-dir', to avoid
        ;; issues if the package directory is moved around.
        (or (and load-file-name (file-name-directory load-file-name))
            (car load-path))))
     nil t)
    (let ((buf (find-buffer-visiting output-file)))
      (when buf (kill-buffer buf)))
    auto-name))

(setq packages
      '((expand-region-improved
         :vc-backend Git
         :url "git@github.com:leotaku/expand-region-improved")
        (fi
         :vc-backend Git
         :url "git@github.com:leotaku/fi-emacs")
        (motion
         :vc-backend Git
         :url "git@github.com:leotaku/motion")
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
