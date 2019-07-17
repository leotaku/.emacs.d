;;; early-init.el --- Early initialisation -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2019 Leo Gaskin

;;; Commentary:

;; This file is loaded before init.el.

;; early-init.el is a new concept introduced in Emacs 27.
;; Until that becomes stable I simply require the file at the top of my
;; `user-init-file'

;;; Code:

;; Defer garbage collection further back in the startup process
(let ((normal-gc-cons-threshold (* 32 1024 1024))
      (init-gc-cons-threshold (* 256 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
	        (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

;; Package initialize occurs automatically, before `user-init-file' is loaded,
;; but after `early-init-file'
(setq package-enable-at-startup nil)

;; Prevent the glimpse of un-styled Emacs by setting these early
(add-to-list 'default-frame-alist '(tool-bar-lines . 0))
(add-to-list 'default-frame-alist '(menu-bar-lines . 0))
(add-to-list 'default-frame-alist '(vertical-scroll-bars))

;; One less file to load at startup
(setq site-run-file nil)

;; load benchmarking utility from local dir, bypassing straight.el
(let ((dir (expand-file-name
            "straight/build/benchmark-init"
            user-emacs-directory)))
  (if (file-directory-p dir)
      (progn
        (add-to-list 'load-path dir)
        (require 'benchmark-init)
        (benchmark-init/activate)
        (add-hook 'after-init-hook 'benchmark-init/deactivate))
    (warn "The benchmark-init package is missing from your straight directory!")))

;; emacs wants to load package.el before the init file, so we do the
;; same with straight.el

(setq straight-enable-use-package-integration nil
      straight-fix-org t
      straight-check-for-modifications '(find-when-checking check-on-save)
      straight-recipe-repositories '(org-elpa melpa emacsmirror gnu-elpa-mirror))

(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(require 'straight-x)

(provide 'early-init)

;; early-init.el ends here
