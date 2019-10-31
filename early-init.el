;;; early-init.el --- Early initialisation -*- lexical-binding: t; -*-

;;; Commentary:

;; This file is loaded before init.el.
;; 
;; `early-init.el' is a new concept introduced in Emacs 27.
;; 
;; Until that becomes stable I simply require the file at the top of
;; my `user-init-file'

;;; Code:

;; Never load outdated bytecode (why is this not the default?)
(setq load-prefer-newer t)

;; Defer garbage collection further back in the startup process
(let ((normal-gc-cons-threshold (* 8 1024 1024))
      (init-gc-cons-threshold (* 256 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (run-with-idle-timer
   5 nil 
   (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

;; Debug garbage collection performance
(setq garbage-collection-messages nil)

;; Package initialization normally occurs automatically, but this can
;; be unset in the `early-init-file'.
(setq package-enable-at-startup nil)

;; Prevent the glimpse of un-styled Emacs by setting these early
(add-to-list 'default-frame-alist '(tool-bar-lines . 0))
(add-to-list 'default-frame-alist '(menu-bar-lines . 0))
(add-to-list 'default-frame-alist '(vertical-scroll-bars))

;; One less file to load at startup
(setq site-run-file nil)

;; Load benchmarking utility from local dir, bypassing `straight.el'
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

;; Emacs wants to load `package.el' before the init file,
;; so we do the same with `straight.el'
(setq straight-enable-use-package-integration nil
      straight-recipes-gnu-elpa-use-mirror t
      straight-fix-org t
      straight-check-for-modifications '(find-when-checking check-on-save)
      straight-recipe-repositories '(org-elpa melpa emacsmirror-mirror gnu-elpa-mirror))

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

;; Load all external packages
(mapc
 'straight-use-package
 (with-current-buffer
     (let ((default-directory user-emacs-directory))
       (find-file-noselect "package-set.el"))
   (goto-char (point-min))
   (prog1
       (read (current-buffer))
     (kill-buffer (current-buffer)))))

(provide 'early-init)

;; early-init.el ends here
