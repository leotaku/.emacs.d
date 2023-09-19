;;; early-init.el --- Early initialisation -*- lexical-binding: t; -*-

;;; Commentary:

;; This file is loaded before init.el.

;;; Code:

;; Never load outdated bytecode (why is this not the default?)

(setq load-prefer-newer t)

;; Ensure native bytecode is stored somewhere reasonable

(startup-redirect-eln-cache (locate-user-emacs-file "var/eln-cache"))

;; Defer garbage collection further back in the startup process

(let ((normal-gc-cons-threshold (* 8 1024 1024))
      (init-gc-cons-threshold (* 256 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (run-with-timer
   5 nil
   (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

;; Debug garbage collection performance

(setq garbage-collection-messages nil)

;; Package initialization normally occurs automatically, but this can
;; be unset in the `early-init-file'.

(setq package-enable-at-startup nil)

;; Completely disable the `custom.el' permanent customization system

(setq custom-file (locate-user-emacs-file "var/custom-ignored.el"))

;; Prevent the glimpse of un-styled Emacs by setting these early

(add-to-list 'default-frame-alist '(tool-bar-lines . 0))
(add-to-list 'default-frame-alist '(menu-bar-lines . 0))
(add-to-list 'default-frame-alist '(vertical-scroll-bars . nil))
(add-to-list 'default-frame-alist '(internal-border-width . 12))

(provide 'early-init)

;; early-init.el ends here
