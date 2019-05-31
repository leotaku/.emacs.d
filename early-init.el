;;; early-init.el --- Early initialisation -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2019 Leo Gaskin

;;; Commentary:

;; This file is loaded before init.el and init-boilerplate.el.

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

(provide 'early-init)

;; early-init.el ends here
