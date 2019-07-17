;;; boilerplate-init.el --- stupid packaging boilerplate -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2019 Leo Gaskin

;;; Commentary:

;; This file is loaded at the beginnig of init.el.

;; Its purpose is to load in settings and packages that are so fundamental
;; to my configuration that they seldom need to be changed or even seen by
;; the user.

;;; Code:

;; named-progn - a macro for visually separating code in the init file

(defmacro named-progn (name &rest body)
  "Just like `progn' but the macro takes an unqoted symbol as the first argument.
This symbol is simply ignored and only serves to add visual identifiers blocks of code in the init file."
  `(progn ,@body))
(put 'named-progn 'lisp-indent-function 'defun)

;; straight.el setup 

;; intrinsic packages - these packages are needed for basic emacs functionality

(named-progn benchmark-init-setup
  "benchmark-init is loaded in early-init.el"
  (straight-use-package 'benchmark-init))

(named-progn keyfreq-setup
  (straight-use-package 'keyfreq)
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

(named-progn emacs-stupid
  (setq custom-safe-themes t))

(provide 'boilerplate-init)

;; boilerplate-init.el ends here
