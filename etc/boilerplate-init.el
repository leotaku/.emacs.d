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

(named-progn straight-pre-setup
  (setq straight-enable-use-package-integration t
        straight-fix-org t)
  (setq straight-check-for-modifications
        '(find-when-checking check-on-save)
        straight-recipe-repositories
        '(org-elpa melpa emacsmirror gnu-elpa-mirror)))

(named-progn straight-bootstrap
  (defvar bootstrap-version)
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
  (require 'straight-x))

;; emacs server

(named-progn emacs-server
  (require 'server)
  (add-hook 'emacs-startup-hook
	        (defun maybe-server-start () (unless (server-running-p)
                                           (server-start nil t)))))

;; intrinsic packages - these packages are needed for basic emacs functionality

(named-progn leaf-setup
  (straight-use-package '(leaf :type git :host github
                           :repo "conao3/leaf.el"))
  (straight-use-package '(leaf-keywords :type git :host github
                                        :repo "conao3/leaf-keywords.el"))
  (require 'leaf)
  (require 'leaf-keywords)
  (leaf-keywords-init)
  (defmacro use-package (name &rest args)
    (declare (indent defun))
    `(leaf ,name ,@args))
  (defmacro use-config (name &rest args)
    (declare (indent defun))
    `(leaf ,name ,@args :leaf-defer nil))
  (with-eval-after-load 'lispy
    (push '(use-config . 1)
          (alist-get 'emacs-lisp-mode lispy-tag-arity))))

(named-progn no-littering-setup
  (straight-use-package 'no-littering)
  (require 'no-littering)
  (setq custom-file (no-littering-expand-etc-file-name "custom.el"))
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

(named-progn emacs-stupid
  (setq custom-safe-themes t))

(provide 'boilerplate-init)

;; boilerplate-init.el ends here
