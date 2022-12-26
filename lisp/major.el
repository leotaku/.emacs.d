;;; major.el --- major-mode configurations -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(bk-block* nix-mode
  :bind ((:nix-mode-map
          :package nix-mode
          ("C-c C-f" . nix-format-buffer)))
  :custom
  (nix-indent-function . 'nix-indent-line))

(bk-block makefile-mode
  :requires .make-mode
  :mode "Makefile" "Justfile")

(bk-block c++-mode
  :requires .cc-mode
  :config
  (setf (alist-get 'other c-default-style) "wierdo")
  (c-add-style "modern" '("gnu" (c-offsets-alist (innamespace . 0))))
  (c-add-style "wierdo" '("ellemtel" (c-basic-offset . 4))))

(bk-block* rust-mode
  :bind ((:rust-mode-map
          :package rust-mode
          ("C-c C-f" . nil))))

(bk-block* markdown-mode
  :mode
  ("\\.md\\'" . gfm-mode)
  ("\\.markdown\\'" . gfm-mode)
  :custom
  (markdown-hide-urls . t))

(bk-block* web-mode
  :bind ((:web-mode-map
          :package web-mode
          ("C-c C-f" . nil)))
  :mode "\\.html?\\'"
  :custom
  (web-mode-code-indent-offset . 2)
  (web-mode-markup-indent-offset . 2)
  (web-mode-css-indent-offset . 2)
  (web-mode-script-padding . 2)
  (web-mode-style-padding . 2))

(bk-block js-mode
  :requires .js
  :mode "\\.[cm]js\\'"
  :custom
  (js-indent-level . 2)
  (js-jsx-indent-level . 2)
  (js-jsx-syntax . t)
  (js-switch-indent-offset . 2))

(bk-block* css-mode
  :mode "\\.rasi\\'"
  :custom
  (css-indent-offset . 2))

(bk-block conf-mode
  :requires .conf-mode .js
  :config
  (setq-mode-local
   conf-mode
   indent-line-function 'js-indent-line))

(bk-block tex
  :requires .latex .study-SyncTeX
  :start TeX-PDF-mode TeX-source-correlate-mode
  :custom
  (TeX-view-program-selection . '((output-pdf "study-SyncTeX"))))

(bk-block emacs-lisp-mode
  :requires .elisp-mode .lispy .theist-mode .aggressive-indent
  :mode ".dir-locals.el"
  :hook
  (minibuffer-setup-hook . conditionally-enable-lispy)
  (lispy-mode-hook . aggressive-indent-mode)
  (emacs-lisp-mode-hook . lispy-mode)
  :bind ((:lispy-mode-map
          :package lispy
          ("{" . lispy-backward)
          ("}" . lispy-forward)))
  :config
  (lispy-define-key lispy-mode-map "x" #'theist-C-x)
  (lispy-define-key lispy-mode-map "[" #'ignore)
  (lispy-define-key lispy-mode-map "]" #'ignore))

(defun conditionally-enable-lispy ()
  (when (eq this-command 'eval-expression)
    (setq-local indent-line-function #'lisp-indent-line)
    (lispy-mode 1)))

;;; major.el ends here
