;;; major.el --- major-mode configurations -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(bk-block tree-sitter-modes
  :custom (toml-ts-mode-indent-offset . 4)
  :at-load
  (mapatoms
   (lambda (it)
     (when (string-suffix-p
            "-ts-mode"
            (symbol-name it))
       (require it nil t)))))

(bk-block* nix-mode
  :bind ((:nix-mode-map
          :package nix-mode
          ("C-c C-f" . nix-format-buffer)))
  :custom
  (nix-indent-function . 'nix-indent-line))

(bk-block makefile-mode
  :requires .make-mode
  :mode "Justfile")

(bk-block c++-mode
  :requires .cc-mode
  :config
  (setf (alist-get 'other c-default-style) "wierdo")
  (c-add-style "modern" '("gnu" (c-offsets-alist (innamespace . 0))))
  (c-add-style "wierdo" '("ellemtel" (c-basic-offset . 4))))

(bk-block go-mode
  :custom (go-ts-mode-indent-offset . 4))

(bk-block* markdown-mode
  :custom
  (markdown-hide-urls . t)
  (markdown-fontify-code-blocks-natively . t)
  :config
  (face-spec-reset-face 'markdown-code-face))

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

(bk-block tex
  :requires .latex .study-SyncTeX
  :start TeX-PDF-mode TeX-source-correlate-mode
  :config
  (setf (alist-get 'output-pdf TeX-view-program-selection) "study-SyncTeX"))

(bk-block emacs-lisp-mode
  :requires .elisp-mode .lispy .theist-mode .aggressive-indent
  :mode ".dir-locals.el"
  :hook
  (minibuffer-setup-hook . minibuffer-lisp-config-enable)
  (lispy-mode-hook . aggressive-indent-mode)
  (emacs-lisp-mode-hook . lispy-mode)
  :bind ((:lispy-mode-map
          :package lispy
          ("{" . lispy-backward)
          ("}" . lispy-forward)))
  :config
  (lispy-define-key lispy-mode-map "x" #'theist-C-x)
  (lispy-define-key lispy-mode-map "[" #'ignore)
  (lispy-define-key lispy-mode-map "]" #'ignore)
  :config
  (load-file (locate-library "lispy-tags")))

(defun minibuffer-lisp-config-enable ()
  (when (eq this-command 'eval-expression)
    (setq-local indent-line-function #'lisp-indent-line)
    (lispy-mode 1)))

;;; major.el ends here
