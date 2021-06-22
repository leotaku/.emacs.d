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
  (setf (alist-get 'c++-mode c-default-style) "mine++")
  (c-add-style
   "mine++"
   '("gnu" (c-offsets-alist (innamespace . 0)))))

(bk-block* rust-mode
  :bind ((:rust-mode-map
          :package rust-mode
          ("C-c C-f" . nil)))
  :mode ("\\.lalrpop\\'" . lalrpop-mode)
  :config
  (define-derived-mode lalrpop-mode
    rust-mode "Lalrpop"))

(bk-block* markdown-mode
  :mode
  ("\\.md\\'" . gfm-mode)
  ("\\.markdown\\'" . gfm-mode)
  :custom
  (markdown-hide-urls . t))

(bk-block* web-mode
  :mode "\\.html?\\'"
  :custom
  (web-mode-code-indent-offset . 2)
  (web-mode-markup-indent-offset . 2)
  (web-mode-css-indent-offset . 2))

(bk-block js-mode
  :requires .js
  :mode "\\.jsx?\\'" "\\.json\\'"
  :custom
  (js-indent-level . 2)
  (js-jsx-indent-level . 2)
  (js-jsx-syntax . t)
  (js-switch-indent-offset . 2))

(bk-block typescript-mode
  :requires .web-mode
  :mode ("\\.tsx?\\'" . web-mode))

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
  :requires .latex
  :start TeX-PDF-mode TeX-source-correlate-mode
  :custom
  (TeX-view-program-selection . '((output-pdf "Zathura")))
  (TeX-view-program-list
   . '(("Zathura" (((output-pdf) "zathura %o")
                   ((output-pdf mode-io-correlate)
                    " --synctex-forward %n:0:%b")
                   ((output-pdf mode-io-correlate)
                    " -x 'emacsclient --socket-name=%sn --no-wait +%{line} %{input}'")))))
  :config
  (add-to-list 'TeX-expand-list '("%sn" (lambda () server-name))))

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

;;; major.el ends here
