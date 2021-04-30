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

(bk-block python-mode
  :requires .python
  :mode "SConstruct" "SConscript")

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
  :requires .web-mode .tide
  :mode ("\\.tsx?\\'" . web-mode)
  :bind ((:tide-mode-map
          :package tide
          ("C-c C-f" . tide-format)))
  :hook
  (web-mode-hook . tide-maybe)
  (tide-mode-hook . flycheck-mode)
  (tide-mode-hook . eldoc-mode))

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
  :custom
  (TeX-view-program-selection
   . (list '(output-pdf "Zathura")))
  (TeX-view-program-list
   . (list '("Zathura"
             ("zathura %o"
              (mode-io-correlate " --synctex-forward %n:0:%b -x \"emacsclient --socket-name=%sn --no-wait +%{line} %{input}\""))
             "zathura")))
  :config
  (TeX-PDF-mode)
  (TeX-source-correlate-mode)
  (advice-add
   'TeX-active-master
   :around 'advice-TeX-active-master-pdf)
  (add-to-list
   'TeX-expand-list
   '("%sn" (lambda () server-name))))

(defun advice-TeX-active-master-pdf (fun &optional extension nondirectory ignore)
  (if (string= extension "pdf")
      (concat "out/" (apply fun extension nondirectory ignore))
    (apply fun extension nondirectory ignore)))

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
