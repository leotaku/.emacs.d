;;; major.el --- major-mode configurations -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(bk-block* nix-mode
  :custom
  (nix-indent-function . 'nix-indent-line))

(bk-block makefile-mode
  :requires .make-mode
  :mode "Makefile" "Justfile" "justfile")

(bk-block c++-mode
  :requires .cc-mode
  :mode
  ("\\.txx\\'" . c++-mode)
  ("\\.ixx\\'" . c++-mode)
  ("\\.tpp\\'" . c++-mode)
  ("\\.ipp\\'" . c++-mode)
  :config
  (setf (alist-get 'c++-mode c-default-style) "mine++")
  (c-add-style
   "mine++"
   '("gnu" (c-offsets-alist (innamespace . 0)))))

(bk-block build2-mode
  :requires .conf-mode .cc-mode
  :mode
  ("buildfile" . conf-mode)
  ("manifest" . conf-mode)
  ("testscript" . conf-mode)
  ("\\.build\\'" . conf-mode)
  ("\\.cli\\'" . cli-mode)
  :config
  (define-derived-mode cli-mode
    c++-mode "Cli"))

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
  ("\\.markdown\\'" . gfm-mode))

(bk-block* web-mode
  :mode "\\.html?\\'")

(bk-block* css-mode
  :mode "\\.rasi\\'")

(bk-block js-mode
  :requires .js
  :custom
  (js-indent-level . 2))

(bk-block conf-mode
  :requires .conf-mode .js
  :config
  (setq-mode-local
   conf-mode
   indent-line-function 'js-indent-line))

(bk-block* haskell-mode
  :mode
  ("\\.wrk\\'" . haskell-mode))

(bk-block tex
  :wanted-by delayed-target
  :requires .latex .ivy-bibtex
  :bind ((:TeX-mode-map
          :package tex
          ("C-c c" . ivy-bibtex-with-local-bibliography)))
  :custom
  (bibtex-completion-cite-default-command
   . "autocite")
  (ivy-bibtex-default-action
   . 'ivy-bibtex-insert-citation)
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
  (add-to-list
   'TeX-expand-list
   '("%sn" (lambda () server-name))))

(advice-add 'TeX-active-master :around 'advice-TeX-active-master-pdf)

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
  (lispy-define-key lispy-mode-map "x" 'theist-C-x)
  (lispy-define-key lispy-mode-map "[" 'fi-do-nothing)
  (lispy-define-key lispy-mode-map "]" 'fi-do-nothing))

(bk-block common-lisp-mode
  :requires .lisp-mode
  :wanted-by delayed-target
  :mode
  ("\\.cl\\'" . common-lisp-mode)
  :hook
  (lisp-mode-hook . lispy-mode)
  :config
  (setq-mode-local
   lisp-mode lisp-indent-function
   'common-lisp-indent-function))

(bk-block* sly
  :wanted-by delayed-target
  :bind ((:sly-mrepl-mode-map
          :package sly-mrepl
          ("C-l" . comint-clear-buffer)))
  :custom
  (inferior-lisp-program . "sbcl"))

;;; major.el ends here
