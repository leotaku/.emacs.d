;;; major.el --- major-mode configurations for fi-emacs -*- lexical-binding: t; -*-

;;; Commentary:
;; 

;;; Code:

(bk-block org
  :wanted-by delayed-target
  :requires .org .org-cliplink
  :custom
  (org-adapt-indentation . nil)
  (org-tags-column . 0)
  :config
  (set-face-attribute
   'org-document-title nil
   :inherit 'variable-pitch
   :height 150))

(bk-block* nix-mode
  :custom
  (nix-indent-function . 'nix-indent-line))

(bk-block makefile-mode
  :requires .make-mode
  :mode "Makefile" "Justfile" "justfile")

(bk-block* rust-mode
  :mode "\\.lalrpop\\'" "\\.rustpeg\\'")

(bk-block python-mode
  :requires .python
  :mode "SConstruct" "SConscript")

(bk-block* markdown-mode
  :mode
  ("\\.md\\'" . gfm-mode)
  ("\\.markdown\\'" . gfm-mode))

(bk-block tex
  :wanted-by delayed-target
  :requires .auctex-latexmk .tex
  :bind ((:TeX-mode-map
          :package tex
          ("C-c c" . ivy-bibtex-with-local-bibliography)))
  :custom
  (bibtex-completion-cite-default-command
   . "autocite")
  (ivy-bibtex-default-action
   . 'ivy-bibtex-insert-citation)
  :config
  (auctex-latexmk-setup)
  (TeX-PDF-mode)
  (TeX-source-correlate-mode)
  
  (setq TeX-view-program-selection
        (list '(output-pdf "Zathura")))
  
  (add-to-list
   'TeX-expand-list
   '("%sn" (lambda () server-name)))
  
  (setq TeX-view-program-list
        (list '("Zathura"
                ("zathura %o"
                 (mode-io-correlate " --synctex-forward %n:0:%b -x \"emacsclient --socket-name=%sn --no-wait +%{line} %{input}\""))
                "zathura"))))

(bk-block lispy
  :requires .lispy .theist-mode .aggressive-indent
  :hook
  (minibuffer-setup-hook . conditionally-enable-lispy)
  (lispy-mode-hook . aggressive-indent-mode)
  (emacs-lisp-mode-hook . lispy-mode)
  :init
  (defun conditionally-enable-lispy ()
    (when (eq this-command 'eval-expression)
      (lispy-mode 1)))
  :config
  (lispy-define-key lispy-mode-map "x" 'theist-C-x))

(bk-block common-lisp-mode
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

(provide 'major)

;;; major.el ends here
