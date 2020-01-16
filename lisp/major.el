;;; major.el --- major-mode configurations for fi-emacs -*- lexical-binding: t; -*-

;;; Commentary:
;; 

;;; Code:

(leaf org
  :leaf-autoload nil
  :mode ("\\.org\\'" . org-mode)
  :leaf-defer t
  :pre-setq
  (org-adapt-indentation . nil)
  (org-tags-column . 0)
  :config
  (set-face-attribute
   'org-document-title nil
   :inherit 'variable-pitch
   :height 150))

(leaf nix-mode
  :leaf-defer t
  :mode "\\.nix\\'"
  :pre-setq
  (nix-indent-function . 'nix-indent-line))

(leaf lua-mode
  :leaf-defer t
  :mode "\\.lua\\'")

(leaf makefile-mode
  :leaf-defer t
  :mode "Makefile" "Justfile" "justfile")

(leaf rust-mode
  :leaf-defer t
  :mode "\\.rs\\'" "\\.lalrpop\\'" "\\.rustpeg\\'")

(leaf toml-mode
  :leaf-defer t
  :mode "\\.toml\\'")

(leaf markdown-mode
  :leaf-defer t
  :mode (("\\.md\\'" "\\.markdown\\'") . gfm-mode))

(leaf tex
  :leaf-defer t
  :mode ("\\.tex\\'" . TeX-mode)
  :bind ((:tex-mode-map
          :package tex
          ("C-c c" . ivy-bibtex)))
  :pre-setq
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

(leaf emacs-lisp-mode
  :leaf-defer t
  :mode "\\.el\\'"
  :hook (emacs-lisp-mode-hook . lispy-mode))

(bk-block* lispy
  :requires theist-mode
  :hook
  (minibuffer-setup-hook . conditionally-enable-lispy)
  (lispy-mode-hook . aggressive-indent-mode)
  :init
  (defun conditionally-enable-lispy ()
    (when (eq this-command 'eval-expression)
      (lispy-mode 1)))
  :config
  (lispy-define-key lispy-mode-map "x" 'theist-C-x))

(leaf common-lisp-mode
  :mode ("\\.cl\\'" "\\.lisp\\'")
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
