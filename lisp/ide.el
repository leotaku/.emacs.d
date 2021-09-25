;;; ide.el --- configurations for Emacs as an IDE -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(bk-block aggressive-backspace-mode
  :requires .aggressive-indent
  :hook
  (prog-mode-hook . aggressive-backspace-mode)
  (text-mode-hook . aggressive-backspace-mode)
  (conf-mode-hook . aggressive-backspace-mode))

(define-minor-mode aggressive-backspace-mode
  nil nil " <="
  `(([backspace]
     menu-item "maybe-delete-indentation" ignore :filter
     (lambda (&optional _)
       (when (and (not aggressive-indent-mode)
                  (looking-back "^[[:blank:]]+")
                  (not (run-hook-wrapped 'aggressive-indent--internal-dont-indent-if #'eval))
                  (not (aggressive-indent--run-user-hooks)))
         #'delete-indentation)))))

(bk-block smartparens
  :requires .smartparens .smartparens-config
  :hook
  (prog-mode-hook . smartparens-mode)
  (text-mode-hook . smartparens-mode)
  (conf-mode-hook . smartparens-mode)
  :bind ((:sp-pair-overlay-keymap
          :package smartparens
          ("TAB" . sp-forward-sexp)))
  :config
  (dolist (paren-type '("(" "[" "{"))
    (sp-local-pair
     'prog-mode paren-type nil
     :post-handlers '((ide-insert-newlines "RET")
                      (ide-insert-spaces "SPC")))))

(defun ide-insert-newlines (&rest _)
  (newline)
  (indent-according-to-mode)
  (forward-line -1)
  (indent-according-to-mode))

(defun ide-insert-spaces (&rest _)
  (insert " ")
  (backward-char))

(bk-block company
  :requires .company .company-posframe .yasnippet
  :start yas-global-mode
  :hook
  (prog-mode-hook . company-mode)
  (text-mode-hook . company-mode)
  (conf-mode-hook . company-mode)
  :bind ((:company-active-map
          :package company
          ("RET" . nil)
          ("<return>" . nil)
          ("C-h" . nil)
          ("<escape>" . nil)
          ("<tab>" . company-complete-selection)
          ("<backtab>" . company-select-previous)))
  :custom
  (company-minimum-prefix-length . 1)
  (company-idle-delay . 0.2)
  (company-dabbrev-downcase . nil)
  (company-dabbrev-ignore-case . nil)
  (company-require-match . nil)
  (company-format-margin-function . nil)
  (company-tooltip-align-annotations . t)
  (company-posframe-show-indicator . nil)
  (company-posframe-show-metadata . nil)
  (company-posframe-quickhelp-delay . nil)
  (company-backends
   . (seq-difference company-backends '(company-cmake company-clang)))
  :config
  (fi-with-gui
   (company-posframe-mode 1)))

(bk-block fix-semantic
  :requires .semantic/db-file
  :at-load
  (with-eval-after-load 'semantic
    (defun hook-semantic-fix-lispy ()
      (dolist (x (default-value 'completion-at-point-functions))
        (when (string-prefix-p "semantic-" (symbol-name x))
          (remove-hook 'completion-at-point-functions x))))
    (add-hook 'semantic-mode-hook 'hook-semantic-fix-lispy)))

(bk-block lsp
  :requires company .lsp-mode .lsp-ui .lsp-haskell
  :hook
  (prog-mode-hook . lsp-maybe)
  (lsp-mode-hook . lsp-ui-mode)
  :bind ((:lsp-mode-map
          :package lsp-mode
          ("C-c C-f" . lsp-format-buffer)))
  :at-load
  (setq lsp-keymap-prefix "C-c l")
  :custom
  (lsp-signature-auto-activate . nil)
  (lsp-enable-symbol-highlighting . nil)
  (lsp-auto-configure . t)
  (lsp-diagnostic-package . :flymake)
  (lsp-ui-doc-enable . nil)
  (lsp-ui-sideline-enable . nil)
  ;; clang
  (lsp-clients-clangd-args
   . '("-header-insertion=never")))

(bk-block direnv
  :requires .direnv
  :start direnv-mode
  :custom
  (direnv-always-show-summary . nil))

;;; ide.el ends here
