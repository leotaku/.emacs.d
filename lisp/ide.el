;;; ide.el --- configurations for Emacs as an IDE -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(bk-block ide-backspace-mode
  :hook
  (prog-mode-hook . ide-backspace-mode)
  (text-mode-hook . ide-backspace-mode)
  (conf-mode-hook . ide-backspace-mode)
  (lispy-mode-hook . (lambda () (ide-backspace-mode -1))))

(define-minor-mode ide-backspace-mode
  "IDE style backspace key."
  nil
  "IDE"
  '(([backspace] . ide-backspace)
    ([C-backspace] . ide-backspace-word)))

(defun ide-backspace ()
  (interactive)
  (cond
   ((region-active-p)
    (delete-region (region-beginning) (region-end)))
   ((looking-back "^[[:space:]]+")
    (ide-delete-to-previous-line))
   (t
    ;; delete char normally
    (call-interactively #'backward-delete-char))))

(defun ide-backspace-word ()
  (interactive)
  (cond
   ((looking-back "^[[:space:]]+")
    (ide-delete-to-previous-line))
   (t
    ;; delete word normally
    (call-interactively #'backward-delete-word))))

(defun backward-delete-word (arg)
  (interactive "p")
  (delete-region (point) (progn (backward-word arg) (point))))

(defun ide-delete-to-previous-line ()
  ;; delete all spaces
  (while (not (looking-back "[\n]"))
    (delete-char -1))
  ;; delete final newline
  (delete-char -1)
  ;; go to indentation
  (when (looking-back "[\n]")
    (indent-according-to-mode)))

(bk-block smartparens
  :requires .smartparens .smartparens-config
  :hook
  (prog-mode-hook . smartparens-mode)
  (text-mode-hook . smartparens-mode)
  (conf-mode-hook . smartparens-mode)
  :bind ((:sp-pair-overlay-keymap
          :package smartparens
          ("TAB" . sp-forward-sexp)
          (">" . nil)
          ("<" . nil)))
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
  :requires .company .company-posframe
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
  (company-tooltip-align-annotations . t)
  (company-posframe-show-indicator . nil)
  (company-posframe-show-metadata . nil)
  (company-posframe-quickhelp-delay . nil)
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
  :requires .lsp-mode .lsp-ui .lsp-haskell
  :hook
  (prog-mode-hook . lsp-maybe)
  (lsp-mode-hook . lsp-ui-mode)
  :bind ((:lsp-mode-map
          :package lsp-mode
          ("C-c C-f" . lsp-format-buffer)))
  :custom
  (lsp-signature-auto-activate . nil)
  (lsp-enable-symbol-highlighting . nil)
  (lsp-auto-configure . t)
  (lsp-diagnostic-package . :flymake)
  (lsp-ui-doc-enable . nil)
  (lsp-ui-sideline-enable . nil)
  ;; ghcide
  (lsp-haskell-server-path . "ghcide")
  (lsp-haskell-server-args . '())
  ;; clang
  (lsp-clients-clangd-args
   . '("-header-insertion=never")))

(bk-block direnv
  :requires .direnv
  :start direnv-mode
  :custom
  (direnv-always-show-summary . nil))

;;; ide.el ends here
