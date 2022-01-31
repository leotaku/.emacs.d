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
     :post-handlers '((sp-pretty-newlines "RET")
                      (sp-pretty-spaces "SPC")))))

(defun sp-pretty-newlines (&rest _)
  (newline)
  (indent-according-to-mode)
  (forward-line -1)
  (indent-according-to-mode))

(defun sp-pretty-spaces (&rest _)
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
    (add-hook 'semantic-mode-hook #'hook-semantic-fix-lispy)))

(bk-block direnv
  :requires .direnv
  :start direnv-mode
  :custom
  (direnv-always-show-summary . nil))

(bk-block eglot
  :requires .eglot
  :custom (eldoc-echo-area-use-multiline-p . nil)
  :hook (prog-mode-hook . eglot-ensure)
  :bind ((:eglot-mode-map
          :package eglot
          ("C-c f" . eglot-format)
          ("C-c r" . eglot-rename)
          ("C-c a" . eglot-code-actions)))
  :config
  (set-face-bold 'eglot-highlight-symbol-face nil)
  (advice-add 'eglot--connect :around #'advice-eglot-connect)
  (advice-add 'eglot-ensure :around #'advice-eglot-ensure))

(defun advice-eglot-connect (fn &rest args)
  (if (eq this-command 'eglot)
      (apply fn args)
    (condition-case-unless-debug err
        (apply fn args)
      (error nil))))

(defun advice-eglot-ensure (fn &rest args)
  (when (or (not (daemonp)) server-process)
    (run-with-timer
     0 nil
     (lambda ()
       (condition-case-unless-debug err
           (and (eglot--lookup-mode major-mode)
                (eglot--guess-contact)
                (apply fn args))
         (error nil))))))

;;; ide.el ends here
