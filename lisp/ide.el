;;; ide.el --- configurations for emacs as an ide -*- lexical-binding: t; -*-

;;; Commentary:
;; 

;;; Code:

(bk-block ide-backspace
  :bind ((:prog-mode-map
          :package emacs
          ("<backspace>" . ide-backspace)
          ("<C-backspace>" . ide-backspace-word))
         (:text-mode-map
          :package emacs
          ("<backspace>" . ide-backspace)
          ("<C-backspace>" . ide-backspace-word))
         (:lispy-mode-map
          :package lispy
          ("<backspace>" . lispy-delete-backward))))

(defun ide-backspace ()
  (interactive)
  (cond
   ((region-active-p)
    (delete-region (region-beginning) (region-end)))
   ((looking-back "^[[:space:]]+")
    (ide-delete-to-previous-line))
   (t
    ;; delete char normally
    (call-interactively 'backward-delete-char))))

(defun ide-backspace-word ()
  (interactive)
  (cond
   ((looking-back "^[[:space:]]+")
    (ide-delete-to-previous-line))
   (t
    ;; delete word normally
    (call-interactively 'backward-delete-word))))

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
  :start smartparens-global-mode
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
  (company-frontends . '(company-tng-frontend
                         company-pseudo-tooltip-frontend
                         company-echo-metadata-frontend))
  (company-posframe-show-indicator . nil)
  (company-posframe-show-metadata . nil)
  (company-posframe-quickhelp-delay . nil)
  :config
  (fi-configure-gui
   (company-posframe-mode)))

(eval-after-load 'semantic
  (add-hook 'semantic-mode-hook
            (lambda ()
              (dolist (x (default-value 'completion-at-point-functions))
                (when (string-prefix-p "semantic-" (symbol-name x))
                  (remove-hook 'completion-at-point-functions x))))))

(bk-block* editorconfig
  :start editorconfig-mode)

(bk-block* direnv
  :start direnv-mode)

(bk-block lsp
  :requires .lsp-mode
  :wanted-by delayed-target
  :hook
  (rust-mode-hook . lsp-deferred)
  :custom
  (lsp-enable-symbol-highlighting . nil)
  (lsp-auto-configure . t)
  (lsp-prefer-flymake . :none))

(bk-block taskrunner
  :requires .taskrunner .ivy-taskrunner
  :custom
  (taskrunner-cache-file
   . (no-littering-expand-var-file-name "taskrunner-cache-file.eld")))

;;; ide.el ends here
