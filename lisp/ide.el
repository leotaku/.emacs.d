;;; ide.el --- settings for emacs as an ide

;;; Commentary:
;; 

;;; Code:

(leaf ide-backspace
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
          ("<backspace>" . lispy-delete-backward)))
  :config
  (defun ide-backspace ()
    (interactive)
    (cond
     ((region-active-p)
      (kill-region (region-beginning) (region-end)))
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
      (call-interactively 'backward-kill-word))))
  (defun ide-delete-to-previous-line ()
    ;; delete all spaces
    (while (not (looking-back "[\n]"))
      (delete-char -1))
    ;; delete final newline
    (delete-char -1)
    ;; go to indentation
    (when (looking-back "[\n]")
      (indent-according-to-mode))))

(leaf smartparens
  :straight t
  :hook ((prog-mode-hook text-mode-hook). smartparens-mode)
  :config
  (require 'smartparens-config)
  (defun ide-insert-newlines (&rest _)
    (newline)
    (indent-according-to-mode)
    (forward-line -1)
    (indent-according-to-mode))
  (defun ide-insert-spaces (&rest _)
    (insert " ")
    (backward-char))
  (dolist (paren-type '("(" "[" "{"))
    (sp-local-pair
     'prog-mode paren-type nil
     :post-handlers '((ide-insert-newlines "RET")
                      (ide-insert-spaces "SPC")))))

(leaf company
  :straight t
  :hook ((prog-mode-hook text-mode-hook) . company-mode)
  :bind (:company-active-map
         ("RET" . nil)
         ("<return>" . nil)
         ("C-h" . nil)
         ("<escape>" . nil)
         ("<tab>" . company-complete-common-or-cycle)
         ("<backtab>" . company-select-previous))
  :custom
  (company-minimum-prefix-length . 1)
  (company-idle-delay . 0.2)
  (company-dabbrev-downcase . nil)
  (company-dabbrev-ignore-case . nil)
  (company-require-match . nil)
  (company-tooltip-align-annotations . t)
  (company-frontends . '(company-tng-frontend
                         company-pseudo-tooltip-frontend
                         company-echo-metadata-frontend)))

(eval-after-load 'semantic
  (add-hook 'semantic-mode-hook
            (lambda ()
              (dolist (x (default-value 'completion-at-point-functions))
                (when (string-prefix-p "semantic-" (symbol-name x))
                  (remove-hook 'completion-at-point-functions x))))))

(provide 'ide)

;;; ide.el ends here
