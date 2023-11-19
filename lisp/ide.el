;;; ide.el --- configurations for Emacs as an IDE -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(bk-block aggressive-backspace-mode
  :requires .aggressive-indent
  :hook
  (prog-mode-hook . aggressive-backspace-mode)
  (text-mode-hook . aggressive-backspace-mode)
  (conf-mode-hook . aggressive-backspace-mode))

(define-minor-mode aggressive-backspace-mode nil
  :lighter " <="
  :keymap `(([backspace]
             menu-item "maybe-delete-indentation" ignore :filter
             (lambda (&optional _)
               (when (and (not aggressive-indent-mode)
                          (looking-back "^[[:blank:]]+")
                          (not (run-hook-wrapped 'aggressive-indent--internal-dont-indent-if #'eval))
                          (not (aggressive-indent--run-user-hooks)))
                 #'delete-indentation)))))

(bk-block electric
  :requires .elec-pair
  :start electric-pair-mode)

(bk-block corfu
  :requires .corfu
  :start global-corfu-mode
  :custom
  (corfu-auto . t)
  (corfu-auto-prefix . 1)
  (corfu-auto-delay . 0)
  (corfu-bar-width . 0)
  (completion-styles . '(basic))
  :bind ((:corfu-map
          :package corfu
          ("RET" . nil)))
  :config
  (setf (alist-get 'child-frame-border-width corfu--frame-parameters) 0))

(bk-block fix-semantic
  :requires .semantic/db-file
  :at-load
  (with-eval-after-load 'semantic
    (defun hook-semantic-fix-lispy ()
      (dolist (x (default-value 'completion-at-point-functions))
        (when (string-prefix-p "semantic-" (symbol-name x))
          (remove-hook 'completion-at-point-functions x))))
    (add-hook 'semantic-mode-hook #'hook-semantic-fix-lispy)))

(bk-block envrc
  :requires .envrc
  :start envrc-global-mode)

(bk-block apheleia
  :requires .apheleia
  :bind (("C-c f" . apheleia-format-buffer)))

(bk-block eglot
  :requires .eglot .yasnippet
  :at-load (setq eglot-stay-out-of '(company))
  :custom
  (eldoc-echo-area-use-multiline-p . nil)
  (eglot-ignored-server-capabilities  . '(:inlayHintProvider))
  :hook (prog-mode-hook . eglot-ensure)
  :bind ((:eglot-mode-map
          :package eglot
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

(bk-block python-ide
  :requires .eglot .aggressive-indent
  :config
  (add-to-list 'aggressive-indent-dont-indent-if '(eq major-mode 'python-mode))
  :config
  (setf (plist-get (default-value 'eglot-workspace-configuration) :pylsp)
        '(:plugins (;;
                    :jedi_completion (:include_params t :fuzzy t)
                    :pylint (:enabled :json-false)
                    :pycodestyle (:enabled :json-false)))))

;;; ide.el ends here
