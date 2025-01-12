;;; ide.el --- configurations for Emacs as an IDE -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(bk-block electric
  :requires .elec-pair
  :start electric-pair-mode)

(bk-block corfu
  :requires .corfu .cape
  :start global-corfu-mode
  :at-load (setq text-mode-ispell-word-completion nil)
  :custom
  (corfu-auto . t)
  (corfu-auto-prefix . 1)
  (corfu-auto-delay . 0.0)
  (corfu-bar-width . 0.0)
  (completion-styles . '(basic))
  :bind ((:corfu-map
          :package corfu
          ("RET" . nil)))
  :config
  (setf (alist-get 'child-frame-border-width corfu--frame-parameters) 0)
  (add-hook 'completion-at-point-functions #'cape-dabbrev 100)
  (setq cape-dabbrev-check-other-buffers nil)
  (add-hook 'multiple-cursors-mode-enabled-hook (lambda () (corfu-mode -1)))
  (add-hook 'multiple-cursors-mode-disabled-hook (lambda () (corfu-mode 1))))

(bk-block envrc
  :requires .envrc
  :start envrc-global-mode)

(bk-block apheleia
  :requires .apheleia
  :bind (("C-c f" . apheleia-format-buffer)))

(bk-block eglot
  :requires .eglot .yasnippet .eglot-booster
  :at-load (setq eglot-stay-out-of '(company))
  :custom
  (eldoc-echo-area-use-multiline-p . nil)
  (eglot-ignored-server-capabilities
   . '(:inlayHintProvider :documentOnTypeFormattingProvider))
  :hook (prog-mode-hook . eglot-ensure)
  :bind ((:eglot-mode-map
          :package eglot
          ("C-c r" . eglot-rename)
          ("C-c a" . eglot-code-actions)))
  :config
  (when (executable-find "emacs-lsp-booster")
    (eglot-booster-mode 1))
  (set-face-bold 'eglot-highlight-symbol-face nil)
  (advice-add 'eglot--connect :around #'advice-eglot-connect)
  (advice-add 'eglot-ensure :around #'advice-eglot-ensure)
  (add-hook 'yas-keymap-disable-hook (lambda () completion-in-region-mode)))

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
  :requires .eglot .aggressive-indent .apheleia
  :config
  (add-to-list 'aggressive-indent-dont-indent-if '(eq major-mode 'python-mode))
  (setf (alist-get 'python-mode apheleia-mode-alist) '(ruff ruff-isort))
  (setf (alist-get 'python-ts-mode apheleia-mode-alist) '(ruff ruff-isort))
  :config
  (setf (plist-get (default-value 'eglot-workspace-configuration) :pylsp)
        '(:plugins (;;
                    :jedi_completion (:include_params t :fuzzy t)
                    :pylint (:enabled :json-false)
                    :pycodestyle (:enabled :json-false)))))

(keymap-global-set "<remap> <delete-backward-char>" #'backward-delete-indentation)
(keymap-global-set "M-<backspace>" #'backward-delete-char-untabify)

(defun backward-delete-indentation ()
  (interactive)
  (cond
   ((= (point) (pos-bol))
    (delete-char -1))
   ((/= (point) (save-excursion (skip-chars-backward " \t") (point)))
    (let ((initial-point (point))
          (track-point -1))
      (delete-region (save-excursion (skip-chars-backward " \t") (point)) (point))
      (while (and (/= (point) track-point)
                  (< (point) initial-point))
        (setq track-point (point))
        (indent-for-tab-command))
      (delete-region track-point (point))))
   (t
    (delete-char -1))))

;;; ide.el ends here
