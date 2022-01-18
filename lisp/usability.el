;;; usability.el --- basic usability configurations -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(bk-block* visual-regexp
  :requires .visual-regexp .pcre2el
  :config
  (advice-add 'vr--get-regexp-string :around #'advice-vr-pcre))

(defun advice-vr-pcre (func &optional for-display)
  (let ((text (funcall func)))
    (pcre-to-elisp text)))

(bk-block wgrep
  :requires .wgrep
  :bind ((:ivy-occur-grep-mode-map
          :package ivy
          ("e" . wgrep-change-to-wgrep-mode))))

(bk-block* ispell
  :bind (("C-." . ispell-word))
  :custom
  (ispell-dictionary . "en_US")
  (ispell-program-name . "aspell")
  (ispell-really-aspell . t)
  (ispell-silently-savep . t))

(bk-block* which-key
  :start which-key-mode)

(bk-block* ivy
  :at-load (setq ivy-do-completion-in-region nil)
  :bind ((:ivy-minibuffer-map
          :package ivy
          ("<C-i>" . ivy-insert-selection)))
  :start ivy-mode
  :custom (ivy-use-selectable-prompt . t)
  :config
  (advice-add
   'ivy-switch-buffer-transformer
   :override #'advice-ivy-switch-buffer-transformer))

(defun advice-ivy-switch-buffer-transformer (str)
  "Transform candidate STR like dired when switching buffers."
  (let ((face (ibuffer-buffer-name-face str 0)))
    (ivy-append-face str face)))

(bk-block* counsel
  :bind (("C-x m" . counsel-M-x)
         ("C-x l" . counsel-recentf)
         ("C-s" . swiper-isearch)
         (:counsel-describe-map
          :package counsel
          ("C-h" . counsel-lookup-symbol)))
  :start counsel-mode
  :config
  (advice-add 'counsel-M-x-action :after #'advice-M-x-action))

(defun advice-M-x-action (command)
  (setq command (intern (subst-char-in-string ?\s ?- (string-remove-prefix "^" command))))
  (when (> (mc/num-cursors) 1)
    (unless (or (memq command mc/cmds-to-run-for-all)
                (memq command mc/cmds-to-run-once))
      (mc/prompt-for-inclusion-in-whitelist command))
    (when (memq command mc/cmds-to-run-for-all)
      (mc/execute-command-for-all-fake-cursors command))))

(bk-block projectile
  :requires .projectile .counsel-projectile counsel
  :bind (("C-x p" . projectile-command-map))
  :custom
  (projectile-completion-system . 'ivy)
  (projectile-project-root-files-functions . '(projectile-root-top-down))
  (projectile-project-root-files
   . '(".git" ".bzr" ".svn" ".hg" "_darcs" ".projectile"))
  (projectile-known-projects-file
   . (no-littering-expand-var-file-name "projectile-bookmarks.eld"))
  :start projectile-mode counsel-projectile-mode
  :config
  (setf (car counsel-projectile-switch-project-action) 4)
  (projectile-load-known-projects))

(bk-block* amx
  :start amx-mode)

(bk-block* undo-fu-session
  :requires .undo-fu-session
  :start global-undo-fu-session-mode
  :custom
  (undo-fu-session-incompatible-files
   . '("COMMIT_EDITMSG$"
       "git-rebase-todo$"))
  (undo-fu-session-linear . t))

(bk-block* ace-link
  :bind (("C-x a" . ace-link))
  :custom
  (ace-link-fallback-function . 'ace-link-org))

(bk-block recentf
  :requires .recentf
  :at-load
  (setq recentf-max-saved-items 4000)
  (setq recentf-max-menu-items 1000)
  :config
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory)
  (add-to-list 'recentf-exclude (temporary-file-directory))

(bk-block* tramp
  :config
  (defun tramp (system)
    (interactive "MSystem: ")
    (find-file (format "/-:%s:/" system)))
  :custom
  (tramp-default-method . "ssh"))

(bk-block shell-and-terminal
  :config
  (defun advice-term-exit (&rest _)
    (kill-buffer))
  (advice-add 'term-handle-exit :after #'advice-term-exit))

(bk-block ibuffer
  :requires .ibuffer .theist-mode
  :bind ((:ibuffer-mode-map
          :package ibuffer
          ("x" . theist-C-x)
          ("j" . next-line)
          ("k" . previous-line)
          ("d" . ibuffer-do-delete)))
  :custom
  (ibuffer-fontification-alist
   . '((10 (derived-mode-p 'org-mode) ivy-org)
       (20 (buffer-modified-p) ivy-modified-buffer)
       (25 (not (buffer-file-name)) font-lock-constant-face)
       (30 (string-match "^\\*" (buffer-name)) ivy-virtual)
       (40 (memq major-mode ibuffer-help-buffer-modes) font-lock-comment-face)
       (40 (derived-mode-p 'circe-mode) ivy-remote)
       (40 (derived-mode-p 'dired-mode) ivy-subdir)
       (50 (ivy--remote-buffer-p (current-buffer)) ivy-remote))))

(bk-block visual-fill-column
  :requires .visual-fill-column
  :hook
  (markdown-mode-hook . visual-fill-column-mode)
  (org-mode-hook . visual-fill-column-mode)
  :config
  (advice-add 'fill-column :around #'advice-visual-fill-column-warn)
  (advice-add 'org-fill-paragraph :around #'advice-visual-fill-column-warn)
  (advice-add 'fill-paragraph :around #'advice-visual-fill-column-warn))

(defun advice-visual-fill-column-warn (f &rest args)
  (interactive)
  (if (bound-and-true-p visual-fill-column-mode)
      (message "Auto-fill should not be used with visual-fill-column")
    (apply f args)))

;;; usability.el ends here
