;;; usability.el --- basic usability configurations -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(bk-block* visual-regexp
  :requires .visual-regexp-steroids
  :custom
  (vr/engine . 'pcre2el))

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
  (ispell-silently-savep . t)
  :config
  (defun hook-ispell-save-local-dictionary ()
    (save-excursion
      (add-file-local-variable
       'ispell-local-dictionary
       ispell-local-dictionary)))
  (add-hook 'ispell-change-dictionary-hook 'hook-ispell-save-local-dictionary))

(bk-block* which-key
  :start which-key-mode)

(bk-block* ivy
  :bind ((:ivy-minibuffer-map
          :package ivy
          ("H-i" . ivy-insert-selection)))
  :start ivy-mode
  :custom
  (ivy-use-selectable-prompt . t))

(bk-block* counsel
  :bind (("C-x m" . counsel-M-x)
         ("C-x l" . counsel-recentf)
         ("C-s" . swiper-isearch)
         (:counsel-describe-map
          :package counsel
          ("C-h" . counsel-lookup-symbol)))
  :start counsel-mode)

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

(bk-block! undo-fu-session
  :requires .undo-fu-session
  :custom
  (undo-fu-session-incompatible-files
   . '("COMMIT_EDITMSG$"
       "git-rebase-todo$"))
  (undo-fu-session-linear . t)
  :start global-undo-fu-session-mode)

(bk-block* ace-link
  :bind
  (("C-x a" . ace-link))
  :custom
  (ace-link-fallback-function . 'ace-link-org))

(bk-block! recentf
  :requires .recentf
  :at-load
  (setq recentf-max-saved-items 4000)
  (setq recentf-max-menu-items 1000)
  :config
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory)
  (add-to-list 'recentf-exclude (getenv "TMPDIR"))
  (add-to-list 'recentf-exclude "/tmp"))

(bk-block dired
  :requires .dired .diredfl .dired-filter .theist-mode trash
  :bind ((:dired-mode-map
          :package dired
          ("j" . next-line)
          ("k" . previous-line)
          ("s" . swiper)
          ("x" . theist-C-x)
          ("d" . dired-do-delete)
          ("D" . dired-do-delete)
          ("e" . wdired-change-to-wdired-mode)
          ("M" . dired-filter-mark-by-regexp)
          ("RET" . dired-better-find-file)
          ("DEL" . dired-better-up-directory)
          ("TAB" . dired-hide-details-mode)))
  :custom
  (dired-filter-stack . '((dot-files) (omit)))
  (dired-clean-confirm-killing-deleted-buffers . nil)
  (dired-listing-switches . "-al --group-directories-first")
  :hook
  (dired-mode-hook . dired-filter-mode)
  :config
  (diredfl-global-mode))

(bk-block tramp
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
  (advice-add 'term-handle-exit :after 'advice-term-exit))

(bk-block ibuffer
  :requires .ibuffer .theist-mode
  :bind ((:ibuffer-mode-map
          :package ibuffer
          ("x" . theist-C-x)
          ("j" . next-line)
          ("k" . previous-line)
          ("d" . ibuffer-do-delete))))

(bk-block visual-fill-column
  :requires .visual-fill-column
  :hook
  (text-mode-hook . visual-fill-column-mode)
  :bind ((:visual-fill-column-mode-map
          :package visual-fill-column
          ([remap fill-column] . visual-fill-column-warn-fill)
          ([remap org-fill-paragraph] . visual-fill-column-warn-fill)
          ([remap fill-paragraph] . visual-fill-column-warn-fill)))
  :config
  (defun visual-fill-column-warn-fill ()
    (interactive)
    (message "Auto-fill should not be used with visual-fill-column")))

;;; usability.el ends here
