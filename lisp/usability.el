;;; usability.el --- basic usability configurations -*- lexical-binding: t; -*-

;;; Commentary:
;; 

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
  (ispell-really-hunspell . nil)
  (ispell-silently-savep . t))

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
  :bind (("C-s" . swiper-isearch)
         (:counsel-describe-map
          :package counsel
          ("C-h" . counsel-lookup-symbol)))
  :start counsel-mode)

(bk-block projectile
  :requires .counsel-projectile .projectile counsel
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

(bk-block! undo-fu
  :requires .undo-fu .undo-fu-session
  :custom
  (undo-fu-session-incompatible-files
   . '("COMMIT_EDITMSG$"
       "git-rebase-todo$"))
  (undo-fu-session-directory
   . (no-littering-expand-var-file-name "undo-fu-session"))
  :start global-undo-fu-session-mode)

(bk-block* ace-link
  :bind
  (("C-x a" . ace-link))
  :custom
  (ace-link-fallback-function . 'ace-link-org))

(bk-block yankpad
  :requires .yankpad .yasnippet projectile
  :bind (("C-x y" . yankpad-insert)
         ("C-x Y" . yankpad-capture-snippet))
  :custom (yankpad-file
           . (expand-file-name "yankpad.org" "~/sync"))
  :start yas-global-mode)

(provide 'usability)

;;; usability.el ends here
