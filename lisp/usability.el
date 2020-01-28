;;; usability.el --- basic usability packages for emacs -*- lexical-binding: t; -*-

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

(defun ivy-insert-selection ()
  (interactive)
  (ivy-exit-with-action
   (lambda (it)
     (interactive)
     (insert it)
     (signal 'quit nil))))

(bk-block* counsel
  :bind (("C-s" . swiper-isearch)
         (:counsel-describe-map
          :package counsel
          ("C-h" . counsel-lookup-symbol)))
  :start counsel-mode)

(defun counsel-lookup-symbol ()
  "Lookup the current symbol in the help docs."
  (interactive)
  (ivy-exit-with-action
   (lambda (x)
     (if (featurep 'helpful)
         (helpful-symbol (intern x))
       (describe-symbol (intern x))
       (signal 'quit nil)))))

(bk-block projectile
  :requires .counsel-projectile .projectile counsel
  :init
  (fi-auto-keymap (kbd "C-x p") 'projectile-command-map 'projectile)
  :custom
  (projectile-completion-system . 'ivy)
  (projectile-project-root-files-functions . '(projectile-root-top-down))
  (projectile-project-root-files . '(".git" ".bzr" ".svn" ".hg" "_darcs" ".projectile"))
  :start projectile-mode counsel-projectile-mode
  :config
  (setf (car counsel-projectile-switch-project-action) 4)
  (projectile-load-known-projects))

(bk-block* amx
  :start amx-mode)

(bk-block! undo-fu
  :requires .undo-fu .undo-fu-session no-littering
  :custom*
  (undo-fu-session-incompatible-files
   . '("COMMIT_EDITMSG$"
       "git-rebase-todo$"))
  (undo-fu-session-directory
   . (no-littering-expand-var-file-name "undo-fu-session"))
  :start global-undo-fu-session-mode)

(bk-block yankpad
  :requires .yankpad .yasnippet projectile
  :bind (("C-x y" . yankpad-insert)
         ("C-x Y" . yankpad-capture-snippet))
  :custom (yankpad-file
           . (expand-file-name "yankpad.org" "~/sync"))
  :start yas-global-mode)

;; I'd rather not depend on this package, but whatever.

(bk-block* openwith
  :custom
  (openwith-associations
   . '(("\\.pdf\\'" "zathura"
        (file "-x" "emacsclient +%{line} %{input}"))))
  :start openwith-mode
  :config
  (with-eval-after-load 'mm-util
    (add-to-list 'mm-inhibit-file-name-handlers
                 'openwith-file-handler)))

(provide 'usability)

;;; usability.el ends here
