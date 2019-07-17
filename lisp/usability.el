;;; usability.el --- basic usability packages for emacs

;;; Commentary:
;; 

;;; Code:

(leaf visual-regexp
  :straight t visual-regexp-steroids pcre2el
  :require visual-regexp-steroids
  :custom
  (vr/engine . 'pcre2el)
  ;; :load "lisp/visual-regexp.el"
  )

(leaf ispell
  :bind (("C-." . ispell-word))
  :custom
  (ispell-dictionary . "en_US")
  (ispell-program-name . "aspell")
  (ispell-really-hunspell . nil)
  (ispell-silently-savep . t))

(leaf which-key
  :straight t
  :config (which-key-mode)
  ;; :start which-key-mode
  )

(leaf ivy
  :straight swiper
  :bind ((:ivy-minibuffer-map
          :package ivy
          ("H-i" . ivy-insert-selection)))
  :config (ivy-mode)
  ;; :start ivy-mode
  :custom
  (ivy-use-selectable-prompt . t)
  :config
  (defun ivy-insert-selection ()
    (interactive)
    (ivy-exit-with-action
     (lambda (it)
       (interactive)
       (insert it)
       (signal 'quit nil)))))

(leaf counsel
  :straight t
  :bind (("C-s" . swiper-isearch)
         (:counsel-describe-map
          :package counsel
          ("C-h" . counsel-lookup-symbol)))
  :config (counsel-mode)
  ;; :start counsel-mode
  :config
  (defun counsel-lookup-symbol ()
    "Lookup the current symbol in the help docs."
    (interactive)
    (ivy-exit-with-action
     (lambda (x)
       (if (featurep 'helpful)
           (helpful-symbol (intern x))
         (describe-symbol (intern x))
         (signal 'quit nil))))))

(leaf projectile
  :straight t counsel-projectile
  :leaf-defer t
  :init
  (fi-auto-keymap (kbd "C-x p") 'projectile-command-map 'projectile)
  :custom
  (projectile-completion-system . 'ivy)
  (projectile-project-root-files-functions . '(projectile-root-top-down))
  (projectile-project-root-files . '(".git" ".bzr" ".svn" ".hg" "_darcs" ".projectile"))
  :config
  (projectile-mode)
  (counsel-projectile-mode)
  ;; :start projectile-mode counsel-projectile-mode
  )

(leaf amx
  :straight t
  :config (amx-mode)
  ;; :start amx-mode
  )

(leaf undohist
  :straight t
  :require undohist
  :config
  (setq undohist-ignored-files  '("COMMIT_EDITMSG"))
  (setq undohist-directory (no-littering-expand-var-file-name "undohist"))
  :config
  (undohist-initialize))

(leaf yankpad
  :straight t yasnippet
  :leaf-defer t
  :require yasnippet
  :bind (("C-x y" . yankpad-insert)
         ("C-x Y" . yankpad-capture-snippet))
  :config
  (setq yankpad-file (expand-file-name "yankpad.org" "~")))

(provide 'usability)

;;; usability.el ends here
