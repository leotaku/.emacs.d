;;; basics.el --- basic configuration for emacs

;;; Commentary:
;; 

;;; Code:

(bk-block! fundamental
  :custom
  (major-mode . 'text-mode)
  :config
  (leaf-key "q" (lambda () (interactive)
                  (if (eq major-mode 'fundamental-mode)
                      (delete-window-or-frame)
                    (insert "q")))))

(bk-block0 scratch
  :custom
  (initial-major-mode . 'emacs-lisp-mode))

(bk-block sensible-gui
  :custom
  (frame-resize-pixelwise . t)
  (custom-safe-themes . t)
  :config
  (blink-cursor-mode -1))

(bk-block0 cursor
  :custom
  (cursor-type . '(bar . 2)))

(bk-block! yes-or-no-query
  :config
  (fset 'yes-or-no-p 'y-or-n-p))

(bk-block0 vc
  :custom
  (vc-follow-symlinks . t))

(bk-block0 backups
  :custom
  (backup-by-copying . t)
  (delete-old-versions . t)
  (kept-new-versions . 6)
  (kept-old-versions . 2)
  (version-control . t))

(bk-block truncate-lines
  :custom
  (truncate-lines . t)
  :hook
  (text-mode-hook . visual-line-mode))

(bk-block auto-fill
  :custom
  (comment-auto-fill-only-comments . t)
  :hook
  (prog-mode-hook . auto-fill-mode)
  (message-mode . auto-fill-mode))

(bk-block show-paren
  :custom
  (show-paren-delay . 0)
  :config
  (show-paren-mode 1))

(bk-block0 tabs
  :custom
  (indent-tabs-mode . nil)
  (tab-width . 4))

(bk-block! savehist
  :config
  (savehist-mode 1))

(bk-block0 help
  :bind ((:help-mode-map
          ("j" . next-line)
          ("k" . previous-line)))
  :custom
  (help-window-select . t))

(bk-block! recentf
  :bind (("C-x l" . counsel-recentf))
  :requires .recentf no-littering
  :custom
  (recentf-max-saved-items . 4000)
  (recentf-max-menu-items . 1000)
  :config
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory))

(bk-block dired
  :bind ((:dired-mode-map
          :package dired
          ("j" . next-line)
          ("k" . previous-line)
          ("s" . swiper)
          ("DEL" . dired-up-directory)
          ("TAB" . dired-hide-details-mode)))
  :config
  (diredfl-global-mode))

(bk-block openwith
  "I'd rather not depend on this package, but whatever."
  :config
  (openwith-mode t)
  (setq openwith-associations
        '(("\\.pdf\\'" "zathura" (file)))))

(provide 'basics)

;;; basics.el ends here
