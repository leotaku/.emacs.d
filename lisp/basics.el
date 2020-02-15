;;; basics.el --- basic configurations -*- lexical-binding: t; -*-

;;; Commentary:
;; 

;;; Code:

(bk-block! emacs-basics
  :custom
  (disabled-command-function . nil)
  (save-interprogram-paste-before-kill . t)
  :config
  (fset 'yes-or-no-p 'y-or-n-p))

(bk-block! fundamental
  :custom
  (major-mode . 'text-mode)
  :config
  (leaf-key "q" (lambda () (interactive)
                  (if (eq major-mode 'fundamental-mode)
                      (delete-window-or-buffer)
                    (insert "q")))))

(bk-block! scratch
  :custom
  (initial-major-mode . 'emacs-lisp-mode))

(bk-block! sensible-gui
  :custom
  (frame-resize-pixelwise . t)
  (custom-safe-themes . t)
  (frame-inhibit-implied-resize . t)
  :config
  (blink-cursor-mode -1))

(bk-block! cursor
  :custom
  (cursor-type . '(bar . 2)))

(bk-block! vc
  :custom
  (vc-follow-symlinks . t))

(bk-block! backups
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

(bk-block! tabs
  :custom
  (indent-tabs-mode . nil)
  (tab-width . 4))

(bk-block! save-place
  :config
  (save-place-mode 1))

(bk-block! savehist
  :config
  (savehist-mode 1))

(bk-block! trash
  :requires .async
  :custom
  (delete-by-moving-to-trash . t)
  (trash-directory . nil)
  :config
  (fmakunbound 'system-move-file-to-trash))

(bk-block help
  :bind ((:help-mode-map
          ("j" . next-line)
          ("k" . previous-line)))
  :custom
  (help-window-select . t))

(bk-block! recentf
  :requires .recentf
  :bind (("C-x l" . counsel-recentf))
  :at-load
  (setq recentf-max-saved-items 4000)
  (setq recentf-max-menu-items 1000)
  :config
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory)
  (add-to-list 'recentf-exclude (getenv "TMPDIR"))
  (add-to-list 'recentf-exclude "/tmp"))

(bk-block dired
  :requires .dired .diredfl .dired-filter trash
  :bind ((:dired-mode-map
          :package dired
          ("j" . next-line)
          ("k" . previous-line)
          ("s" . swiper)
          ("e" . wdired-change-to-wdired-mode)
          ("DEL" . dired-up-directory)
          ("TAB" . dired-hide-details-mode)))
  :custom
  (dired-filter-stack . '((omit) (dot-files)))
  (dired-clean-confirm-killing-deleted-buffers . nil)
  :hook
  (dired-mode-hook . dired-filter-mode)
  :config
  (diredfl-global-mode))

(provide 'basics)

;;; basics.el ends here
