;;; basics.el --- basic configuration for emacs -*- lexical-binding: t; -*-

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

(bk-block! save-place
  :config
  (save-place-mode 1))

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
  :requires .recentf no-littering
  :bind (("C-x l" . counsel-recentf))
  :custom
  (recentf-max-saved-items . 4000)
  (recentf-max-menu-items . 1000)
  :config
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory)
  (add-to-list 'recentf-exclude (getenv "TMPDIR"))
  (add-to-list 'recentf-exclude "/tmp"))

(bk-block dired
  :requires .dired .diredfl
  :bind ((:dired-mode-map
          :package dired
          ("j" . next-line)
          ("k" . previous-line)
          ("s" . swiper)
          ("e" . wdired-change-to-wdired-mode)
          ("DEL" . dired-up-directory)
          ("TAB" . dired-hide-details-mode)))
  :config
  (diredfl-global-mode))

(provide 'basics)

;;; basics.el ends here
