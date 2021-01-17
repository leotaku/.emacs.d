;;; basics.el --- basic configurations -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(bk-block emacs-basics
  :custom
  (disabled-command-function . nil)
  (save-interprogram-paste-before-kill . t)
  (inhibit-compacting-font-caches . t)
  :config
  (fset 'yes-or-no-p 'y-or-n-p))

(bk-block utf-8
  :config
  (prefer-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-language-environment 'utf-8)
  (set-selection-coding-system 'utf-8))

(bk-block fundamental
  :custom
  (major-mode . 'text-mode)
  :config
  (leaf-key "q" (lambda () (interactive)
                  (if (eq major-mode 'fundamental-mode)
                      (delete-window-and-buffer)
                    (insert "q")))))

(bk-block scratch
  :custom
  (initial-major-mode . 'emacs-lisp-mode))

(bk-block sensible-gui
  :custom
  (frame-resize-pixelwise . t)
  (custom-safe-themes . t)
  (frame-inhibit-implied-resize . t)
  :config
  (blink-cursor-mode -1))

(bk-block cursor
  :custom
  (cursor-type . '(bar . 2)))

(bk-block vc
  :custom
  (vc-follow-symlinks . t))

(bk-block backups
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
  (comment-auto-fill-only-comments . nil)
  :hook
  (prog-mode-hook . auto-fill-mode)
  (prog-mode-hook . hook-auto-fill-comments)
  (message-mode . auto-fill-mode)
  :config
  (defun hook-auto-fill-comments ()
    (setq-local comment-auto-fill-only-comments t)))

(bk-block show-paren
  :custom
  (show-paren-delay . 0)
  :config
  (show-paren-mode 1))

(bk-block show-trailing-whitespace
  :custom
  (show-trailing-whitespace . nil)
  :config
  (setq-mode-local
   text-mode
   show-trailing-whitespace t)
  (setq-mode-local
   prog-mode
   show-trailing-whitespace t))

(bk-block tabs
  :custom
  (indent-tabs-mode . nil)
  (tab-width . 4))

(bk-block save-place
  :config
  (save-place-mode 1))

(bk-block savehist
  :config
  (savehist-mode 1))

(bk-block trash
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

;;; basics.el ends here
