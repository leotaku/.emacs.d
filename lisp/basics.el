;;; basics.el --- basic configuration for Emacs builtin

;;; Commentary:
;; 

;;; Code:

(leaf sensible-gui
  :custom
  (frame-resize-pixelwise . t)
  (custom-safe-themes . t)
  :config
  (blink-cursor-mode -1))

(leaf cursor
  :custom
  (cursor-type . '(bar . 2)))

(leaf yes-or-no-query
  :config
  (fset 'yes-or-no-p 'y-or-n-p))

(leaf vc
  :custom
  (vc-follow-symlinks . t))

(leaf backups
  :custom
  (backup-by-copying . t)
  (delete-old-versions . t)
  (kept-new-versions . 6)
  (kept-old-versions . 2)
  (version-control . t))

(leaf truncate-lines
  :custom
  (truncate-lines . t))

(leaf show-paren
  :custom
  (show-paren-delay . 0)
  :config
  (show-paren-mode 1))

(leaf tabs
  :custom
  (indent-tabs-mode . nil)
  (tab-width . 4))

(leaf savehist
  :config
  (savehist-mode 1))

(leaf help
  :bind ((:help-mode-map
          ("j" . next-line)
          ("k" . previous-line)))
  :custom
  (help-window-select . t))

(leaf recentf
  :bind (("C-x l" . counsel-recentf))
  :require recentf
  :custom
  (recentf-max-saved-items . 4000)
  (recentf-max-menu-items . 1000)
  :config
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory))

(leaf dired
  :bind ((:dired-mode-map
          :package dired
          ("j" . next-line)
          ("k" . previous-line)
          ("s" . swiper)
          ("DEL" . dired-up-directory))))

(provide 'basics)

;;; basics.el ends here
