(leaf solarized-theme
  :straight t
  :config
  (fi-configure-gui
   (load-theme 'solarized-light)
   (let ((line (face-attribute 'mode-line :underline)))
     (set-face-attribute 'mode-line          nil :overline   line)
     (set-face-attribute 'mode-line-inactive nil :overline   line)
     (set-face-attribute 'mode-line-inactive nil :underline  line)
     (set-face-attribute 'mode-line          nil :box        nil)
     (set-face-attribute 'mode-line-inactive nil :box        nil)
     (set-face-attribute 'mode-line-inactive nil :background "#FDF6E3")
     (set-face-attribute 'mode-line nil          :background "#f9f2d9"))))

(leaf mode-line-other
  ;; :start column-number-mode
  :config
  (column-number-mode))

(leaf moody
  :straight t
  :custom
  (moody-mode-line-height . 50)
  (x-underline-at-descent-line . t)
  :config
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))

(leaf minions
  :straight t
  :after moody
  ;; :start minions-mode
  :config
  (minions-mode))
