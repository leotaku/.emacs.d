;;; visual.el --- visual configurations -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(bk-block fonts
  :config
  (fi-with-gui
   (set-face-attribute 'default nil :font "Fira Mono" :height 110)
   (set-face-attribute 'variable-pitch nil :font "Alegreya SC" :height 110)
   (set-face-attribute 'mode-line-inactive nil :font "Alegreya" :height 120)
   (set-face-attribute 'mode-line nil :font "Alegreya" :height 120)))

(bk-block theme
  :requires .doom-themes
  :config
  (dolist (theme custom-enabled-themes)
    (disable-theme theme))
  (load-theme 'doom-aurora))

(bk-block icons
  :custom (icon-preference . '(text))
  :config
  (fi-with-gui
   (face-spec-reset-face 'icon-button)
   (set-face-attribute 'icon-button nil :inherit 'button :box t)))

(bk-block mode-line
  :requires .moody .minions .tracking
  :custom
  (moody-mode-line-height . 50)
  (x-underline-at-descent-line . t)
  :config
  (tracking-mode)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-element
   '(vc-mode vc-mode)
   '(vc-mode ((:propertize "." invisible t) moody-vc-mode)))
  (minions-mode)
  (column-number-mode))

;;; visual.el ends here
