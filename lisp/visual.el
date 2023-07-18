;;; visual.el --- visual configurations -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(bk-block fonts
  :config
  (fi-with-gui
   (set-face-attribute 'default nil :font "Source Code Pro" :height 100)
   (set-face-attribute 'variable-pitch nil :font "Alegreya" :height 110)
   (set-face-attribute 'mode-line-inactive nil :font "Alegreya" :height 110)
   (set-face-attribute 'mode-line nil :font "Alegreya" :height 110)))

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
  (moody-mode-line-height . 35)
  (x-underline-at-descent-line . t)
  (minions-mode-line-lighter . ";-")
  :config
  (tracking-mode 1)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-element
   '(vc-mode vc-mode)
   '(vc-mode ((:propertize "." invisible t) moody-vc-mode)))
  (minions-mode 1)
  (column-number-mode 1))

;;; visual.el ends here
