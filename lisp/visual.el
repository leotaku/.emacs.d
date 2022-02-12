;;; visual.el --- visual configurations -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(bk-block fonts
  :config
  (custom-set-faces
   '(variable-pitch ((t (:font "Alegreya SC" :height 110))))
   '(mode-line ((t (:font "Alegreya" :height 120))))
   '(mode-line-inactive ((t (:font "Alegreya" :height 120))))
   '(default ((t (:font "Fira Mono" :height 110 :weight regular))))))

(bk-block theme
  :requires .doom-themes
  :config
  (load-theme 'doom-aurora)
  '(fi-with-gui
    (let ((line (face-attribute 'mode-line :underline)))
      (set-face-attribute 'mode-line          nil :overline   line)
      (set-face-attribute 'mode-line-inactive nil :overline   line)
      (set-face-attribute 'mode-line-inactive nil :underline  line)
      (set-face-attribute 'mode-line          nil :box        nil)
      (set-face-attribute 'mode-line-inactive nil :box        nil))))

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
