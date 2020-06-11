;;; visual.el --- visual configurations -*- lexical-binding: t; -*-

;;; Commentary:
;; 

;;; Code:

(bk-block fonts
  :wanted-by theme-target
  :config
  (custom-set-faces
   '(variable-pitch ((t (:font "Alegreya SC" :height 110))))
   '(mode-line ((t (:font "Alegreya" :height 120))))
   '(mode-line-inactive ((t (:font "Alegreya" :height 120))))
   '(default ((t (:font "Fira Mono" :height 110 :weight regular)))))
  :config
  (fi-with-gui
   (set-fontset-font t 'symbol (font-spec :family "JoyPixels") nil t)))

(bk-block0 theme
  :wanted-by theme-target
  :requires .doom-themes
  :config
  (load-theme 'doom-one)
  '(fi-with-gui
    (let ((line (face-attribute 'mode-line :underline)))
      (set-face-attribute 'mode-line          nil :overline   line)
      (set-face-attribute 'mode-line-inactive nil :overline   line)
      (set-face-attribute 'mode-line-inactive nil :underline  line)
      (set-face-attribute 'mode-line          nil :box        nil)
      (set-face-attribute 'mode-line-inactive nil :box        nil))))

(bk-block mode-line-other
  :wanted-by theme-target
  :config
  (column-number-mode))

(bk-block* moody
  :wanted-by theme-target
  :custom
  (moody-mode-line-height . 50)
  (x-underline-at-descent-line . t)
  :config
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))

(bk-block* minions
  :wanted-by theme-target
  :config
  (minions-mode))

;;; visual.el ends here
