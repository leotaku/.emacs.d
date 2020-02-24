;;; org-cfg.el --- org-mode related configurations -*- lexical-binding: t; -*-

;;; Commentary:
;; 

;;; Code:

(bk-block org
  :wanted-by delayed-target
  :requires .org .org-tempo .org-cliplink worf org-capture
  :hook
  (org-mode-hook . auto-revert-mode)
  :custom
  (org-adapt-indentation . nil)
  (org-tags-column . 0)
  (org-blank-before-new-entry
   . '((heading . nil) (plain-list-item . nil)))
  :config
  (set-face-attribute
   'org-document-title nil
   :inherit 'variable-pitch
   :height 150))

(bk-block0 worf
  :requires .worf .theist-mode
  :hook (org-mode-hook . worf-mode)
  :bind ((:worf-mode-map
          :package worf
          ("[" . nil)
          ("]" . nil)
          ("<backtab>" . nil)
          ("<S-iso-lefttab>" . nil)))
  :config
  (worf-define-key
   worf-mode-map
   (kbd "x") 'theist-C-x)
  (worf-define-key
   worf-mode-map
   (kbd "z")
   'theist-C-c))

(bk-block0 org-capture
  :requires .org-capture .org-protocol
  :custom
  (org-capture-templates
   . '(("j" "journal" entry (file+function "~/sync/things.org" org-reverse-datetree-goto-date-in-file)
        "* TODO %?\n:PROPERTIES:\nDATE: %U\n:END:")
       ("t" "todo" entry (file "~/sync/homework.org")
        "* TODO %?\n:PROPERTIES:\nDATE: %U\n:END:")
       ("w" "Capture templates using org-protocol")
       ("ww" "web-capture" item (file+headline "~/sync/things.org" "Capture")
        "+ [[%(org-clean-link \"%:link\")][%(org-clean-description \"%:description\")]]"
        :immediate-finish t)
       ("wc" "web-context" item (file+headline "~/sync/things.org" "Capture")
        "+ [[%(org-clean-link \"%:link\")][%(org-clean-description \"%:description\")]] :: %i"
        :immediate-finish t)
       ("wt" "web-todo" entry (file "~/sync/homework.org")
        "* TODO %i"
        :immediate-finish t)))
  :config
  (add-hook 'org-capture-mode-hook 'modalka-deactivate))

(bk-block yankpad
  :wanted-by delayed-target
  :requires .org .yankpad .yasnippet projectile
  :bind (("C-x y" . yankpad-insert)
         ("C-x Y" . yankpad-capture-snippet))
  :custom (yankpad-file
           . (expand-file-name "yankpad.org" "~/sync"))
  :start yas-global-mode)

;;; org-cfg.el ends here
