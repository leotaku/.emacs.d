;;; org-cfg.el --- org-mode related configurations -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(bk-block org
  :wanted-by delayed-target
  :requires .org .org-tempo worf org-capture
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

(bk-block0 local-files
  :at-load
  (defun expand-sync-file (name)
    (expand-file-name name sync-directory))
  :custom
  (sync-directory . "~/sync")
  (todo-file . (expand-sync-file "homework.org"))
  (things-file . (expand-sync-file "things.org"))
  (journal-file . (expand-sync-file "journal.org")))

(bk-block yankpad
  :wanted-by delayed-target
  :requires local-files projectile .org .yankpad .yasnippet
  :bind (("C-x y" . yankpad-insert)
         ("C-x Y" . yankpad-capture-snippet))
  :custom (yankpad-file
           . (expand-sync-file "yankpad.org"))
  :start yas-global-mode)

(bk-block0 org-capture
  :requires local-files .org-capture .org-protocol .org-reverse-datetree
  :custom
  (org-capture-templates
   . '(("t" "todo" entry (file todo-file)
        "* TODO %?\n:PROPERTIES:\nDATE: %U\n:END:")
       ("w" "Capture templates using org-protocol")
       ("ww" "web-capture" item (file+headline things-file "Capture")
        "+ [[%:link][%(org-clean-description \"%:description\")]]"
        :immediate-finish t)
       ("wc" "web-context" item (file+headline things-file "Capture")
        "+ [[%:link][%(org-clean-description \"%:description\")]] :: %i"
        :immediate-finish t)
       ("wt" "web-todo" entry (file todo-file)
        "* TODO %i"
        :immediate-finish t)))
  :config
  (add-hook 'org-capture-mode-hook 'modalka-deactivate))


;;; org-cfg.el ends here
