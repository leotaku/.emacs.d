;;; org-cfg.el --- org-mode related configurations -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(bk-block org
  :requires .org .org-tempo .org-timer worf org-capture
  :hook (org-mode-hook . auto-revert-mode)
  :bind (("C-x SPC" . org-agenda))
  :custom
  (org-adapt-indentation . nil)
  (org-tags-column . 0)
  (org-blank-before-new-entry
   . '((heading . nil) (plain-list-item . nil)))
  (org-reverse-datetree-entry-time
   . '((property "CREATED")))
  (org-link-parameters
   . (assoc-delete-all
      "file+" org-link-parameters
      (lambda (it key) (string-prefix-p key it)))))

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
  (let ((map worf-mode-map))
    (worf-define-key map "x" #'theist-C-x)
    (worf-define-key map "z" #'theist-C-c)
    (worf-define-key map "P" #'org-priority)))

(bk-block0 org-capture
  :requires local-files .org-capture .org-protocol .org-reverse-datetree
  :custom
  (org-capture-templates
   . '(("w" "Capture templates using org-protocol")
       ("ww" "web-capture" item (file+headline things-file "Capture")
        "+ [[%:link][%(string-trim-right \"%:description\" \"\s*[-–|].*\")]]"
        :immediate-finish t)
       ("wc" "web-context" item (file+headline things-file "Capture")
        "+ [[%:link][%(string-trim-right \"%:description\" \"\s*[-–|].*\")]] :: %i"
        :immediate-finish t)))
  :config
  (add-hook 'org-capture-mode-hook #'motion-insert))

;;; New functionality

(defun org-journal (arg)
  "Visit journal entry for current day.
Offer day selection when ARG is non-nil."
  (interactive "P")
  (with-current-buffer (find-file-noselect journal-file)
    (save-excursion
      (if arg
          (org-reverse-datetree-goto-read-date-in-file)
        (org-reverse-datetree-goto-date-in-file))
      (org-tree-to-indirect-buffer)))
  (select-window (get-buffer-window org-last-indirect-buffer))
  (org-map-entries #'outline-hide-subtree nil 'tree)
  (org-cycle)
  (run-hooks 'org-capture-mode-hook)
  (goto-char (point-at-bol))
  (quick-commit-mode))

(defun org-refile-to-journal (arg)
  "Refile subtree to journal based on org-reverse-datetree logic.
Force day selection when ARG is non-nil."
  (interactive "P")
  (org-reverse-datetree-refile-to-file journal-file arg))

(define-minor-mode quick-commit-mode nil
  :lighter "Quick"
  :keymap '(("\C-c\C-c" . quick-commit-buffer)
            ("\C-c\C-k" . kill-buffer-and-window)))

(defun quick-commit-buffer ()
  "Save the current buffer, then kill it and its window."
  (interactive)
  (save-buffer)
  (kill-buffer-and-window))

;;; org-cfg.el ends here
