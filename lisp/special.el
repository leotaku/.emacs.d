;;; special.el --- non-editor configurations -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(bk-block magit
  :requires .magit .forge .theist-mode
  :at-load (setq smerge-command-prefix (kbd "C-c s"))
  :bind (("C-x g" . magit-status)
         (:magit-status-mode-map
          :package magit
          ("<return>" . magit-diff-visit-file-other-window)
          ("j" . magit-next-line)
          ("k" . magit-previous-line)
          ("v" . motion-mark-cycle)
          ("x" . theist-C-x)
          ("C-k" . magit-discard)))
  :config
  (setq-mode-local
   gitignore-mode
   indent-line-function #'ignore))

(bk-block ediff
  :requires .ediff .winner
  :start winner-mode
  :custom
  (ediff-window-setup-function . #'ediff-setup-windows-plain)
  (ediff-split-window-function . #'split-window-horizontally)
  :config
  (add-hook 'ediff-after-quit-hook-internal #'winner-undo))

(bk-block calendar
  :requires .calendar .diary-lib local-files
  :custom (calendar-date-style . 'iso)
  :hook (diary-mode-hook . auto-revert-mode)
  :bind (("C-x c" . calendar)
         (:calendar-mode-map
          :package calendar
          ("ö" . calendar-backward-day)
          ("j" . calendar-forward-week)
          ("k" . calendar-backward-week)
          ("l" . calendar-forward-day)))
  :config
  (advice-add 'diary-make-entry :after #'advice-diary-make-entry)
  (setq-mode-local diary-mode indent-line-function #'ignore))

(defun advice-diary-make-entry (&rest _)
  (condition-case err
      (with-current-buffer (find-file-noselect diary-file)
        (motion-insert))
    (error (message "advice diary: %v" err))))

(bk-block dired
  :requires .dired .diredfl .dired-filter .theist-mode trash
  :bind ((:dired-mode-map
          :package dired
          ("j" . next-line)
          ("k" . previous-line)
          ("s" . swiper)
          ("x" . theist-C-x)
          ("d" . dired-do-delete)
          ("e" . wdired-change-to-wdired-mode)
          ("M" . dired-filter-mark-by-regexp)
          ("TAB" . dired-hide-details-mode)
          ("DEL" . dired-up-directory)))
  :custom
  (dired-filter-stack . '((dot-files) (omit)))
  (dired-clean-confirm-killing-deleted-buffers . nil)
  (dired-listing-switches . "-al --group-directories-first")
  (dired-filter-mark-prefix . "*")
  (dired-dwim-target . t)
  :hook
  (dired-mode-hook . dired-filter-mode)
  :config
  (advice-add 'dired-find-file :around #'advice-dired-find-file)
  (advice-add 'dired-up-directory :override #'advice-dired-up-directory)
  (diredfl-global-mode))

(defun advice-dired-find-file (fun &rest args)
  (interactive)
  (let ((file (dired-get-file-for-visit)))
    (if (file-directory-p file)
        (dired-find-alternate-file)
      (apply fun args))))

(defun advice-dired-up-directory ()
  (interactive)
  (find-alternate-file ".."))

(bk-block mail
  :custom
  (user-full-name . "Leo Georg Gaskin")
  (send-mail-function . #'sendmail-send-it)
  (message-send-mail-function . #'message-send-mail-with-sendmail))

(bk-block mu4e
  :requires mail .mu4e
  :custom
  (mu4e-get-mail-command . "mbsync -a")
  (mu4e-change-filenames-when-moving . t)
  :custom
  (mu4e-contexts
   . (list (make-mu4e-context
            :name "Kitchen Sink"
            :vars `((user-mail-address . ,(auth-source-pick-first-password :user "kitchen^mail"))
                    (mu4e-mail-subdir . "kitchen")))
           (make-mu4e-context
            :name "Personal"
            :vars `((user-mail-address . ,(auth-source-pick-first-password :user "personal^mail"))
                    (mu4e-mail-subdir . "personal")))
           (make-mu4e-context
            :name "Website"
            :vars `((user-mail-address . ,(auth-source-pick-first-password :user "website^mail"))
                    (mu4e-mail-subdir . "website")))
           (make-mu4e-context
            :name "University"
            :vars `((user-mail-address . ,(auth-source-pick-first-password :user "university^mail"))
                    (mu4e-mail-subdir . "university")))
           (make-mu4e-context
            :name "Outlook"
            :vars `((user-mail-address . ,(auth-source-pick-first-password :user "outlook^mail"))
                    (mu4e-mail-subdir . "outlook")))))
  :custom
  (mu4e-maildir-shortcuts
   . '((:maildir "/kitchen/inbox"    :key ?k)
       (:maildir "/personal/inbox"   :key ?p)
       (:maildir "/website/inbox"    :key ?w)
       (:maildir "/university/inbox" :key ?u)
       (:maildir "/outlook/inbox"    :key ?s)))
  (mu4e-bookmarks
   . '((:name "Today's messages"   :key ?t :query "date:today..now AND NOT flag:draft")
       (:name "Last 7 days"        :key ?w :query "date:7d..now AND NOT flag:draft")
       (:name "Important messages" :key ?i :query "prio:high or prio:medium AND NOT flag:trashed")
       (:name "Signed messages"    :key ?s :query "flag:signed AND NOT flag:trashed")
       (:name "Drafted messages"   :key ?d :query "flag:draft")
       (:name "Unread messages"    :key ?u :query "flag:unread" :hide t)))
  :custom
  (mu4e-sent-folder    . (lambda (msg) (file-name-concat "/" mu4e-mail-subdir "sent")))
  (mu4e-trash-folder   . (lambda (msg) (file-name-concat "/" mu4e-mail-subdir "trash")))
  (mu4e-drafts-folder  . (lambda (msg) (file-name-concat "/" mu4e-mail-subdir "drafts")))
  (mu4e-archive-folder . (lambda (msg) (file-name-concat "/" mu4e-mail-subdir "archive"))))

(bk-block study
  :requires .study-okular .study-dired .study-deadgrep
  :bind ((:dired-mode-map
          ("n" . study-next)
          ("p" . study-previous))))

(bk-block deadgrep
  :requires .deadgrep
  :custom
  (deadgrep-project-root-function
   . (lambda () default-directory))
  (deadgrep-extra-arguments
   . '("--no-config" "--sort=path")))

(bk-block ledger
  :requires .ledger-mode
  :hook
  (ledger-mode-hook . aggressive-indent-mode)
  :custom
  (ledger-post-amount-alignment-at . :decimal)
  (ledger-default-date-format . "%Y-%m-%d"))

(bk-block eat
  :requires .eat
  :bind ((:eat-semi-char-mode-map
          :package eat
          ("C-v" . eat-yank)
          ("C-<left>" . eat-self-input)
          ("C-<right>" . eat-self-input)
          ("<escape>" . eat-self-input)))
  :custom
  (shell-file-name . (executable-find "zsh"))
  (eat-kill-buffer-on-exit . t)
  (eat-shell-prompt-annotation-position . 'right-margin))

(bk-block circe
  :requires .circe .circe-color-nicks .auth-source
  :hook
  (circe-mode-hook . visual-line-mode)
  :bind ((:lui-mode-map
          :package lui
          ("<up>" . lui-previous-input)
          ("<down>" . lui-next-input)))
  :custom
  (circe-reduce-lurker-spam . t)
  (circe-network-defaults . nil)
  (circe-server-buffer-name . "{network}")
  (circe-znc-password . (auth-source-pick-first-password :user "leotaku^znc"))
  (circe-network-options
   . `(("libera"
        :host "raw.le0.gs"
        :use-tls t
        :port 6697
        :user "leotaku/libera"
        :pass ,circe-znc-password)
       ("irchighway"
        :host "raw.le0.gs"
        :use-tls t
        :port 6697
        :user "leotaku/irchighway"
        :pass ,circe-znc-password)))
  :config
  (enable-circe-color-nicks)
  (advice-add 'lui-send-input :around #'advice-lui-send-input))

(defun circe-command-EXIT (&optional ignored)
  "Exit the current circe buffer."
  (interactive)
  (kill-buffer))

(defun advice-lui-send-input (fun &rest args)
  (if (< (point) lui-input-marker)
      (goto-char lui-input-marker)
    (apply fun args)))

;;; special.el ends here
