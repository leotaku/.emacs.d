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
          ("h" . calendar-backward-day)
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

(bk-block study
  :requires .study-okular .study-dired .study-deadgrep)

(bk-block ledger
  :requires .ledger-mode
  :hook
  (ledger-mode-hook . aggressive-indent-mode)
  :custom
  (ledger-post-amount-alignment-at . :decimal)
  (ledger-default-date-format . "%Y-%m-%d"))

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
  (circe-znc-password . (auth-source-secret :user "leotaku^znc"))
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

(defun auth-source-secret (&rest query)
  (let* ((matches (apply #'auth-source-search query))
         (secret (plist-get (car-safe matches) :secret)))
    (lambda (&rest _) (funcall secret))))

(defun circe-command-EXIT (&optional ignored)
  "Exit the current circe buffer."
  (interactive)
  (kill-buffer))

(defun advice-lui-send-input (fun &rest args)
  (if (< (point) lui-input-marker)
      (goto-char lui-input-marker)
    (apply fun args)))

;;; special.el ends here
