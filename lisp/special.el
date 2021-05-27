;;; special.el --- non-editor configurations -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(bk-block magit
  :requires .magit .forge .theist-mode
  :bind (("C-x g" . magit-status)
         (:magit-status-mode-map
          :package magit
          ("<return>" . magit-diff-visit-file-other-window)
          ("j" . magit-next-line)
          ("k" . magit-previous-line)
          ("v" . switch-mark-command)
          ("x" . theist-C-x)
          ("C-k" . magit-discard))))

(bk-block circe
  :requires .circe .auth-source
  :custom
  (circe-reduce-lurker-spam . t)
  (circe-network-defaults . nil)
  (circe-server-buffer-name . "{network}")
  (circe-znc-password . (auth-source-secret :user "leotaku^znc"))
  (circe-network-options
   . `(("freenode"
        :host "raw.le0.gs"
        :use-tls nil
        :port 6667
        :user "leotaku/freenode"
        :pass ,circe-znc-password)
       ("irchighway"
        :host "raw.le0.gs"
        :use-tls nil
        :port 6667
        :user "leotaku/irchighway"
        :pass ,circe-znc-password)))
  :config
  (add-hook
   'lui-mode-hook
   (lambda () (setq-local completion-in-region-function #'completion--in-region)))
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
      (setf (point) lui-input-marker)
    (apply fun args)))

;;; special.el ends here
