;;; helpers.el --- small helper functions -*- lexical-binding: t -*-

;;; Commentary:

;;;; Modalka

(defmacro modalka-reserve (transforms &rest keys)
  (declare (indent 1))
  `(modalka-multiplex
       (lambda () (interactive))
       ,transforms
     ,@keys))

(defmacro modalka-multiplex (command transforms &rest keys)
  (declare (indent 2))
  `(dolist (key ',keys)
     (dolist (transform ',transforms)
       (define-key modalka-mode-map
         (kbd (funcall transform key)) ,command))))

(defmacro modalka-keys (&rest keys)
  `(dolist (pair ',keys)
     (pcase pair
       (`(,key . ,(and command (pred symbolp)))
        (define-key modalka-mode-map (kbd key) command))
       (`(,key . ,(and command (pred listp)))
        (define-key modalka-mode-map (kbd key) `(lambda (&optional arg) (interactive "p") ,@command)))
       (otherwise (error "Invalid entry: %S" pair)))))

(defun modalka-deactivate ()
  (interactive)
  (modalka-mode -1))

;;;; Editing commands

(defun kill-region-or-line (arg)
  (interactive "p")
  (if (region-active-p)
      (call-interactively #'kill-region)
    (kill-whole-line arg)))

(defun copy-region-or-line (arg)
  (interactive "p")
  (if (region-active-p)
      (call-interactively #'copy-region-as-kill)
    (let ((begin (point-at-bol)))
      (save-excursion
        (forward-line arg)
        (copy-region-as-kill begin (point-at-bol))))))

(defun goto-or-quit (arg)
  (interactive "P")
  (if (and (not (bound-and-true-p multiple-cursors-mode))
           (numberp arg))
      (if (> arg 0)
          (goto-line arg)
        (goto-line (+ arg (line-number-at-pos (point-max)))))
    (if (bound-and-true-p multiple-cursors-mode)
        (mc/keyboard-quit)
      (keyboard-quit))))

(defun switch-mark-command ()
  (interactive)
  (if (region-active-p)
      (if (null rectangle-mark-mode)
          (rectangle-mark-mode)
        (deactivate-mark))
    (set-mark-command nil)))

;;;; Window management commands

(defun delete-window-or-frame ()
  (interactive)
  (unless (ignore-errors (delete-window) t)
    (unless (ignore-errors (delete-frame) t)
      (save-buffers-kill-emacs))))

(defun delete-window-or-buffer ()
  (interactive)
  (if (= (count-windows) 1)
      (kill-buffer)
    (delete-window)))

(defun delete-window-and-buffer ()
  (interactive)
  (if (= (count-windows) 1)
      (kill-buffer)
    (let ((b (current-buffer)))
      (delete-window)
      (kill-buffer b))))

(defun delete-if-fundamental ()
  (interactive)
  (if (and (eq major-mode 'fundamental-mode)
           (not (active-minibuffer-window)))
      (delete-window-and-buffer)
    (insert (char-to-string last-command-event))))

(defun split-window-left (&optional size)
  (interactive)
  (split-window-right size)
  (other-window 1))

(defun split-window-above (&optional size)
  (interactive)
  (split-window-below size)
  (other-window 1))

;;;; Org-mode

(defun org-clean-description (str)
  (car (split-string str " [-–|]" t)))

;;;; Lispy

(defun conditionally-enable-lispy ()
  (when (eq this-command 'eval-expression)
    (setq-local completion-in-region-function #'completion--in-region)
    (setq-local indent-line-function #'lisp-indent-line)
    (lispy-mode 1)))

;;;; Visual-fill-column

(defun visual-fill-column-warn-fill ()
  (interactive)
  (message "Auto-fill should not be used with visual-fill-column"))

;;;; Ivy

(defun ivy-insert-selection ()
  (interactive)
  (ivy-exit-with-action
   (lambda (it)
     (interactive)
     (insert (file-name-shortest it))
     (signal 'quit nil))))

(defun counsel-lookup-symbol ()
  "Lookup the current symbol in the help docs."
  (interactive)
  (ivy-exit-with-action
   (lambda (x)
     (if (featurep 'helpful)
         (helpful-symbol (intern x))
       (describe-symbol (intern x))
       (signal 'quit nil)))))

;;;; Dired

(defun dired-better-find-file ()
  (interactive)
  (let ((file (dired-get-file-for-visit)))
    (if (file-directory-p file)
        (find-alternate-file file)
      (find-file file))))

(defun dired-better-up-directory ()
  (interactive)
  (find-alternate-file ".."))

;;;; Development servers

(defun lsp-maybe ()
  (when (lsp-workspace-root)
    (lsp-deferred)))

(defun tide-maybe ()
  (when (member (file-name-extension buffer-file-name)
                '("tsx" "ts" "jsx" "js"))
    (run-with-timer 0 nil 'tide-setup)))

;;;; Generic

(defun file-name-shortest (file)
  "Return the shortest possible FILE name.
Expands relative to `default-directory' and the home directory."
  (let ((rel (file-relative-name file))
        (home (concat "~/" (file-relative-name file "~"))))
    (car (seq-sort-by 'length '< (list rel home file)))))

;;; helpers.el ends here
