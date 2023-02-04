;;; helpers.el --- small helper functions -*- lexical-binding: t -*-

;;; Commentary:

;;;; Modalka

(defmacro modalka-reserve (transforms &rest keys)
  (declare (indent 1))
  `(modalka-multiplex #'ignore ,transforms ,@keys))

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
       (otherwise (error "Not a valid binding: %S" pair)))))

;;;; Motion

(defcustom motion-mode-function #'ignore
  "Mode function used for motions that change the editing mode.")

(defun motion-insert ()
  (interactive)
  (funcall motion-mode-function -1))

(defun motion-append ()
  (interactive)
  (when (/= (point) (point-at-eol))
    (forward-char))
  (motion-insert))

(defun motion-Insert ()
  (interactive)
  (back-to-indentation)
  (motion-insert))

(defun motion-Append ()
  (interactive)
  (end-of-line)
  (motion-insert))

(defun motion-change (arg)
  (interactive "p")
  (kill-region-or-line arg)
  (motion-insert))

(defun motion-forward-word (arg)
  (interactive "p")
  (motion-syntax arg nil "[:word:]" "^[:word:]"))

(defun motion-forward-end (arg)
  (interactive "p")
  (motion-syntax arg t "^[:word:]" "[:word:]"))

(defun motion-backward-word (arg)
  (interactive "p")
  (motion-forward-end (- arg)))

(defun motion-forward-Word (arg)
  (interactive "p")
  (motion-syntax arg nil "^[:space:]\n" "[:space:]\n"))

(defun motion-forward-End (arg)
  (interactive "p")
  (motion-syntax arg t "[:space:]\n" "^[:space:]\n"))

(defun motion-backward-Word (arg)
  (interactive "p")
  (motion-forward-End (- arg)))

(defun motion-syntax (n reverse-adjust &rest syntaxes)
  (let ((f (if (< 0 n) #'skip-chars-forward #'skip-chars-backward)))
    (if reverse-adjust (when (< 0 n) (forward-char)) (when (> 0 n) (backward-char)))
    (dotimes (_ (abs n)) (mapc f syntaxes))
    (if reverse-adjust (and (not (region-active-p)) (< 0 n) (backward-char)) (when (> 0 n) (backward-char)))))

(defvar-local motion-last-char nil)
(defvar-local motion-last-count nil)
(defvar-local motion-last-until nil)

(defun motion-goto-char (arg)
  (interactive "p")
  (let ((char (char-to-string (read-char))))
    (goto-char (motion-find-char char arg))
    (setq motion-last-char char)
    (setq motion-last-count arg)
    (setq motion-last-until nil)))

(defun motion-till-char (arg)
  (interactive "p")
  (let ((char (char-to-string (read-char))))
    (goto-char (motion-find-char char arg t))
    (setq motion-last-char char)
    (setq motion-last-count arg)
    (setq motion-last-until t)))

(defun motion-repeat-char (arg)
  (interactive "p")
  (when (null motion-last-char)
    (error "No previous jump that can be repeated"))
  (let* ((sign (/ motion-last-count (abs motion-last-count)))
         (point (motion-find-char motion-last-char sign motion-last-until)))
    (goto-char point)))

(defun motion-find-char (char count &optional until)
  (save-excursion
    (forward-char (if (< 0 count) (if until 2 1) (if until -2 -1)))
    (search-forward char nil nil count)
    (backward-char (if (< 0 count) (if until 2 1) (if until -1 0)))
    (point)))

;;;; Editing commands

(defun replace-char (arg)
  (interactive "p")
  (let ((char (char-to-string (read-char))))
    (save-excursion
      (dotimes (_ arg) (delete-char 1) (insert char)))))

(defun kill-ring-uncycle (arg)
  (interactive "p")
  (kill-ring-cycle (- arg)))

(defun kill-ring-cycle (arg)
  (interactive "p")
  (let* ((length (seq-length kill-ring))
         (to-append (% (+ length arg) length))
         (_ (current-kill 0)))
    (setq kill-ring (append (seq-drop kill-ring to-append)
                            (seq-take kill-ring to-append)))))

(defun yank-put-before (arg)
  (interactive "p")
  (dotimes (_ arg) (yank-put nil)))

(defun yank-put-after (arg)
  (interactive "p")
  (dotimes (_ arg) (yank-put t)))

(defun yank-put (after-p)
  (let ((kill (current-kill 0))
        (column (current-column)))
    (if (string-suffix-p "\n" kill)
        (progn
          (beginning-of-line)
          (if after-p
              (if (and (/= (point-at-eol) (point-max))
                       (/= (point-at-bol) (point-min)))
                  (insert kill)
                (end-of-line)
                (insert "\n" (string-remove-suffix "\n" kill)))
            (insert kill))
          (move-to-column column))
      (insert kill)
      (when after-p (backward-char (length kill))))))

(defun kill-region-or-line (arg)
  (interactive "p")
  (if (region-active-p)
      (call-interactively #'kill-region)
    (kill-whole-line arg)))

(defun copy-region-or-line (arg)
  (interactive "p")
  (if (region-active-p)
      (call-interactively #'copy-region-as-kill)
    (let ((begin (point-at-bol))
          (adapt (if (= arg 0) 0 (/ arg (abs arg)))))
      (save-excursion
        (condition-case nil (forward-line (- arg adapt)) (quit))
        (copy-region-as-kill begin (point-at-eol))
        (kill-append "\n" nil)))))

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

(defun split-window-left (&optional size)
  (interactive)
  (split-window-right size)
  (other-window 1))

(defun split-window-above (&optional size)
  (interactive)
  (split-window-below size)
  (other-window 1))

;;;; Ivy

(defun ivy-insert-selection ()
  (interactive)
  (ivy-exit-with-action
   (lambda (it)
     (let ((it (or (car-safe it) it)))
       (insert (file-name-shortest (if (stringp it) it (format "%s" it)))))
     (signal 'quit nil))))

(defun ivy-lookup-symbol ()
  "Lookup the current symbol in the help docs."
  (interactive)
  (ivy-exit-with-action
   (lambda (it)
     (let ((it (or (car-safe it) it)))
       (describe-symbol (if (symbolp it) it (intern it))))
     (signal 'quit nil))))

;;;; Generic

(defun file-name-shortest (file)
  "Return the shortest possible FILE name.
Expands relative to `default-directory' and the home directory."
  (let ((rel (file-relative-name file))
        (home (concat "~/" (file-relative-name file "~"))))
    (car (seq-sort-by 'length '< (list rel home file)))))

(defun file-contained-in-symlink-p (file)
  "Return non-nil if one of the segments of FILE is a symbolic link."
  (while (and file (not (file-symlink-p (directory-file-name file))))
    (setq file (file-name-parent-directory file)))
  file)

;;; helpers.el ends here
