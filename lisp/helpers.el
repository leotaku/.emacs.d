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

;;;; Window management commands

(defun delete-window-or-frame ()
  (interactive)
  (unless (ignore-errors (delete-window) t)
    (delete-frame)))

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
       (insert (file-name-shortest (format "%s" it))))
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
