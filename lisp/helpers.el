;;; helpers.el --- small helper functions -*- lexical-binding: t -*-

;;; Commentary:
;; 

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

;;;; Expand-region 

(defun er/mark-line ()
  (interactive)
  (setf (point) (point-at-eol))
  (forward-char)
  (set-mark (point))
  (backward-char)
  (setf (point) (point-at-bol)))

(defun er/mark-block ()
  (interactive)
  (while (not (= (point-at-eol) (point-at-bol)))
    (forward-line))
  (forward-line -1)
  (setf (point) (point-at-eol))
  (set-mark (point))
  (while (not (= (point-at-eol) (point-at-bol)))
    (forward-line -1))
  (forward-line 1)
  (setf (point) (point-at-bol)))

;;;; Multiple-cursors

(defun mc/mark-down-or-more (arg)
  (interactive "p")
  (if (region-active-p)
      (dotimes (_ arg) (mc/mark-next-like-this 1))
    (mc/mark-next-lines arg)))

;;;; Editing commands

(defun kill-region-or-line (arg)
  (interactive "p")
  (if (region-active-p)
      (call-interactively 'kill-region)
    (kill-whole-line arg)))

(defun copy-region-or-line (arg)
  (interactive "p")
  (if (region-active-p)
      (call-interactively 'copy-region-as-kill)
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
    (fi-universal-quit)))

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

(defun split-window-left (&optional size)
  (interactive)
  (split-window-right size)
  (other-window 1))

(defun split-window-above (&optional size)
  (interactive)
  (split-window-below size)
  (other-window 1))

(provide 'helpers)

;;; helpers.el ends here
