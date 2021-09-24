;;; load-packages.el --- load straight.el packages  -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

;; Emacs wants to load `package.el' before the init file,
;; so we do the same with `straight.el'

(setq straight-enable-use-package-integration nil
      straight-check-for-modifications '(find-when-checking check-on-save))

(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(require 'straight)
(require 'straight-x)

;; `package-set.el' loading mechanism

(let* ((contents
        (with-current-buffer
            (let ((default-directory user-emacs-directory))
              (find-file-noselect "package-set.el"))
          (goto-char (point-min))
          (prog1
              (read (current-buffer))
            (kill-buffer (current-buffer)))))
       (repos (nth 0 contents))
       (packages (nth 1 contents)))
  (setq straight-recipe-repositories (append repos nil))
  (let ((straight--allow-find t))
    (mapc 'straight-use-package packages)))

(provide 'load-packages)

;;; load-packages.el ends here
