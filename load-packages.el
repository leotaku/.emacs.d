;;; load-packages.el --- load elpaca.el packages  -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

;; Emacs wants to load `package.el' before the init file,
;; so we do the same with `elpaca.el'

(declare-function elpaca-generate-autoloads "elpaca")
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(when-let ((elpaca-repo (expand-file-name "repos/elpaca/" elpaca-directory))
           (elpaca-build (expand-file-name "elpaca/" elpaca-builds-directory))
           (elpaca-target (if (file-exists-p elpaca-build) elpaca-build elpaca-repo))
           (elpaca-url  "https://www.github.com/progfolio/elpaca.git")
           ((add-to-list 'load-path elpaca-target))
           ((not (file-exists-p elpaca-repo)))
           (buffer (get-buffer-create "*elpaca-bootstrap*")))
  (condition-case-unless-debug err
      (progn
        (unless (zerop (call-process "git" nil buffer t "clone" elpaca-url elpaca-repo))
          (error "%s" (list (with-current-buffer buffer (buffer-string)))))
        (byte-recompile-directory elpaca-repo 0 'force)
        (require 'elpaca)
        (elpaca-generate-autoloads "elpaca" elpaca-repo)
        (kill-buffer buffer))
    ((error)
     (delete-directory elpaca-directory 'recursive)
     (with-current-buffer buffer
       (goto-char (point-max))
       (insert (format "\n%S" err))
       (display-buffer buffer)))))
(require 'elpaca-autoloads)
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca (elpaca :host github :repo "progfolio/elpaca"))

;; Load `fi-emacs' startup utilities

(when-let ((fi-emacs-repo (expand-file-name "repos/fi-emacs/" elpaca-directory)))
  (add-to-list 'load-path fi-emacs-repo)
  (require 'fi)
  (require 'bk))

;; `package-set.el' loading mechanism

(let ((packages
       (with-current-buffer
           (let ((default-directory user-emacs-directory))
             (find-file-noselect "package-set.el"))
         (goto-char (point-min))
         (prog1 (read (current-buffer))
           (kill-buffer (current-buffer))))))
  (dolist (package packages)
    (eval `(elpaca ,package))))

(provide 'load-packages)

;;; load-packages.el ends here
