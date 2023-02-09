;;; load-packages.el --- load elpaca.el packages  -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

;; Emacs wants to load `package.el' before the init file,
;; so we do the same with `elpaca.el'

(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil
                              :files (:defaults (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(when-let ((repo  (expand-file-name "repos/elpaca/" elpaca-directory))
           (build (expand-file-name "elpaca/" elpaca-builds-directory))
           (order (cdr elpaca-order))
           ((add-to-list 'load-path (if (file-exists-p build) build repo)))
           ((not (file-exists-p repo))))
  (condition-case-unless-debug err
      (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
               ((zerop (call-process "git" nil buffer t "clone"
                                     (plist-get order :repo) repo)))
               (default-directory repo)
               ((zerop (call-process "git" nil buffer t "checkout"
                                     (or (plist-get order :ref) "--")))))
          (progn
            (byte-recompile-directory repo 0 'force)
            (require 'elpaca)
            (and (fboundp 'elpaca-generate-autoloads)
                 (elpaca-generate-autoloads "elpaca" repo))
            (kill-buffer buffer))
        (error "%s" (with-current-buffer buffer (buffer-string))))
    ((error)
     (warn "%s" err)
     (delete-directory repo 'recursive))))
(require 'elpaca-autoloads)
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

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
