;;; load-packages.el --- load elpaca.el packages  -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(defvar elpaca-installer-version 0.2)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil
                              :files (:defaults (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(when-let ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
           (build (expand-file-name "elpaca/" elpaca-builds-directory))
           (order (cdr elpaca-order))
           ((add-to-list 'load-path (if (file-exists-p build) build repo)))
           ((not (file-exists-p repo))))
  (condition-case-unless-debug err
      (if-let ((buffer (pop-to-buffer-same-window "*elpaca-installer*"))
               ((zerop (call-process "git" nil buffer t "clone"
                                     (plist-get order :repo) repo)))
               (default-directory repo)
               ((zerop (call-process "git" nil buffer t "checkout"
                                     (or (plist-get order :ref) "--"))))
               (emacs (concat invocation-directory invocation-name))
               ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                     "--eval" "(byte-recompile-directory \".\" 0 'force)"))))
          (progn (require 'elpaca)
                 (elpaca-generate-autoloads "elpaca" repo)
                 (kill-buffer buffer))
        (error "%s" (with-current-buffer buffer (buffer-string))))
    ((error) (warn "%s" err) (delete-directory repo 'recursive))))
(require 'elpaca-autoloads)
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Load `fi-emacs' startup utilities

(let ((fi-build-directory (expand-file-name "fi/" elpaca-builds-directory)))
  (if (file-directory-p fi-build-directory)
      (progn (add-to-list 'load-path fi-build-directory)
             (require 'fi)
             (require 'bk))
    (elpaca (fi :type git :host github :files ("*.el") :repo "leotaku/fi-emacs"))
    (elpaca-wait)))

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
