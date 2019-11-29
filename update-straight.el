;;; update-straight.el --- auto-update with `straight.el'

;;; Commentary:
;; 

;;; Code:

;; Straight bootstrap

;; FIXME: Ensure that straight is always consistent

(setq straight-fix-org t)

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

;; Get al packages

(let* ((contents
        (with-current-buffer
            (let ((default-directory user-emacs-directory))
              (find-file-noselect "package-set.el"))
          (goto-char (point-min))
          (prog1
              (read (current-buffer))
            (kill-buffer (current-buffer)))))
       (repos (append (nth 0 contents) nil))
       (packages (append (nth 1 contents) nil)))
  (setq straight-recipe-repositories repos)
  (setq update--all-packages packages)
  (mapc 'straight-use-package packages))

;; Update all available packages

(setq update--valid-recipes '())

(straight--map-repos
 (lambda (package)
   (straight--with-plist package
       (local-repo)
     (if (straight--repository-is-available-p package)
         (let* ((straight--default-directory (straight--repos-dir local-repo))
                (status (straight--get-call-raw
                         "git" "-c" "status.branch=false"
                         "status" "--short")))
           (if (string-empty-p status)
               (push package update--valid-recipes)
             (message "Dirty repository: \"%s\"" local-repo)))
       (message "Unavailable repository: \"%s\"" local-repo)))))

(switch-to-buffer "*straight*")

(setq straight-x-all update--valid-recipes
      straight-x-waiting straight-x-all
      straight-x-running nil
      straight-x-finished nil)

(message "Fetching repos...")
(dotimes (_ straight-x-process-limit)
  (straight-x-start-process))

(while (< 0 (length
             (append
              straight-x-waiting
              straight-x-running)))
  (sleep-for 0 1000)
  (message "Waiting (%s/%s)..."
           (length straight-x-finished)
           (length straight-x-all)))

(message "Merging repos...")
(dolist (recipe update--valid-recipes)
  (straight-vc-merge-from-remote recipe))

(message "Freezing versions...")
(straight-freeze-versions t)

(message "Checking packages...")
(straight-check-all)

(kill-buffer "*straight*")

(provide 'update-straight)

;;; update-straight.el ends here
