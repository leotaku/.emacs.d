;;; major.el --- major-mode configurations for fi-emacs

;;; Commentary:
;; 

;;; Code:

(leaf org
  :leaf-autoload nil
  :mode ("\\.org\\'" . org-mode)
  :leaf-defer t
  :custom
  (org-adapt-indentation . nil)
  (org-tags-column . 0)
  :config
  (set-face-attribute
   'org-document-title nil
   :inherit 'variable-pitch
   :height 150)
  (defun org-clip ()
    (interactive)
    (require 'org-cliplink)
    (if (region-active-p)
        (let ((old (region-beginning)))
          (setf (point) old)
          (insert "[[")
          (insert (org-cliplink-clipboard-content))
          (insert "][")
          (setf (point) (region-end))
          (insert "]]")
          (setf (point) old))
      (org-cliplink))))

(leaf nix-mode
  :leaf-defer t
  :mode "\\.nix\\'"
  :custom
  (nix-indent-function . 'nix-indent-line))

(leaf lua-mode
  :leaf-defer t
  :mode "\\.lua\\'")

(leaf rust-mode
  :leaf-defer t
  :mode "\\.rs\\'" "\\.lalrpop\\'" "\\.rustpeg\\'")

(leaf toml-mode
  :leaf-defer t
  :mode "\\.toml\\'")

(leaf markdown-mode
  :leaf-defer t
  :mode (("\\.md\\'" "\\.markdown\\'") . gfm-mode))

(leaf tex
  :leaf-defer t
  :mode ("\\.tex\\'" . TeX-mode)
  :config
  (TeX-PDF-mode)
  (TeX-source-correlate-mode)
  
  (setq TeX-view-program-selection
        (list '(output-pdf "Zathura")))
  
  (add-to-list
   'TeX-expand-list
   '("%sn" (lambda () server-name)))
  
  (setq TeX-view-program-list
        (list '("Zathura"
                ("zathura %o"
                 (mode-io-correlate " --synctex-forward %n:0:%b -x \"emacsclient --socket-name=%sn --no-wait +%{line} %{input}\""))
                "zathura"))))

(defun TeX-view ()
  "Start a viewer without confirmation.
The viewer is started either on region or master file,
depending on the last command issued."
  (interactive)
  (let ((output-file (concat "out/" (TeX-active-master (TeX-output-extension)))))
    (if (file-exists-p output-file)
        (TeX-command
         "View"
         (lambda (&rest _)
           output-file)
         0))))

(leaf emacs-lisp-mode
  :leaf-defer t
  :mode "\\.el\\'"
  :hook (emacs-lisp-mode-hook . lispy-mode))

(bk-block* lispy
  :requires theist-mode
  :hook
  (minibuffer-setup-hook . conditionally-enable-lispy)
  (lispy-mode-hook . aggressive-indent-mode)
  :init
  (defun conditionally-enable-lispy ()
    (when (eq this-command 'eval-expression)
      (lispy-mode 1)))
  :config
  (lispy-define-key lispy-mode-map "x" 'theist-C-x))

(leaf el2org
  :leaf-defer t)

;; (leaf common-lisp-mode
;;   :mode ("\\.cl\\'" "\\.lisp\\'")
;;   :hook
;;   (lisp-mode-hook . lispy-mode)
;;   :config
;;   (setq-mode-local
;;    lisp-mode lisp-indent-function
;;    'common-lisp-indent-function))

;; (leaf sly
;;   :straight t
;;   :commands sly
;;   :bind ((:sly-mrepl-mode-map
;;           ("C-l" . comint-clear-buffer)))
;;   :custom
;;   (inferior-lisp-program . "sbcl"))

(provide 'major)

;;; major.el ends here
