(leaf org-mode
  :straight org-cliplink
  :leaf-defer t
  :custom
  (org-adapt-indentation . nil))

(leaf nix-mode
  :straight t
  :leaf-defer t
  :mode "\\.nix\\'"
  :custom
  (nix-indent-function . 'nix-indent-line))

(leaf lua-mode
  :straight t
  :leaf-defer t
  :mode "\\.lua\\'"
  )

(leaf rust-mode
  :straight t
  :leaf-defer t
  :mode "\\.rs\\'"
  )

(leaf toml-mode
  :straight t
  :leaf-defer t
  :mode "\\.toml\\'"
  )

(leaf markdown-mode
  :straight t
  :leaf-defer t
  :mode (("\\.md\\'" "\\.markdown\\'") . gfm-mode)
  )

(leaf tex
  :straight (auctex :type git :host github
                    :repo "emacs-straight/auctex")
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
                "zathura")))

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
           0)))))

(leaf emacs-lisp-mode
  :leaf-defer t
  :mode "\\.el\\'"
  :hook (emacs-lisp-mode-hook . lispy-mode))

(leaf lispy
  :straight t aggressive-indent
  :leaf-defer t
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
  :leaf-defer t
  :straight t ox-gfm)

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
