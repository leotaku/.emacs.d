;;; usability.el --- basic usability configurations -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(bk-block* visual-regexp
  :requires .visual-regexp .pcre2el
  :config
  (advice-add 'vr--get-regexp-string :around #'advice-vr-pcre))

(defun advice-vr-pcre (func &optional for-display)
  (let ((text (funcall func)))
    (pcre-to-elisp text)))

(bk-block wgrep
  :requires .wgrep
  :bind ((:ivy-occur-grep-mode-map
          :package ivy
          ("e" . wgrep-change-to-wgrep-mode))))

(bk-block* ispell
  :bind (("C-." . ispell-word))
  :custom
  (ispell-dictionary . "en_US")
  (ispell-program-name . "aspell")
  (ispell-really-aspell . t)
  (ispell-silently-savep . t))

(bk-block llm-support
  :requires .leyline-assistant .leyline-chat .llm-claude .llm-openai .llm-ollama .markdown-mode
  :custom
  (leyline-providers
   . `(("sonnet"
        . ,(make-llm-claude
            :key (auth-source-pick-first-password :host "api.anthropic.com")
            :chat-model "claude-3-7-sonnet-latest"))
       ("sonnet-3.5"
        . ,(make-llm-claude
            :key (auth-source-pick-first-password :host "api.anthropic.com")
            :chat-model "claude-3-5-sonnet-20241022"))
       ("o1-mini"
        . ,(make-llm-openai
            :key (auth-source-pick-first-password :host "api.openai.com")
            :chat-model "o1-mini"))
       ("deepseek-reasoner"
        . ,(make-llm-openai-compatible
            :url "https://api.deepseek.com"
            :key (auth-source-pick-first-password :host "api.deepseek.com")
            :chat-model "deepseek-reasoner"))
       ("deepseek-chat"
        . ,(make-llm-openai-compatible
            :url "https://api.deepseek.com"
            :key (auth-source-pick-first-password :host "api.deepseek.com")
            :chat-model "deepseek-chat"))))
  (leyline-configurations
   . `(("terse" . (:context "Answer using at most one paragraph of text! If you are asked for explicit code examples, those may be longer." :temperature 0.35))
       ("chat" . (:context "You are a helpful assistant. Please act accordingly!" :temperature 0.35))
       ("coding" . (:context "You are a helpful coding assistant. Please act accordingly!" :temperature 0.00))
       ("human" . (:context "You are the users friend! Help him as best as you can, while acting like a human who is being contacted by text." :temperature 1.00))
       ("sentiment" . (:context "You are a classification algorithm. Always answer the given question using only one phrase." :temperature 0.00))))
  :custom
  (leyline-chat-initial-major-mode . 'markdown-mode)
  (llm-warn-on-nonfree . nil))

(bk-block* which-key
  :start which-key-mode)

(bk-block* ivy
  :at-load (setq ivy-do-completion-in-region nil)
  :bind ((:ivy-minibuffer-map
          :package ivy
          ("<C-i>" . ivy-insert-selection)
          ("C-h" . ivy-lookup-symbol)
          ("C-v" . yank)))
  :start ivy-mode
  :custom (ivy-use-selectable-prompt . t)
  :config
  (advice-add
   'ivy-switch-buffer-transformer
   :override #'advice-ivy-switch-buffer-transformer))

(defun advice-ivy-switch-buffer-transformer (str)
  "Transform candidate STR like dired when switching buffers."
  (let ((face (ibuffer-buffer-name-face str 0)))
    (ivy-append-face str face)))

(bk-block* swiper
  :bind (("C-s" . swiper-isearch)))

(bk-block* counsel
  :bind ((:ivy-mode-map
          ([remap yank-pop] . counsel-yank-pop))))

(bk-block* project
  :bind* (("C-x p" . project-prefix-map))
  :bind (:project-prefix-map
         :package project
         ("s" . counsel-rg))
  :custom
  (project-switch-commands . #'project-dired)
  (project-vc-extra-root-markers . '(".projectile")))


(bk-block* undo-fu-session
  :requires .undo-fu-session .no-littering
  :start global-undo-fu-session-mode
  :custom
  (undo-fu-session-incompatible-files
   . '("COMMIT_EDITMSG$"
       "git-rebase-todo$"))
  (undo-fu-session-linear . t))

(bk-block* ace-link
  :bind (("C-x a" . ace-link))
  :custom
  (ace-link-fallback-function . 'ace-link-org))

(bk-block recentf
  :requires .recentf .no-littering
  :bind (("C-x l" . recentf))
  :at-load
  (setq recentf-max-saved-items 4000)
  (setq recentf-max-menu-items 1000)
  :config
  (add-to-list 'recentf-exclude #'file-contained-in-symlink-p)
  (add-to-list 'recentf-exclude #'file-remote-p)
  (add-hook 'recentf-mode-hook #'recentf-save-list))

(bk-block* tramp
  :config
  (defun tramp (system)
    (interactive "MSystem: ")
    (find-file (format "/-:%s:/" system)))
  :custom
  (tramp-default-method . "ssh"))

(bk-block shell-and-terminal
  :config
  (defun advice-term-exit (&rest _)
    (kill-buffer))
  (advice-add 'term-handle-exit :after #'advice-term-exit))

(bk-block ibuffer
  :requires .ibuffer .theist-mode
  :bind (("C-x i" . ibuffer)
         (:ibuffer-mode-map
          :package ibuffer
          ("x" . theist-C-x)
          ("j" . next-line)
          ("k" . previous-line)
          ("d" . ibuffer-do-delete)))
  :custom
  (ibuffer-fontification-alist
   . '((10 (derived-mode-p 'org-mode) ivy-org)
       (20 (buffer-modified-p) ivy-modified-buffer)
       (25 (not (buffer-file-name)) font-lock-constant-face)
       (30 (string-match "^\\*" (buffer-name)) ivy-virtual)
       (40 (memq major-mode ibuffer-help-buffer-modes) font-lock-comment-face)
       (40 (derived-mode-p 'circe-mode) ivy-remote)
       (40 (derived-mode-p 'dired-mode) ivy-subdir)
       (50 (ivy--remote-buffer-p (current-buffer)) ivy-remote))))

(bk-block visual-fill-column
  :requires .visual-fill-column
  :hook
  (markdown-mode-hook . visual-fill-column-mode)
  (org-mode-hook . visual-fill-column-mode)
  :config
  (advice-add 'fill-column :around #'advice-visual-fill-column-warn)
  (advice-add 'org-fill-paragraph :around #'advice-visual-fill-column-warn)
  (advice-add 'fill-paragraph :around #'advice-visual-fill-column-warn))

(defun advice-visual-fill-column-warn (f &rest args)
  (interactive)
  (if (bound-and-true-p visual-fill-column-mode)
      (message "Auto-fill should not be used with visual-fill-column")
    (apply f args)))

(defun save-clipboard-before-delete-frame (_frame)
  (when (string= (gui-get-selection 'CLIPBOARD)
                 (current-kill 0))
    (start-process "wl-copy" nil "wl-copy" (current-kill 0))))

(add-hook 'delete-frame-functions #'save-clipboard-before-delete-frame)

;;; usability.el ends here
