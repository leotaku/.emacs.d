([org-elpa melpa emacsmirror-mirror gnu-elpa-mirror]
 [(expand-region-improved
   :type git :host github
   :repo "leotaku/expand-region-improved")
  (fi
   :type git :host github
   :repo "leotaku/fi-emacs"
   :files ("*.el"))
  (theist-mode
   :type git :host github
   :repo "leotaku/theist-mode")
  (flycheck-aspell
   :type git :host github
   :repo "leotaku/flycheck-aspell"
   :files ("flymake-aspell.el"))
  (auctex
   :type git :host github
   :repo "emacs-straight/auctex")
  (doom-themes
   :type git :flavor melpa
   :files (:defaults "themes/*.el"
                     "doom-themes-pkg.el")
   :host github
   :repo "leotaku/emacs-doom-themes")
  ace-link
  ace-window
  aggressive-indent
  amx
  avy
  benchmark-init
  company
  company-posframe
  counsel
  counsel-projectile
  crux
  deadgrep
  diredfl
  dired-filter
  envrc
  haskell-mode
  hl-todo
  ivy-bibtex
  keyfreq
  leaf
  lice
  lispy
  lsp-mode
  lsp-ui
  company-lsp
  lua-mode
  magit
  magit-todos
  markdown-mode
  minions
  modalka
  moody
  multiple-cursors
  nix-mode
  no-littering
  org-cliplink
  org-plus-contrib
  org-reverse-datetree
  pcre2el
  projectile
  rainbow-mode
  rust-mode
  sly
  smartparens
  swiper
  undo-fu
  undo-fu-session
  visual-regexp
  visual-regexp-steroids
  wgrep
  which-key
  worf
  yaml-mode
  yankpad
  yasnippet])
