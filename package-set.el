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
  company-lsp
  company-posframe
  counsel
  counsel-projectile
  crux
  deadgrep
  dired-filter
  diredfl
  envrc
  fish-mode
  git-modes
  go-mode
  haskell-mode
  ivy-bibtex
  keyfreq
  leaf
  lice
  lispy
  lsp-haskell
  lsp-mode
  lsp-ui
  lua-mode
  magit
  markdown-mode
  minions
  modalka
  moody
  multiple-cursors
  nix-mode
  no-littering
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
  visual-fill-column
  visual-regexp
  visual-regexp-steroids
  web-mode
  wgrep
  which-key
  worf
  yaml-mode
  yankpad
  yasnippet])
