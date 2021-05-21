([melpa gnu-elpa-mirror emacsmirror-mirror]
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
  circe
  company
  company-posframe
  counsel
  counsel-projectile
  crux
  dired-filter
  diredfl
  direnv
  fish-mode
  git-modes
  go-mode
  haskell-mode
  keyfreq
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
  org-cliplink
  org
  org-reverse-datetree
  pcre2el
  projectile
  rainbow-mode
  rust-mode
  smartparens
  swiper
  undo-fu-session
  visual-fill-column
  visual-regexp
  web-mode
  wgrep
  which-key
  worf
  yaml-mode
  yasnippet])
