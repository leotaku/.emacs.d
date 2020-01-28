([org-elpa melpa emacsmirror-mirror gnu-elpa-mirror]
 [(fi
   :type git :host github
   :repo "leotaku/fi-emacs"
   :files ("*.el"))
  (theist-mode
   :type git :host github
   :repo "leotaku/theist-mode")
  (flycheck-aspell
   :type git :host github
   :repo "leotaku/flycheck-aspell")
  (auctex
   :type git :host github
   :repo "emacs-straight/auctex")
  (doom-themes
   :type git :flavor melpa
   :files (:defaults "themes/*.el"
                     "doom-themes-pkg.el")
   :host github
   :repo "leotaku/emacs-doom-themes")
  (undohist
   :type git :host gitlab
   :repo "ideasman42/emacs-undohist")
  ace-window
  aggressive-indent
  amx
  auctex-latexmk
  avy
  benchmark-init
  cargo
  company
  company-posframe
  compdef
  counsel
  counsel-projectile
  crux
  deadgrep
  diredfl
  direnv
  eglot
  el2org
  elfeed
  elfeed-protocol
  expand-region
  flymake-diagnostic-at-point
  handle
  hl-todo
  ivy-bibtex
  keyfreq
  leaf
  lice
  lispy
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
  openwith
  org-cliplink
  org-plus-contrib
  ox-gfm
  pcre2el
  projectile
  rainbow-mode
  rust-mode
  rustfmt
  sly
  smartparens
  suggest
  swiper
  toml-mode
  visual-regexp
  visual-regexp-steroids
  wgrep
  which-key
  yaml-mode
  yankpad
  yasnippet])
