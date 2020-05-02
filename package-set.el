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
  (ivy-taskrunner
   :type git :host github
   :repo "emacs-taskrunner/ivy-taskrunner")
  taskrunner
  ace-link
  ace-window
  aggressive-indent
  amx
  auctex-latexmk
  avy
  benchmark-init
  cargo
  company
  company-posframe
  counsel
  counsel-projectile
  crux
  deadgrep
  diredfl
  dired-filter
  direnv
  eglot
  el2org
  expand-region
  flymake-diagnostic-at-point
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
  lsp-ivy
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
  ox-gfm
  pcre2el
  poly-org
  polymode
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
