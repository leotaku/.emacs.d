;; FIXME: Org Elpa is outdated but still needed by straight.el
([org-elpa melpa gnu-elpa-mirror emacsmirror-mirror]
 [(expand-region-improved
   :type git :host github
   :repo "leotaku/expand-region-improved")
  (fi
   :type git :host github
   :files ("*.el")
   :repo "leotaku/fi-emacs")
  (theist-mode
   :type git :host github
   :repo "leotaku/theist-mode")
  (flycheck-aspell
   :type git :host github
   :files ("flymake-aspell.el")
   :repo "leotaku/flycheck-aspell")
  (study
   :type git :host github
   :files (:defaults "study-deadgrep.el")
   :repo "leotaku/study.el")
  (auctex
   :type git :host github
   :repo "emacs-straight/auctex")
  (doom-themes
   :type git :host github
   :files (:defaults "themes/*.el" "doom-themes-pkg.el")
   :repo "leotaku/emacs-doom-themes")
  (emacsql
   :type git :host github
   :files (:defaults "emacsql-sqlite-builtin.el")
   :repo "tarsiiformes/emacsql"
   :branch "sqlite-backends")
  ace-link
  ace-window
  aggressive-indent
  amx
  apheleia
  avy
  circe
  company
  company-posframe
  counsel
  counsel-projectile
  crux
  dired-filter
  diredfl
  eglot
  envrc
  fish-mode
  forge
  git-modes
  go-mode
  haskell-mode
  hcl-mode
  keyfreq
  ledger-mode
  lice
  lispy
  lua-mode
  magit
  markdown-mode
  meson-mode
  minions
  modalka
  moody
  multiple-cursors
  nix-mode
  no-littering
  org
  org-cliplink
  org-reverse-datetree
  pcre2el
  projectile
  rainbow-mode
  rust-mode
  smartparens
  swiper
  terraform-mode
  undo-fu-session
  visual-fill-column
  visual-regexp
  web-mode
  wgrep
  which-key
  worf
  yaml-mode])
