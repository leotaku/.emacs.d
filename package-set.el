;; FIXME: Org Elpa is outdated but still needed by straight.el
([org-elpa melpa gnu-elpa-mirror emacsmirror-mirror]
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
  (study
   :type git :host github
   :repo "leotaku/study.el"
   :files ("*.el"))
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
