;; FIXME: Org Elpa is outdated but still needed by straight.el
([org-elpa melpa gnu-elpa-mirror emacsmirror-mirror]
 [(expand-region-improved
   :type git :host github
   :repo "leotaku/expand-region-improved")
  (fi
   :type git :host github
   :files ("*.el")
   :repo "leotaku/fi-emacs")
  (flycheck-aspell
   :type git :host github
   :files ("flymake-aspell.el")
   :repo "leotaku/flycheck-aspell")
  (study
   :type git :host github
   :files (:defaults "study-*.el")
   :repo "leotaku/study.el")
  (theist-mode
   :type git :host github
   :repo "leotaku/theist-mode")
  (auctex
   :type git :host github
   :repo "emacs-straight/auctex")
  (doom-themes
   :type git :host github
   :files (:defaults "themes/*.el" "doom-themes-pkg.el")
   :repo "leotaku/emacs-doom-themes")
  (mu4e :type built-in)
  (vterm :type built-in)
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
  deadgrep
  dired-filter
  diredfl
  eglot
  emacsql-sqlite-builtin
  envrc
  fish-mode
  forge
  git-modes
  go-mode
  haskell-mode
  hcl-mode
  keyfreq
  ledger-mode
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
  swiper
  terraform-mode
  tide
  undo-fu-session
  visual-fill-column
  visual-regexp
  web-mode
  wgrep
  which-key
  worf
  yaml-mode])
