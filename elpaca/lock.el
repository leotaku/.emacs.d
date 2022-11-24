((yaml-mode :package "yaml-mode" :repo "yoshiki/yaml-mode" :fetcher github :files
	    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
	     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
	    :protocol https :remotes "origin" :inherit t :depth 1 :ref "3fcb36d6039bef57e2a0f6e24c51f623c0bf5fb7")
 (worf :package "worf" :repo "abo-abo/worf" :fetcher github :files
       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
	(:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
       :protocol https :remotes "origin" :inherit t :depth 1 :ref "8681241e118585824cd256e5b026978bf06c7e58")
 (which-key :package "which-key" :repo "justbur/emacs-which-key" :fetcher github :files
	    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
	     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
	    :protocol https :remotes "origin" :inherit t :depth 1 :ref "8093644032854b1cdf3245ce4e3c7b6673f741bf")
 (wgrep :package "wgrep" :fetcher github :repo "mhayashi1120/Emacs-wgrep" :files
	("wgrep.el")
	:protocol https :remotes "origin" :inherit t :depth 1 :ref "f9687c28bbc2e84f87a479b6ce04407bb97cfb23")
 (web-mode :package "web-mode" :repo "fxbois/web-mode" :fetcher github :files
	   ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
	    (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
	   :protocol https :remotes "origin" :inherit t :depth 1 :ref "4b8a695825fda366927894e498421f35fce1cbb9")
 (visual-regexp :package "visual-regexp" :repo "benma/visual-regexp.el" :fetcher github :files
		("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
		 (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		:protocol https :remotes "origin" :inherit t :depth 1 :ref "48457d42a5e0fe10fa3a9c15854f1f127ade09b5")
 (visual-fill-column :package "visual-fill-column" :fetcher codeberg :repo "joostkremers/visual-fill-column" :files
		     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
		      (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		     :protocol https :remotes "origin" :inherit t :depth 1 :ref "453d698d7fc243a547665f8ba43c55eee574e0db")
 (undo-fu-session :package "undo-fu-session" :fetcher codeberg :repo "ideasman42/emacs-undo-fu-session" :files
		  ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
		   (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		  :protocol https :remotes "origin" :inherit t :depth 1 :ref "e141c929f617acfb888a6e5d72ace740b7c4d535")
 (tide :package "tide" :fetcher github :repo "ananthakumaran/tide" :files
       (:defaults "tsserver")
       :protocol https :remotes "origin" :inherit t :depth 1 :ref "4cf6a0d89da7f946565a425a632ee2410a40c7da")
 (terraform-mode :package "terraform-mode" :repo "emacsorphanage/terraform-mode" :fetcher github :files
		 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
		  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		 :protocol https :remotes "origin" :inherit t :depth 1 :ref "80f0433358b79ed4ba88c51829c7359baa1af8b1")
 (swiper :package "swiper" :repo "abo-abo/swiper" :fetcher github :files
	 ("swiper.el")
	 :protocol https :remotes "origin" :inherit t :depth 1 :ref "b8be4913a661b557e0d3275726e36871556569d3")
 (rust-mode :package "rust-mode" :repo "rust-lang/rust-mode" :fetcher github :files
	    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
	     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
	    :protocol https :remotes "origin" :inherit t :depth 1 :ref "b4537b6f5fa65626c1bab944681b35769cab9b5c")
 (rainbow-mode :package "rainbow-mode" :host github :repo "emacs-straight/rainbow-mode" :protocol https :remotes "origin" :inherit t :depth 1 :files
	       (:defaults)
	       :ref "55a8c15782197cd9db8950d2f5ed1b9caca08dae")
 (projectile :package "projectile" :fetcher github :repo "bbatsov/projectile" :files
	     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
	      (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
	     :protocol https :remotes "origin" :inherit t :depth 1 :ref "036d327b5ad9b970bd1ea3123692a80bc1015b4a")
 (pcre2el :package "pcre2el" :fetcher github :repo "joddie/pcre2el" :files
	  ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
	   (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
	  :protocol https :remotes "origin" :inherit t :depth 1 :ref "38c6f80c787da547287db96b495e5b695ca0b4b8")
 (org-reverse-datetree :package "org-reverse-datetree" :fetcher github :repo "akirak/org-reverse-datetree" :files
		       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
			(:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		       :protocol https :remotes "origin" :inherit t :depth 1 :ref "127b168960296861f73f8e38247438ebdc575d1e")
 (org :package "org" :local-repo "org" :repo "https://git.savannah.gnu.org/git/emacs/org-mode.git" :depth 1 :pre-build
      (progn
	(require 'elpaca-menu-org)
	(elpaca-menu-org--build))
      :build
      (:not elpaca--generate-autoloads-async)
      :files
      (:defaults
       ("etc/styles/" "etc/styles/*"))
      :protocol https :remotes "origin" :inherit t :ref "f7831cc9aca1139c9b79fdb1b48af95b79c85752")
 (no-littering :package "no-littering" :fetcher github :repo "emacscollective/no-littering" :files
	       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
		(:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
	       :protocol https :remotes "origin" :inherit t :depth 1 :ref "fea487d7cb0e9a09fcf0b522c91fbc89d57ef1bb")
 (nix-mode :package "nix-mode" :fetcher github :repo "NixOS/nix-mode" :files
	   (:defaults
	    (:exclude "nix-company.el" "nix-mode-mmm.el"))
	   :protocol https :remotes "origin" :inherit t :depth 1 :ref "127d76202f10973e5af760fdb7804cc55cf51152")
 (multiple-cursors :package "multiple-cursors" :fetcher github :repo "magnars/multiple-cursors.el" :files
		   ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
		    (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		   :protocol https :remotes "origin" :inherit t :depth 1 :ref "1e4842d1297241a5277bfd5c7bfab9e8711da60a")
 (moody :package "moody" :fetcher github :repo "tarsius/moody" :files
	("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
	 (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
	:protocol https :remotes "origin" :inherit t :depth 1 :ref "546472d1cabafa092fbc24f467e2acc02fe713ec")
 (modalka :package "modalka" :repo "mrkkrp/modalka" :fetcher github :files
	  ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
	   (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
	  :protocol https :remotes "origin" :inherit t :depth 1 :ref "e674b08c21f24bc32ee8fa3db45ffb0743410bce")
 (minions :package "minions" :fetcher github :repo "tarsius/minions" :files
	  ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
	   (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
	  :protocol https :remotes "origin" :inherit t :depth 1 :ref "ef6d7daaab9268d2e71eac6511ececa2ba42d3bf")
 (meson-mode :package "meson-mode" :fetcher github :repo "wentasah/meson-mode" :files
	     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
	      (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
	     :protocol https :remotes "origin" :inherit t :depth 1 :ref "82220d12899b87acb3c862b17368f63de7d08d07")
 (markdown-mode :package "markdown-mode" :fetcher github :repo "jrblevin/markdown-mode" :files
		("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
		 (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		:protocol https :remotes "origin" :inherit t :depth 1 :ref "c338cdff80012893e64ba62a199281f430db7021")
 (magit :package "magit" :fetcher github :repo "magit/magit" :files
	("lisp/magit" "lisp/magit*.el" "lisp/git-rebase.el" "docs/magit.texi" "docs/AUTHORS.md" "LICENSE" "Documentation/magit.texi" "Documentation/AUTHORS.md"
	 (:exclude "lisp/magit-libgit.el" "lisp/magit-libgit-pkg.el" "lisp/magit-section.el" "lisp/magit-section-pkg.el"))
	:protocol https :remotes "origin" :inherit t :depth 1 :ref "744818a3be01034a8577063e061e9b9e53ccf890")
 (lua-mode :package "lua-mode" :repo "immerrr/lua-mode" :fetcher github :files
	   (:defaults
	    (:exclude "init-tryout.el"))
	   :protocol https :remotes "origin" :inherit t :depth 1 :ref "3e783c93aa8a3d3ca985686438aa8d140cbddae6")
 (lispy :package "lispy" :repo "abo-abo/lispy" :fetcher github :files
	(:defaults "lispy-clojure.clj" "lispy-clojure.cljs" "lispy-python.py")
	:protocol https :remotes "origin" :inherit t :depth 1 :ref "f35eadf8c1be43a395e196463314b17ea3b4e16f")
 (ledger-mode :package "ledger-mode" :fetcher github :repo "ledger/ledger-mode" :files
	      (:defaults "ledger-test.el")
	      :old-names
	      (ldg-mode)
	      :protocol https :remotes "origin" :inherit t :depth 1 :ref "8bad528d43007e0310b5e72e6e021b502b30495c")
 (keyfreq :package "keyfreq" :fetcher github :repo "dacap/keyfreq" :files
	  ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
	   (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
	  :protocol https :remotes "origin" :inherit t :depth 1 :ref "dd88193cd7a91a92113121191573758ea2a3ceb1")
 (hcl-mode :package "hcl-mode" :repo "purcell/emacs-hcl-mode" :fetcher github :files
	   ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
	    (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
	   :protocol https :remotes "origin" :inherit t :depth 1 :ref "e4d9eef631e8a386341ae8f94f7c2579586e65b5")
 (haskell-mode :package "haskell-mode" :repo "haskell/haskell-mode" :fetcher github :files
	       (:defaults "NEWS" "logo.svg")
	       :protocol https :remotes "origin" :inherit t :depth 1 :ref "a34ccdc54be15043ff0d253c3c20087524255491")
 (go-mode :package "go-mode" :repo "dominikh/go-mode.el" :fetcher github :files
	  ("go-mode.el")
	  :protocol https :remotes "origin" :inherit t :depth 1 :ref "08aa90d52f0e7d2ad02f961b554e13329672d7cb")
 (git-modes :package "git-modes" :fetcher github :repo "magit/git-modes" :old-names
	    (gitattributes-mode gitconfig-mode gitignore-mode)
	    :files
	    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
	     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
	    :protocol https :remotes "origin" :inherit t :depth 1 :ref "4f9ad30f01cfb3f8460b786153f8569578c0f8dc")
 (forge :package "forge" :fetcher github :repo "magit/forge" :files
	("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
	 (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
	:protocol https :remotes "origin" :inherit t :depth 1 :ref "33651c0c8225371c78503b2eb7a543d42e71f885")
 (envrc :package "envrc" :fetcher github :repo "purcell/envrc" :files
	("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
	 (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
	:protocol https :remotes "origin" :inherit t :depth 1 :ref "c54bf9e6972c563d345e20571ffd44d7bfb56974")
 (emacsql-sqlite-builtin :package "emacsql-sqlite-builtin" :fetcher github :repo "magit/emacsql" :files
			 ("emacsql-sqlite-builtin.el")
			 :protocol https :remotes "origin" :inherit t :depth 1 :ref "e318a6c8f65371e2ab667d811205a0d9a98dacbb")
 (eglot :package "eglot" :repo "joaotavora/eglot" :fetcher github :files
	("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
	 (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
	:protocol https :remotes "origin" :inherit t :depth 1 :ref "e501275e06952889056268dabe08ccd0dbaf23e5")
 (diredfl :package "diredfl" :fetcher github :repo "purcell/diredfl" :files
	  ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
	   (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
	  :protocol https :remotes "origin" :inherit t :depth 1 :ref "94bd99eeced6d52a5a7b9db3745239feafd633e2")
 (dired-filter :package "dired-filter" :fetcher github :repo "Fuco1/dired-hacks" :files
	       ("dired-filter.el")
	       :protocol https :remotes "origin" :inherit t :depth 1 :ref "7c0ef09d57a80068a11edc74c3568e5ead5cc15a")
 (deadgrep :package "deadgrep" :repo "Wilfred/deadgrep" :fetcher github :files
	   ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
	    (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
	   :protocol https :remotes "origin" :inherit t :depth 1 :ref "f687ca31f8d3bd8ebf05165b080b50ba724ce9bf")
 (crux :package "crux" :fetcher github :repo "bbatsov/crux" :files
       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
	(:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
       :protocol https :remotes "origin" :inherit t :depth 1 :ref "f8789f67a9d2e1eb31a0e4531aec9bb6d6ec1282")
 (counsel-projectile :package "counsel-projectile" :fetcher github :repo "ericdanan/counsel-projectile" :files
		     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
		      (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		     :protocol https :remotes "origin" :inherit t :depth 1 :ref "40d1e1d4bb70acb00fddd6f4df9778bf2c52734b")
 (counsel :package "counsel" :repo "abo-abo/swiper" :fetcher github :files
	  ("counsel.el")
	  :protocol https :remotes "origin" :inherit t :depth 1 :ref "b8be4913a661b557e0d3275726e36871556569d3")
 (company-posframe :package "company-posframe" :fetcher github :repo "tumashu/company-posframe" :files
		   ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
		    (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		   :protocol https :remotes "origin" :inherit t :depth 1 :ref "ab58972c2cebc5ecf68c4cdd140c3aed2c68f42b")
 (company :package "company" :repo "company-mode/company-mode" :fetcher github :files
	  (:defaults "icons"
		     ("images/small" "doc/images/small/*.png"))
	  :protocol https :remotes "origin" :inherit t :depth 1 :ref "48fea7a905b3bcc6d97609316beced666da89b1f")
 (circe :package "circe" :repo "emacs-circe/circe" :fetcher github :files
	("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
	 (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
	:protocol https :remotes "origin" :inherit t :depth 1 :ref "68cdb22b33b733c5e412092160ab84264bb4c51f")
 (avy :package "avy" :repo "abo-abo/avy" :fetcher github :files
      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
       (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
      :protocol https :remotes "origin" :inherit t :depth 1 :ref "955c8dedd68c74f3cf692c1249513f048518c4c9")
 (apheleia :package "apheleia" :fetcher github :repo "radian-software/apheleia" :files
	   ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
	    (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
	   :protocol https :remotes "origin" :inherit t :depth 1 :ref "33d4542b58476d50f01464576664de1acea1f62f")
 (amx :package "amx" :repo "DarwinAwardWinner/amx" :fetcher github :files
      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
       (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
      :protocol https :remotes "origin" :inherit t :depth 1 :ref "37f9c7ae55eb0331b27200fb745206fc58ceffc0")
 (aggressive-indent :package "aggressive-indent" :repo "Malabarba/aggressive-indent-mode" :fetcher github :files
		    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
		     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		    :protocol https :remotes "origin" :inherit t :depth 1 :ref "f376cdc25de5c0f8c330f1e053557d95ca47a540")
 (ace-window :package "ace-window" :repo "abo-abo/ace-window" :fetcher github :files
	     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
	      (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
	     :protocol https :remotes "origin" :inherit t :depth 1 :ref "43dc77e2136259d5601cc6af5d0f90cbaad79461")
 (ace-link :package "ace-link" :repo "abo-abo/ace-link" :fetcher github :files
	   ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
	    (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
	   :protocol https :remotes "origin" :inherit t :depth 1 :ref "77c683ef8aaa89e578d7fb248da69eb2e7245ad1")
 (doom-themes :package "doom-themes" :fetcher github :repo "leotaku/emacs-doom-themes" :files
	      (:defaults "themes/*.el" "doom-themes-pkg.el")
	      :protocol https :remotes "origin" :inherit t :depth 1 :type git :host github :ref "c50eed2ab185c1772aea438e88509e860c143cc1")
 (prisma-mode :protocol https :remotes "origin" :inherit t :depth 1 :type git :host github :repo "pimeys/emacs-prisma-mode" :package "prisma-mode" :files
	      (:defaults)
	      :ref "f7744a995e84b8cf51265930ce18f6a6b26dade7")
 (auctex :package "auctex" :host github :repo "emacs-straight/auctex" :protocol https :remotes "origin" :inherit t :depth 1 :type git :files
	 (:defaults)
	 :ref "ae16c19a599e6bf4507c167e3c8d34870668710f")
 (theist-mode :protocol https :remotes "origin" :inherit t :depth 1 :type git :host github :repo "leotaku/theist-mode" :package "theist-mode" :files
	      (:defaults)
	      :ref "278e78e0659dda49cffcb57a1403ae02fb01ceb2")
 (study :protocol https :remotes "origin" :inherit t :depth 1 :type git :host github :files
	(:defaults "study-*.el")
	:repo "leotaku/study.el" :package "study" :ref "01f2bddb0ce0eb1f4e2a4adedff5346f09ed240b")
 (flycheck-aspell :package "flycheck-aspell" :fetcher github :repo "leotaku/flycheck-aspell" :files
		  ("flymake-aspell.el")
		  :protocol https :remotes "origin" :inherit t :depth 1 :type git :host github :ref "6efe0ae2d82ba83c563e117b290cb22ae91d2f58")
 (fi :protocol https :remotes "origin" :inherit t :depth 1 :type git :host github :files
     ("*.el")
     :repo "leotaku/fi-emacs" :package "fi" :ref "baaad90d13a705fc7214a0cc3b1a9800c369041f")
 (expand-region-improved :protocol https :remotes "origin" :inherit t :depth 1 :type git :host github :repo "leotaku/expand-region-improved" :package "expand-region-improved" :files
			 (:defaults)
			 :ref "ccf8a149670ca901da7095edc71f1f16c9082c85")
 (elpaca :protocol https :remotes "origin" :inherit t :depth 1 :host github :repo "progfolio/elpaca" :package "elpaca" :files
	 (:defaults)
	 :ref "728b71598409176c998894d2c3fe93a1d55c55f3"))
