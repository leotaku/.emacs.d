((elpaca :source "lockfile" :date
	 (25509 30898 572716 34000)
	 :recipe
	 (:protocol https :remotes "origin" :inherit t :depth 1 :host github :repo "progfolio/elpaca" :package "elpaca" :files
		    (:defaults)
		    :ref "6bdef5bdec556be378bed9d1b1f009b4e2b6a38f"))
 (expand-region-improved :source "lockfile" :date
			 (25509 30898 570716 628000)
			 :recipe
			 (:protocol https :remotes "origin" :inherit t :depth 1 :type git :host github :repo "leotaku/expand-region-improved" :package "expand-region-improved" :files
				    (:defaults)
				    :ref "ccf8a149670ca901da7095edc71f1f16c9082c85"))
 (fi :source "lockfile" :date
     (25509 30898 568657 383000)
     :recipe
     (:protocol https :remotes "origin" :inherit t :depth 1 :type git :host github :files
		("*.el")
		:repo "leotaku/fi-emacs" :package "fi" :ref "248661773c8db3e02bb8bb6efd9b57214396f9ef"))
 (flycheck-aspell :source "lockfile" :date
		  (25509 30898 566641 337000)
		  :recipe
		  (:package "flycheck-aspell" :fetcher github :repo "leotaku/flycheck-aspell" :files
			    ("flymake-aspell.el")
			    :protocol https :remotes "origin" :inherit t :depth 1 :type git :host github :ref "6efe0ae2d82ba83c563e117b290cb22ae91d2f58"))
 (study :source "lockfile" :date
	(25509 30898 564561 327000)
	:recipe
	(:protocol https :remotes "origin" :inherit t :depth 1 :type git :host github :files
		   (:defaults "study-*.el")
		   :repo "leotaku/study.el" :package "study" :ref "01f2bddb0ce0eb1f4e2a4adedff5346f09ed240b"))
 (theist-mode :source "lockfile" :date
	      (25509 30898 562606 162000)
	      :recipe
	      (:protocol https :remotes "origin" :inherit t :depth 1 :type git :host github :repo "leotaku/theist-mode" :package "theist-mode" :files
			 (:defaults)
			 :ref "278e78e0659dda49cffcb57a1403ae02fb01ceb2"))
 (auctex :source "lockfile" :date
	 (25509 30898 560521 674000)
	 :recipe
	 (:package "auctex" :host github :repo "emacs-straight/auctex" :protocol https :remotes "origin" :inherit t :depth 1 :type git :files
		   (:defaults)
		   :ref "ae16c19a599e6bf4507c167e3c8d34870668710f"))
 (prisma-mode :source "lockfile" :date
	      (25509 30898 558260 978000)
	      :recipe
	      (:protocol https :remotes "origin" :inherit t :depth 1 :type git :host github :repo "pimeys/emacs-prisma-mode" :package "prisma-mode" :files
			 (:defaults)
			 :ref "f7744a995e84b8cf51265930ce18f6a6b26dade7"))
 (doom-themes :source "lockfile" :date
	      (25509 30898 556001 643000)
	      :recipe
	      (:package "doom-themes" :fetcher github :repo "leotaku/emacs-doom-themes" :files
			(:defaults "themes/*.el" "doom-themes-pkg.el")
			:protocol https :remotes "origin" :inherit t :depth 1 :type git :host github :ref "c50eed2ab185c1772aea438e88509e860c143cc1"))
 (ace-link :source "lockfile" :date
	   (25509 30898 553206 535000)
	   :recipe
	   (:package "ace-link" :repo "abo-abo/ace-link" :fetcher github :files
		     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
		      (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		     :protocol https :remotes "origin" :inherit t :depth 1 :ref "77c683ef8aaa89e578d7fb248da69eb2e7245ad1"))
 (ace-window :source "lockfile" :date
	     (25509 30898 550593 450000)
	     :recipe
	     (:package "ace-window" :repo "abo-abo/ace-window" :fetcher github :files
		       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
			(:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		       :protocol https :remotes "origin" :inherit t :depth 1 :ref "43dc77e2136259d5601cc6af5d0f90cbaad79461"))
 (aggressive-indent :source "lockfile" :date
		    (25509 30898 548511 432000)
		    :recipe
		    (:package "aggressive-indent" :repo "Malabarba/aggressive-indent-mode" :fetcher github :files
			      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
			       (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
			      :protocol https :remotes "origin" :inherit t :depth 1 :ref "f376cdc25de5c0f8c330f1e053557d95ca47a540"))
 (amx :source "lockfile" :date
      (25509 30898 546515 968000)
      :recipe
      (:package "amx" :repo "DarwinAwardWinner/amx" :fetcher github :files
		("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
		 (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		:protocol https :remotes "origin" :inherit t :depth 1 :ref "37f9c7ae55eb0331b27200fb745206fc58ceffc0"))
 (apheleia :source "lockfile" :date
	   (25509 30898 544571 326000)
	   :recipe
	   (:package "apheleia" :fetcher github :repo "radian-software/apheleia" :files
		     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
		      (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		     :protocol https :remotes "origin" :inherit t :depth 1 :ref "deab8fb972f0cbc03c6a5409564435121b5db9c2"))
 (avy :source "lockfile" :date
      (25509 30898 542509 296000)
      :recipe
      (:package "avy" :repo "abo-abo/avy" :fetcher github :files
		("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
		 (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		:protocol https :remotes "origin" :inherit t :depth 1 :ref "955c8dedd68c74f3cf692c1249513f048518c4c9"))
 (circe :source "lockfile" :date
	(25509 30898 540490 735000)
	:recipe
	(:package "circe" :repo "emacs-circe/circe" :fetcher github :files
		  ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
		   (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		  :protocol https :remotes "origin" :inherit t :depth 1 :ref "57fe189d7c0b98b9b1b5a59767cea1c7e2c22b13"))
 (company :source "lockfile" :date
	  (25509 30898 538499 970000)
	  :recipe
	  (:package "company" :repo "company-mode/company-mode" :fetcher github :files
		    (:defaults "icons"
			       ("images/small" "doc/images/small/*.png"))
		    :protocol https :remotes "origin" :inherit t :depth 1 :ref "6884e3ad717419b4a64a5fab08c8cb9bd20a0b27"))
 (company-posframe :source "lockfile" :date
		   (25509 30898 536515 567000)
		   :recipe
		   (:package "company-posframe" :fetcher github :repo "tumashu/company-posframe" :files
			     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
			      (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
			     :protocol https :remotes "origin" :inherit t :depth 1 :ref "ab58972c2cebc5ecf68c4cdd140c3aed2c68f42b"))
 (counsel :source "lockfile" :date
	  (25509 30898 534530 457000)
	  :recipe
	  (:package "counsel" :repo "abo-abo/swiper" :fetcher github :files
		    ("counsel.el")
		    :protocol https :remotes "origin" :inherit t :depth 1 :ref "b8be4913a661b557e0d3275726e36871556569d3"))
 (counsel-projectile :source "lockfile" :date
		     (25509 30898 532306 669000)
		     :recipe
		     (:package "counsel-projectile" :fetcher github :repo "ericdanan/counsel-projectile" :files
			       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
				(:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
			       :protocol https :remotes "origin" :inherit t :depth 1 :ref "40d1e1d4bb70acb00fddd6f4df9778bf2c52734b"))
 (crux :source "lockfile" :date
       (25509 30898 530340 794000)
       :recipe
       (:package "crux" :fetcher github :repo "bbatsov/crux" :files
		 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
		  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		 :protocol https :remotes "origin" :inherit t :depth 1 :ref "f8789f67a9d2e1eb31a0e4531aec9bb6d6ec1282"))
 (deadgrep :source "lockfile" :date
	   (25509 30898 528309 719000)
	   :recipe
	   (:package "deadgrep" :repo "Wilfred/deadgrep" :fetcher github :files
		     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
		      (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		     :protocol https :remotes "origin" :inherit t :depth 1 :ref "f687ca31f8d3bd8ebf05165b080b50ba724ce9bf"))
 (dired-filter :source "lockfile" :date
	       (25509 30898 526203 262000)
	       :recipe
	       (:package "dired-filter" :fetcher github :repo "Fuco1/dired-hacks" :files
			 ("dired-filter.el")
			 :protocol https :remotes "origin" :inherit t :depth 1 :ref "41d3eb42195d9f0894c20d18cc8e722b099aa1c1"))
 (diredfl :source "lockfile" :date
	  (25509 30898 523940 916000)
	  :recipe
	  (:package "diredfl" :fetcher github :repo "purcell/diredfl" :files
		    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
		     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		    :protocol https :remotes "origin" :inherit t :depth 1 :ref "94bd99eeced6d52a5a7b9db3745239feafd633e2"))
 (eglot :source "lockfile" :date
	(25509 30898 521905 250000)
	:recipe
	(:package "eglot" :repo "joaotavora/eglot" :fetcher github :files
		  ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
		   (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		  :protocol https :remotes "origin" :inherit t :depth 1 :ref "e501275e06952889056268dabe08ccd0dbaf23e5"))
 (emacsql-sqlite-builtin :source "lockfile" :date
			 (25509 30898 519551 167000)
			 :recipe
			 (:package "emacsql-sqlite-builtin" :fetcher github :repo "magit/emacsql" :files
				   ("emacsql-sqlite-builtin.el")
				   :protocol https :remotes "origin" :inherit t :depth 1 :ref "6b2e65bdf785364cf7c34c31fea5812e1e58c657"))
 (envrc :source "lockfile" :date
	(25509 30898 517279 492000)
	:recipe
	(:package "envrc" :fetcher github :repo "purcell/envrc" :files
		  ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
		   (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		  :protocol https :remotes "origin" :inherit t :depth 1 :ref "417285c4e259abab8ae43e2d72b0e1110563efbc"))
 (forge :source "lockfile" :date
	(25509 30898 515318 790000)
	:recipe
	(:package "forge" :fetcher github :repo "magit/forge" :files
		  ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
		   (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		  :protocol https :remotes "origin" :inherit t :depth 1 :ref "6ff776a651706212dc88f6d1d39a2b57df9d9571"))
 (git-modes :source "lockfile" :date
	    (25509 30898 513325 411000)
	    :recipe
	    (:package "git-modes" :fetcher github :repo "magit/git-modes" :old-names
		      (gitattributes-mode gitconfig-mode gitignore-mode)
		      :files
		      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
		       (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		      :protocol https :remotes "origin" :inherit t :depth 1 :ref "be96ef14fab6a2d76cca3ebf9a15b462a695923d"))
 (go-mode :source "lockfile" :date
	  (25509 30898 511371 494000)
	  :recipe
	  (:package "go-mode" :repo "dominikh/go-mode.el" :fetcher github :files
		    ("go-mode.el")
		    :protocol https :remotes "origin" :inherit t :depth 1 :ref "08aa90d52f0e7d2ad02f961b554e13329672d7cb"))
 (haskell-mode :source "lockfile" :date
	       (25509 30898 509397 462000)
	       :recipe
	       (:package "haskell-mode" :repo "haskell/haskell-mode" :fetcher github :files
			 (:defaults "NEWS" "logo.svg")
			 :protocol https :remotes "origin" :inherit t :depth 1 :ref "a34ccdc54be15043ff0d253c3c20087524255491"))
 (hcl-mode :source "lockfile" :date
	   (25509 30898 507408 105000)
	   :recipe
	   (:package "hcl-mode" :repo "purcell/emacs-hcl-mode" :fetcher github :files
		     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
		      (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		     :protocol https :remotes "origin" :inherit t :depth 1 :ref "e4d9eef631e8a386341ae8f94f7c2579586e65b5"))
 (keyfreq :source "lockfile" :date
	  (25509 30898 505349 736000)
	  :recipe
	  (:package "keyfreq" :fetcher github :repo "dacap/keyfreq" :files
		    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
		     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		    :protocol https :remotes "origin" :inherit t :depth 1 :ref "dd88193cd7a91a92113121191573758ea2a3ceb1"))
 (ledger-mode :source "lockfile" :date
	      (25509 30898 503441 140000)
	      :recipe
	      (:package "ledger-mode" :fetcher github :repo "ledger/ledger-mode" :files
			(:defaults "ledger-test.el")
			:old-names
			(ldg-mode)
			:protocol https :remotes "origin" :inherit t :depth 1 :ref "8bad528d43007e0310b5e72e6e021b502b30495c"))
 (lispy :source "lockfile" :date
	(25509 30898 501493 940000)
	:recipe
	(:package "lispy" :repo "abo-abo/lispy" :fetcher github :files
		  (:defaults "lispy-clojure.clj" "lispy-clojure.cljs" "lispy-python.py")
		  :protocol https :remotes "origin" :inherit t :depth 1 :ref "f35eadf8c1be43a395e196463314b17ea3b4e16f"))
 (lua-mode :source "lockfile" :date
	   (25509 30898 499336 377000)
	   :recipe
	   (:package "lua-mode" :repo "immerrr/lua-mode" :fetcher github :files
		     (:defaults
		      (:exclude "init-tryout.el"))
		     :protocol https :remotes "origin" :inherit t :depth 1 :ref "ad639c62e38a110d8d822c4f914af3e20b40ccc4"))
 (magit :source "lockfile" :date
	(25509 30898 497366 880000)
	:recipe
	(:package "magit" :fetcher github :repo "magit/magit" :files
		  ("lisp/magit" "lisp/magit*.el" "lisp/git-rebase.el" "docs/magit.texi" "docs/AUTHORS.md" "LICENSE" "Documentation/magit.texi" "Documentation/AUTHORS.md"
		   (:exclude "lisp/magit-libgit.el" "lisp/magit-libgit-pkg.el" "lisp/magit-section.el" "lisp/magit-section-pkg.el"))
		  :protocol https :remotes "origin" :inherit t :depth 1 :ref "010fec9cdedb2cbe40fc92b0385823e9a21f9842"))
 (markdown-mode :source "lockfile" :date
		(25509 30898 495387 238000)
		:recipe
		(:package "markdown-mode" :fetcher github :repo "jrblevin/markdown-mode" :files
			  ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
			   (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
			  :protocol https :remotes "origin" :inherit t :depth 1 :ref "d95107f5b77d6c010e89259e05adfcd79a21f26a"))
 (meson-mode :source "lockfile" :date
	     (25509 30898 493243 640000)
	     :recipe
	     (:package "meson-mode" :fetcher github :repo "wentasah/meson-mode" :files
		       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
			(:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		       :protocol https :remotes "origin" :inherit t :depth 1 :ref "82220d12899b87acb3c862b17368f63de7d08d07"))
 (minions :source "lockfile" :date
	  (25509 30898 491269 190000)
	  :recipe
	  (:package "minions" :fetcher github :repo "tarsius/minions" :files
		    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
		     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		    :protocol https :remotes "origin" :inherit t :depth 1 :ref "f7c4767f259bcee0ed39a1846de1818913b274d7"))
 (modalka :source "lockfile" :date
	  (25509 30898 489257 261000)
	  :recipe
	  (:package "modalka" :repo "mrkkrp/modalka" :fetcher github :files
		    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
		     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		    :protocol https :remotes "origin" :inherit t :depth 1 :ref "e674b08c21f24bc32ee8fa3db45ffb0743410bce"))
 (moody :source "lockfile" :date
	(25509 30898 486757 471000)
	:recipe
	(:package "moody" :fetcher github :repo "tarsius/moody" :files
		  ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
		   (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		  :protocol https :remotes "origin" :inherit t :depth 1 :ref "546472d1cabafa092fbc24f467e2acc02fe713ec"))
 (multiple-cursors :source "lockfile" :date
		   (25509 30898 483988 578000)
		   :recipe
		   (:package "multiple-cursors" :fetcher github :repo "magnars/multiple-cursors.el" :files
			     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
			      (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
			     :protocol https :remotes "origin" :inherit t :depth 1 :ref "fe0d5167459b792a699af782685582a195852cb9"))
 (nix-mode :source "lockfile" :date
	   (25509 30898 481803 183000)
	   :recipe
	   (:package "nix-mode" :fetcher github :repo "NixOS/nix-mode" :files
		     (:defaults
		      (:exclude "nix-company.el" "nix-mode-mmm.el"))
		     :protocol https :remotes "origin" :inherit t :depth 1 :ref "54e5626829168e22126b233e079f04dff3c71b90"))
 (no-littering :source "lockfile" :date
	       (25509 30898 479493 289000)
	       :recipe
	       (:package "no-littering" :fetcher github :repo "emacscollective/no-littering" :files
			 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
			  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
			 :protocol https :remotes "origin" :inherit t :depth 1 :ref "fea487d7cb0e9a09fcf0b522c91fbc89d57ef1bb"))
 (org :source "lockfile" :date
      (25509 30898 477510 653000)
      :recipe
      (:package "org" :local-repo "org" :repo "https://git.savannah.gnu.org/git/emacs/org-mode.git" :depth nil :pre-build
		(progn
		  (require 'elpaca-menu-org)
		  (elpaca-menu-org--build))
		:build
		(:not elpaca--generate-autoloads-async)
		:files
		(:defaults
		 ("etc/styles/" "etc/styles/*"))
		:protocol https :remotes "origin" :inherit t :ref "f731d45d28627a9e3c732070e55d68e2b16bda80"))
 (org-reverse-datetree :source "lockfile" :date
		       (25509 30898 475527 88000)
		       :recipe
		       (:package "org-reverse-datetree" :fetcher github :repo "akirak/org-reverse-datetree" :files
				 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
				  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
				 :protocol https :remotes "origin" :inherit t :depth 1 :ref "fca95cd22ed29653f3217034c71ec0ab0a7c7734"))
 (pcre2el :source "lockfile" :date
	  (25509 30898 473545 887000)
	  :recipe
	  (:package "pcre2el" :fetcher github :repo "joddie/pcre2el" :files
		    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
		     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		    :protocol https :remotes "origin" :inherit t :depth 1 :ref "38c6f80c787da547287db96b495e5b695ca0b4b8"))
 (projectile :source "lockfile" :date
	     (25509 30898 471609 55000)
	     :recipe
	     (:package "projectile" :fetcher github :repo "bbatsov/projectile" :files
		       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
			(:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		       :protocol https :remotes "origin" :inherit t :depth 1 :ref "036d327b5ad9b970bd1ea3123692a80bc1015b4a"))
 (rainbow-mode :source "lockfile" :date
	       (25509 30898 469649 29000)
	       :recipe
	       (:package "rainbow-mode" :host github :repo "emacs-straight/rainbow-mode" :protocol https :remotes "origin" :inherit t :depth 1 :files
			 (:defaults)
			 :ref "8e96388fb4d616a9dde23e712bad0d9cd048fbf0"))
 (rust-mode :source "lockfile" :date
	    (25509 30898 467632 461000)
	    :recipe
	    (:package "rust-mode" :repo "rust-lang/rust-mode" :fetcher github :files
		      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
		       (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		      :protocol https :remotes "origin" :inherit t :depth 1 :ref "384051e23f0f2e2950e6ce2efd1a2e8ac0c53613"))
 (swiper :source "lockfile" :date
	 (25509 30898 465431 852000)
	 :recipe
	 (:package "swiper" :repo "abo-abo/swiper" :fetcher github :files
		   ("swiper.el")
		   :protocol https :remotes "origin" :inherit t :depth 1 :ref "b8be4913a661b557e0d3275726e36871556569d3"))
 (terraform-mode :source "lockfile" :date
		 (25509 30898 463188 805000)
		 :recipe
		 (:package "terraform-mode" :repo "emacsorphanage/terraform-mode" :fetcher github :files
			   ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
			    (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
			   :protocol https :remotes "origin" :inherit t :depth 1 :ref "e67459fefc871fdbf20e27be8f85b98b10b97b1b"))
 (undo-fu-session :source "lockfile" :date
		  (25509 30898 460623 434000)
		  :recipe
		  (:package "undo-fu-session" :fetcher codeberg :repo "ideasman42/emacs-undo-fu-session" :files
			    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
			     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
			    :protocol https :remotes "origin" :inherit t :depth 1 :ref "a8f84132f2c6ac35c5a741d2f9912474b429d171"))
 (visual-fill-column :source "lockfile" :date
		     (25509 30898 458143 461000)
		     :recipe
		     (:package "visual-fill-column" :fetcher codeberg :repo "joostkremers/visual-fill-column" :files
			       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
				(:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
			       :protocol https :remotes "origin" :inherit t :depth 1 :ref "453d698d7fc243a547665f8ba43c55eee574e0db"))
 (visual-regexp :source "lockfile" :date
		(25509 30898 456238 646000)
		:recipe
		(:package "visual-regexp" :repo "benma/visual-regexp.el" :fetcher github :files
			  ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
			   (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
			  :protocol https :remotes "origin" :inherit t :depth 1 :ref "48457d42a5e0fe10fa3a9c15854f1f127ade09b5"))
 (web-mode :source "lockfile" :date
	   (25509 30898 454300 82000)
	   :recipe
	   (:package "web-mode" :repo "fxbois/web-mode" :fetcher github :files
		     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
		      (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		     :protocol https :remotes "origin" :inherit t :depth 1 :ref "4b8a695825fda366927894e498421f35fce1cbb9"))
 (wgrep :source "lockfile" :date
	(25509 30898 452374 685000)
	:recipe
	(:package "wgrep" :fetcher github :repo "mhayashi1120/Emacs-wgrep" :files
		  ("wgrep.el")
		  :protocol https :remotes "origin" :inherit t :depth 1 :ref "f9687c28bbc2e84f87a479b6ce04407bb97cfb23"))
 (which-key :source "lockfile" :date
	    (25509 30898 450399 535000)
	    :recipe
	    (:package "which-key" :repo "justbur/emacs-which-key" :fetcher github :files
		      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
		       (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		      :protocol https :remotes "origin" :inherit t :depth 1 :ref "8093644032854b1cdf3245ce4e3c7b6673f741bf"))
 (worf :source "lockfile" :date
       (25509 30898 448516 892000)
       :recipe
       (:package "worf" :repo "abo-abo/worf" :fetcher github :files
		 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
		  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		 :protocol https :remotes "origin" :inherit t :depth 1 :ref "8681241e118585824cd256e5b026978bf06c7e58"))
 (yaml-mode :source "lockfile" :date
	    (25509 30898 446521 801000)
	    :recipe
	    (:package "yaml-mode" :repo "yoshiki/yaml-mode" :fetcher github :files
		      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
		       (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
		      :protocol https :remotes "origin" :inherit t :depth 1 :ref "3fcb36d6039bef57e2a0f6e24c51f623c0bf5fb7")))
