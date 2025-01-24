<div align="center">
    <h1>Leo's Emacs Configuration</h1>
    Robust Emacs configuration using custom tooling
</div>

## About

This is my new Emacs configuration, after having given up my old literate and evil-centric [setup](https://github.com/leotaku/literate-emacs).

## Central Features

+ Keybindings
  + [modalka](https://github.com/mrkkrp/modalka) :: Modal editing
  + [theist-mode (mine)](https://github.com/leotaku/theist-mode) :: Automatic efficient keystrokes
  + [motion (mine)](https://github.com/leotaku/motion.el) :: Minimal vim-like motions
  + [expand-region-improved (mine)](https://github.com/leotaku/expand-region-improved) :: Semantic region selection
+ Configuration
  + [fi-emacs (mine)](https://github.com/leotaku/fi-emacs) :: Miscellaneous functionality
    + bk :: Simple use-package replacement
    + sd :: Traceable Emacs startup
  + [package-vc](https://git.savannah.gnu.org/cgit/emacs.git/tree/doc/emacs/package.texi) :: Native source-based package management
+ Visuals
  + [doom-aurora (mine)](https://github.com/leotaku/emacs-doom-themes) :: Dark theme
  + [moody](https://github.com/tarsius/moody) :: Emacs mode-line

Other than that, we use many more great and helpful packages provided by the Emacs community.
Simply visit [load-packages.el](load-packages.el) to find them.

## Motivation

My previous Emacs configuration had multiple issues.

First of all, it was undeniably bloated.
Being inexperienced in the Emacs ecosystem, I simply added every package I found interesting to my configuration.
This issue was further compounded by my usage of Literate Configuration, as well as my reliance on evil-mode.
Literate Configuration slowed down my feedback loop and also made it harder for me to remove useless parts of my configuration, which I already had spent time documenting.
Evil on the other hand interacts deeply with Emacs internals and in turn makes the entire system harder to predict.

As such, the main goal of my newer configuration has been robustness, determinism and low complexity at reasonable performance cost.
Evil has been replaced with the minimal modalka, which interacts predictably with all Emacs packages, while my own theist-mode largely eliminates the need for excessive custom keybindings or complex modal interfaces.

Another interesting change in the name of determinism is my move away from the much-loved use-package.
For me, the auto-loading of configuration often lead to configuration errors being hidden until I needed to use a specific piece of functionality, at which point I often found fixing the the problem considerably annoying.
In contrast, my own init system now immediately loads all packages and configuration at startup, providing a traceback if any errors occur.
At the cost of startup time, which is irrelevant to me as I use the Emacs daemon, this has provided me with a more consistent and easily modifiable configuration.

## License

This Emacs package is distributed under the terms of the [GPL-3.0-or-later](LICENSE) license, meaning the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

Copyright for all contents of this repository belongs to `Leo Gaskin 2019-2025`.

This project makes use of a high number of Emacs packages.
If you think that the usage of your, or any, package used in this project does not comply with the terms of its license or copyright, please open an issue or pull request.
