<div align="center">
    <h1><i>Fi-Emacs</i></h1>
    Robust Emacs configuration using custom tooling
</div>

## About

This is my new Emacs configuration, after having given up my old literate and evil-centric [setup](https://github.com/leotaku/literate-emacs).

## Central Features

+ Keybindings
  + [modalka](https://github.com/mrkkrp/modalka) :: Modal editing
  + [theist-mode (mine)](https://github.com/leotaku/theist-mode) :: Automatic efficient keystrokes
  + [expand-region-improved (mine)](https://github.com/leotaku/expand-region-improved) :: Semantic region selection
+ Configuration
  + [fi-emacs (mine)](https://github.com/leotaku/fi-emacs) :: Miscellaneous functionality
    + bk :: Faster, more reliable use-package replacement
    + sd :: Asynchronous, traceable Emacs startup
  + [straight](https://github.com/raxod502/straight.el) :: Package management based on Git
+ Visuals
  + [doom-aurora (custom)](https://github.com/leotaku/emacs-doom-themes) :: Dark theme
  + [moody](https://github.com/tarsius/moody) :: Emacs mode-line
+ Fonts
  + [Fira Mono](https://github.com/mozilla/Fira) :: Monospaced font
  + [Alegreya Sans](https://github.com/huertatipografica/Alegreya-Sans) :: Variable font

Other than that, we use many more great and helpful packages provided by the Emacs community.
Simply visit [package-set.el](package-set.el) to find them.

## Motivation

My previous Emacs configuration had multiple glaring issues.

First of all, it was undeniably bloated.
Me being inexperienced in the Emacs ecosystem, I simply added every package I could find to my configuration.
This issue was further compounded by my usage of Literate Configuration, as well as my reliance on evil-mode.
Literate Configuration majorly slowed me down and prevented me from removing useless parts of my configuration, which I had spent time documenting.
Evil on the other hand interacts so deeply with Emacs internals that it makes the entire system essentially completely unpredictable.

Even the much loved use-package, in my opinion, only amplifies these issues.
By encouraging users to defer parts of their configuration, bugs become harder to reproduce.
Moreover, the non-insignificant amount of magic involved makes understanding any configuration harder than necessary.

As such, the main goal of my newer configuration has been robustness, determinism and low complexity at reasonable performance cost.
Evil has been replaced with the decidedly minimal modalka, which interacts predictably with all packages in the non-evil ecosystem, while my own theist-mode largely eliminates the need for excessive custom keybindings or complex modal interfaces, which are all too common in evil-based configurations.

As for configuration utilities, I have replaced use-package with parts of my modular fi-emacs system, which this configuration also inherits its name from.
Unlike use-package, this system works by defining an explicit tree of dependencies and loading said tree at specific times during the Emacs initialization.
This allows for an entirely deterministic startup sequence, as well as detailed error backtraces.
Still, in order to improve performance, large trees of dependencies may be loaded concurrently and during normal Emacs operation.
The modular and minimal architecture should also aid in adapting fi-emacs to newer features of emacs (e.g. parallelism, dumping).

## License

This Emacs package is distributed under the terms of the [GPL-3.0-or-later](LICENSE) license, meaning the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

Copyright for all contents of this repository belongs to `Leo Gaskin 2019-2020`.

This project makes use of a high number of Emacs packages.
If you think that the usage of your, or any, package used in this project does not comply with the terms of its license or copyright, please open an issue or pull request.
