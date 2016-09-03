
Provides instant haskell error feedback inside of neovim.
This should be a lot faster than running neomake with ghc-mod, and
also a lot simpler.

![Obligatory gif][1]

[1]: https://github.com/cloudhead/images/raw/master/neovim-ghcid.gif

### Requirements

  * neovim >= 0.1.5 (https://github.com/neovim/neovim)
  * ghcid >= 0.6.5

### Installation

  If you're using vim-plug, then add the following line to your list of plugins:

      Plug 'ndmitchell/ghcid', { 'rtp': 'plugins/nvim' }

  Then run `:PlugInstall`.

  For vundle, add the following:

      Plugin 'ndmitchell/ghcid', { 'rtp': 'plugins/vim/' }

  Then run `:PluginInstall`.

  Alternatively, copy the files in this folder to to your .vim directory.

### Usage

  `:Ghcid` runs ghcid inside a neovim terminal buffer and populates
  the quickfix list with any errors or warnings. You can call `:Ghcid`
  to toggle the window.

  After every file save, the quickfix list is updated with the output
  of ghcid.

  `:GhcidKill` kills the ghcid job.

