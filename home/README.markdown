# $HOME

The `$HOME` directory contains all of my user-specific configurations in the
form of "dot files" - hidden files and directories whose name is prefixed by a
full stop. For ease of browsing this repository, all dot files are missing
their preceeding dot. Should these files be used in your `$HOME` directory, the
dot must be added back. The installation script in the parent directory will
ensure that all files are named correctly.

As with all other folders, the files within `$HOME` have been tested only in
Arch Linux.

## Contents

    home/                   - my `$HOME` directory (e.g. `/home/joshua`)
      |-- bash/             - various bash configuration files and helpers
      |-- urxvt/            - urxvt configuration files
      |   |-- perl/         - perl extensions for urxvt
      |-- vim/              - Vim configuration files
      |   |-- packages/     - Vim plugins as submodules
      |   |-- pathogen/     - Pathogen package management system for Vim as a submodule
      |-- xmonad/           - xmonad configuration files
      |   |-- bin/          - conky configurations and helper scripts for xmonad
      |   |-- icons/        - icons for use with conky inside xmonad
      |   |-- xmonad.hs     - the primary xmonad configuration file
      |-- bashrc            - the primary Bash configuration script
      |-- Xresources        - the primary Bash configuration script
      |-- vimrc             - the primary Bash configuration script

