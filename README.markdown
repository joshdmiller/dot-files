# Josh Miller's Dot Files

Technically speaking, in computing a "dot file" is a hidden directory or
file on a UNIX/Linux system that are so named because they are prefixed
with a full stop. Dot files within a user's home directory contain most of
the user-specific configuration available on a desktop and are configured
manually or through the use of tools. 

While not all strictly dot files, the files contained here are some of the
customization I regularly use on my Linux machines as well as on the Linux
machines of those whom I convert from the lesser operating systems. They
are offered here with plentiful documentation in the hopes that they will
be useful to others, whether as an inspiration to explore some different
areas of Linux, as a shortcut to get a Linux machine up and running, or as
a guide for users not familiar with how to configure some of these
applications (like xmonad).

The below tree is an overview of what's contained in this respository or
configuration information. In each directory of this repository is a `README`
file providing more information.

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

