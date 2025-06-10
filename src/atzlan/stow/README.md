# stow

Stow is a GNU tool that is used to create symbolic links. It is ideal for maintaining and transfering dotfiles, which are generally stored in a `.dotfiles` rep.

## stow_same_root.sh

This script will create a symbolic link using `stow` in the directory of interest, which in the template is `.bin` from the directory that will be stowed (`atzlan` in the example). The script does not retain the dir structure of the stowed directory. 
Within the `stowed_scripts()` method, it stows the scripts only, not retaining the directory structure, and adds executable permissions if it is an executable script that does not currently have permissions.

