#!/usr/bin/env bash

######################################################################################
### Install programming languages


brew install lua 

# brew install php  # PHP programming language

brew install r

# for latex
brew install texlive



#####################################################################################
### lsp


# Install lsp for rust
brew install rust-analyzer

# Install clang support
brew install llvm
# download clang formatter
brew install clang-format

# Install for md support
brew install marksman

# install pyright (lsp) using conda forget to have it in .../miniconda3/bin/pyright
# Install miniconda for python envs.
brew install --cask miniconda
conda init # make sure that conda is installed before running this
conda deactivate
conda activate base
conda install conda-forge::pyright
which pyright
# for formatting does not work with conda for some reason
brew install black
# install black-macchiato for python partial formatting with python-black

