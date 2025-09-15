#!/usr/bin/env bash

######################################################################################
### lua

brew install lua 

######################################################################################
### r

brew install r

# to get root git working dir if current working dir is nested
brew install libgit2


######################################################################################
### latex

brew install texlive

# install latex lsp
brew install texlab

######################################################################################
### golang

brew install go

######################################################################################
### rust

# Installing rust
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh

# Install lsp for rust
brew install rust-analyzer

######################################################################################
### clang

# Install clang support
brew install llvm
# download clang formatter
brew install clang-format

######################################################################################
### md

# Install for md support
brew install marksman


######################################################################################
### python

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

#####################################################################################
### misc frameworks

# install hugo for static site generation
brew install hugo


#####################################################################################
### ts/js

# install typescript language server
npm install typescript-language-server typescript

# install eslint plugin
npm install typescript-eslint-language-service -D

# install prettier for ts code formatting
npm install prettier

# install astro lang server
npm i -g @astrojs/language-server

# install mdx lang server
npm install -g @mdx-js/language-server
