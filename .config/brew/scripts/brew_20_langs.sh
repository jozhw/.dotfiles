#!/usr/bin/env bash
#
# Languages — compilers, runtimes, and per-language dev tooling.
#
# Add here: language compilers/runtimes and the tooling that goes with each —
# language servers (LSPs), linters, and formatters. Group additions under the
# matching "### <language>" section below.
# Note: this script also runs a few non-brew installers (rustup, conda, npm).

######################################################################################
### lua

brew install lua    # Lua interpreter

######################################################################################
### r

brew install r         # R statistical computing language
brew install libgit2   # Git library — lets tooling find the repo root from a subdir

######################################################################################
### latex

brew install texlive   # Full TeX Live LaTeX distribution
brew install texlab    # LaTeX language server (LSP)

######################################################################################
### golang

brew install go    # Go compiler and toolchain

######################################################################################
### rust

# rustup installs the Rust compiler and cargo (not available as a brew formula).
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh

brew install rust-analyzer   # Rust language server (LSP)

######################################################################################
### clang / c / c++

brew install llvm           # LLVM/Clang compiler toolchain
brew install clang-format   # C/C++/etc. code formatter

######################################################################################
### markdown

brew install marksman   # Markdown language server (LSP)

######################################################################################
### python

# pyright (the LSP) is installed via conda so it lands in miniconda3/bin/pyright.
brew install --cask miniconda   # Minimal conda Python distribution / env manager
conda init                      # requires miniconda to be installed first
conda deactivate
conda activate base
conda install conda-forge::pyright   # Python language server (LSP)
which pyright
brew install black   # Python code formatter (conda's build misbehaves, so use brew's)
# See also black-macchiato for partial-region Python formatting.

######################################################################################
### misc frameworks

brew install hugo   # Static site generator

######################################################################################
### typescript / javascript (installed via npm)

npm install typescript-language-server typescript      # TypeScript LSP + compiler
npm install typescript-eslint-language-service -D      # ESLint integration for the TS server
npm install prettier                                   # Code formatter for JS/TS/etc.
npm i -g @astrojs/language-server                      # Astro language server (LSP)
npm install -g @mdx-js/language-server                 # MDX language server (LSP)
