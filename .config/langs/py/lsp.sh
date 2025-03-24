#!/bin/bash

# install pyright (lsp) using conda forget to have it in .../miniconda3/bin/pyright

conda init

conda deactivate

conda activate base

conda install conda-forge::pyright

which pyright

# for formatting does not work with conda for some reason
brew install black

# install black-macchiato for python partial formatting with python-black
