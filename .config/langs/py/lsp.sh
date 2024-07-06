#!/bin/bash

# install pyright (lsp) using conda forget to have it in .../miniconda3/bin/pyright

conda init

conda deactivate

conda install conda-forge::pyright

conda activate base

which pyright
