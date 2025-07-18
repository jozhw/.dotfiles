#!/bin/bash

# convert readme.org for starlight docs
python src/scripts/org_to_md.py README.org -o docs/src/content/docs/overview

# convert emacs.org for starlight docs
python src/scripts/org_to_md.py Emacs.org -o docs/src/content/docs/emacs
