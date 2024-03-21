# set ~/.bin to PATH
export PATH="$HOME/.bin:$PATH"

# add conda initialization to all shell types 
eval "$(conda "shell.$(basename "${SHELL}")" hook)"
