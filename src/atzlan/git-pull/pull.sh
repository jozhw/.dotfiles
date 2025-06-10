#!/usr/bin/env bash
# pull from remote for select projects given the relative path from projects
# follow the structure {name-project}/name for the current working project
# the directories array is the list of repositories of interest for pulling
#
# store the current working directory
original_dir="$PWD"

# get the absolute path of the directory containing the script
# BASH_SOURCE extracts the directory of where the source script is
script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# change to the script's directory
cd "$script_dir" || exit

# get list of directories to run the command
declare -a directories=(
  "stdirgen-project/stdirgen"
  "executor-project/executor"
)

for directory in "${directories[@]}"; do
  # Use double quotes for variables and add comments for clarity
  current_project="./$directory"
  
  # Perform the pull using the specified git directory and work tree
  git --git-dir="$current_project/.git" --work-tree="$PWD/$current_project" pull origin main
done

# return to the original working directory
cd "$original_dir" || exit
