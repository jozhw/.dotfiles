#!/bin/bash

# set to home directory
cd $HOME

# change directory to two directories up to access the local harddrive to transfer
cd ..
cd ..

# get user input if want to run a dry run or not
read -p "Would you like to run a dry-run? Enter 0 for yes and 1 for no." input

# adjust the code here if you do not want to have the --delete flag
# --exclude='**/.DS_Store' will exclude all instances of .DS_Store
if [ "$input" -eq 0 ]; then
    echo "Running dry run of rsync script"
    rsync -anPv --exclude='**/.DS_Store' --delete Users/johnz.wu/trove Volumes/johnzwu/backup/jozhw
elif [ "$input" -eq 1 ]; then
    echo "Running rsync script"
    rsync -aPv --exclude='**/.DS_Store' --delete Users/johnz.wu/trove Volumes/johnzwu/backup/jozhw
else
    echo "No command selected"
fi

