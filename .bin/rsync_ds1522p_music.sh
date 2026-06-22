#!/bin/bash

# set to home directory
cd $HOME

# change directory to two directories up to access the local harddrive to transfer

# get user input if want to run a dry run or not
read -p "Would you like to run a dry-run? Enter 0 for yes and 1 for no: " input

# adjust the code here if you do not want to have the --delete flag
# --exclude='**/.DS_Store' will exclude all instances of .DS_Store
if [ "$input" -eq 0 ]; then
    echo "Running dry run of rsync script"
    rsync -anPv --no-times --exclude='**/.DS_Store' Core/wu/wuMedia/Music/* jozhw@ds1522p.wu.ventures:/volume1/Trove/Music/navidrome__music/music__library/

elif [ "$input" -eq 1 ]; then
    echo "Running rsync script"
    rsync -aPv --no-times --append-verify --exclude='**/.DS_Store' Core/wu/wuMedia/Music/* jozhw@ds1522p.wu.ventures:/volume1/Trove/Music/navidrome__music/music__library/
   

else
    echo "No command selected"
fi
