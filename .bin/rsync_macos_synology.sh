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
    rsync -anPv --exclude='**/.DS_Store' --delete Otzar jozhw@ds1522p.winterume.com:/volume1/johnz.wu
    rsync -anPv --exclude='**/.DS_Store' --delete Downloads/books/* jozhw@ds1522p.winterume.com:/volume1/Downloads/books/

    rsync -anPv --exclude='**/.DS_Store' myOtzar__archive jozhw@ds1522p.winterume.com:/volume1/johnz.wu
    rsync -anPv --exclude='**/.DS_Store' wuMedia jozhw@ds1522p.winterume.com:/volume1/wu
    rsync -anPv --exclude='**/.DS_Store' wuArchive-v1 jozhw@ds1522p.winterume.com:/volume1/wu/wuArchive

    rsync -anPv --exclude='**/.DS_Store' Archival-Library/* jozhw@ds1522p.winterume.com:/volume2/Archival-Library/



elif [ "$input" -eq 1 ]; then
    echo "Running rsync script"
    rsync -aPv --exclude='**/.DS_Store' --delete Otzar jozhw@ds1522p.winterume.com:/volume1/johnz.wu   
    rsync -aPv --exclude='**/.DS_Store' --delete Downloads/books/* jozhw@ds1522p.winterume.com:/volume1/Downloads/books/

    rsync -aPv --exclude='**/.DS_Store' myOtzar__archive jozhw@ds1522p.winterume.com:/volume1/johnz.wu
    rsync -aPv --exclude='**/.DS_Store' wuMedia jozhw@ds1522p.winterume.com:/volume1/wu
    rsync -aPv --exclude='**/.DS_Store' wuArchive-v1 jozhw@ds1522p.winterume.com:/volume1/wu/wuArchive

    rsync -aPv --exclude='**/.DS_Store' Archival-Library/* jozhw@ds1522p.winterume.com:/volume2/Archival-Library/
else
    echo "No command selected"
fi
