#!/bin/bash

# note that this script will cause the video to be downloaded to the current
# working directory

read -p "Enter the youtube video id: " video_id

# store the current working directory
original_dir=$(pwd)

# prompt to enter directory path to store the video
read -p "Enter the directory path: " input_dir

# check if the directory exists
if [ ! -d "$input_dir" ]; then
    echo "Error: Directory '$input_dir' does not exist."
    exit 1  # exit the script with an error status
fi

# change to the directory of interest where the video will be stored
cd "$input_dir"

# command to download the video using yt-dlp
yt-dlp https://youtu.be/"$video_id"

# return to the original working directory
cd "$original_dir" || exit



