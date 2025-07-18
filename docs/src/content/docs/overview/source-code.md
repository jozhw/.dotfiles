---
title: Source Code
description: Documentation for Source Code
---


Consists of scripts and "repos" that are part of my overall setup, not just on my local machine. The rationale behind this is because monorepos may be better suited for this.

## Garage

This repo serves as a place that consolidates all of my services that I run in my home laboratory along with all of the instructions and config files necessary to get these applications up and running. 

### Structure

Each service has its own directory and within each service directory contains subdirectories that specify the major services used for setup. This is done so that replicating would be easy — minimal changes needed to get things working.


### Docker

To be consistent with the philosophy of reproducibility, `docker-compose.yaml` files are used if possible. 

It is important to note that the docker compose files may have a `.env` file that must be created within the same directory as the `docker-compose.yaml`. 


## Atzlan

Atzlan a Repository to be Lazy

### IMPORTANT

All the scripts, unless otherwise noted, are geared towards macos users using bash shell.
Linux users may benefit depending on their shell configurations.

### What is Atzlan?

"Atzlan" is the English translation of the Hebrew word for lazy, indolent, or sluggish.
As the name implies, this repository is for those who seek to be "lazy" or are "lazy."
Joking aside, the purpose of this respository is to house scripts that can make you life
easier.

### Developers

#### Executable Scripts

Make sure that the scripts are executable and according to your operating system.

To make a script executable, for unix users just do the following:

```shell

  chmod +x <NAME_OF_EXECUTABLE_SCRIPT>

```

Make sure that you read the documentation for each of the scripts in order that you have the right dependencies installed in order for the script to work
