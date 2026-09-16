# .dotfiles


## Version Control

To keep all of the commits organized, I suggest for my own reference to use the following commit syntax. The first is the action (i.e. fix, add, rm) and then the functionality that was changed (i.e. emacs, readme, stow) followed by the commit message.

```bash
<ACTION>/<FUNCTIONALITY_CHANGED>: MESSAGE HERE
```

The repo uses `git` and Git LFS. Make sure `git lfs` is installed before cloning; `.gitattributes` records the file types managed by LFS. There are currently no active Git submodules.

Releases use semantic-version tags with a leading `v`. See the [release checklist](https://dotfiles.jozhw.com/guides/site/#cutting-a-release) before pushing a tag.


## Setting Up

### brew

All Homebrew scripts live under `.config/brew/scripts/`.

#### brew.sh

The script was heavily inspired from [[https://github.com/mathiasbynens/dotfiles/blob/main/brew.sh][mathiasbynens dotfiles github repo]], but adjusted to suit my needs. A notable difference between my "brew.sh" is that it is modularized with the brew.sh being the wrapper that will download all of the brew files.

Make sure the wrapper is executable by running `chmod +x .config/brew/scripts/brew.sh`.

To run the script:

#+begin_src shell

./.config/brew/scripts/brew.sh

#+end_src

#### brew_<NN>_<category>.sh

The purpose of these scripts are to categorize brew installs. The wrapper, =brew.sh=, when executed will iterate (in filename order) through all of the =brew_*.sh= scripts within the same directory as the `brew.sh` and =source= each one.

Each category script is named =brew_<NN>_<category>.sh=:

- =<NN>= is a two-digit number that both **groups** related scripts and **fixes the run order** (lower numbers run first). Numbers are spaced in ranges (00, 10, 20, ...) so new categories can be slotted in between without renaming existing files.
- =<category>= names the bucket, e.g. =system=, =files=, =langs=, =cask=.

The ranges currently in use:

| Range | Group    | Scope                                                    |
| :---- | :------- | :------------------------------------------------------- |
| 00-09 | system   | GNU/updated replacements for built-in macOS tools        |
| 10-19 | cli      | files, search, compression, shell, data pipelines        |
| 20-29 | dev      | languages, LSPs, formatters, version control             |
| 30-39 | media    | audio/video, graphics, documents                         |
| 40-49 | network  | web browsing and downloading                             |
| 50-59 | security | CTF / pentest / forensics tooling                        |
| 60-69 | apps     | GUI applications (Homebrew casks)                         |
| 70-79 | misc     | everything else (personal information management, etc.)   |


### stow

To stow (create symbolic links), after installing gnu stow, run the script in the `/.config/stow/stow.sh` script.

## Configurations

See the [documentation site](https://dotfiles.jozhw.com) for configuration and operational notes.

### emacs

All emacs configurations are written via a literate configuration in `org-mode` named `Emacs.org`. Each section in `Emacs.org` is modularized via =tangle= upon saving the file. All of the modularized configs are stored within the =.emacs.d= directory.

### Emacs packages

Emacs packages are managed with `straight.el`. Package checkouts and build artifacts under `.emacs.d/straight/` are local generated state; the package declarations live in `Emacs.org`.

### .profile

`.profile` serves as a generic shell configuration that will be applied in all shell sessions (bash or zsh).

To find which shell you are using, simply enter the command `echo %SHELL`.

To switch default shells, enter the command `chsh -s <PATH_TO_SHELL>`. The `<PATH_TO_SHELL>` typically is in the form of `/bin/bash` for =bash= and `/bin/zsh` for `zsh`.
   
### macos

The =.macos= file is used to configure macs. The template was taken from [@mathiasbynens](https://github.com/mathiasbynens/dotfiles/blob/main/.macos) and adjusted for my needs.

## Source Code 

Consists of scripts and "repos" that are part of my overall setup, not just on my local machine. The rationale behind this is because monorepos may be better suited for this.

### Garage

This repo serves as a place that consolidates all of my services that I run in my home laboratory along with all of the instructions and config files necessary to get these applications up and running. 

#### Structure

Each service has its own directory and within each service directory contains subdirectories that specify the major services used for setup. This is done so that replicating would be easy — minimal changes needed to get things working.


#### Docker

To be consistent with the philosophy of reproducibility, `docker-compose.yaml` files are used if possible. 

It is important to note that the docker compose files may have a `.env` file that must be created within the same directory as the `docker-compose.yaml`. 


### Atzlan

Atzlan a Repository to be Lazy

#### IMPORTANT

All the scripts, unless otherwise noted, are geared towards macos users using bash shell.
Linux users may benefit depending on their shell configurations.

#### What is Atzlan?

"Atzlan" is the English translation of the Hebrew word for lazy, indolent, or sluggish.
As the name implies, this repository is for those who seek to be "lazy" or are "lazy."
Joking aside, the purpose of this respository is to house scripts that can make you life
easier.

#### Developers

##### Executable Scripts

Make sure that the scripts are executable and according to your operating system.

To make a script executable, for unix users just do the following:

```bash
  chmod +x <NAME_OF_EXECUTABLE_SCRIPT>
```

Make sure that you read the documentation for each of the scripts in order that you have the right dependencies installed in order for the script to work

## Documentation

Please see the provided link to the documentation - [dotfiles.jozhw.com](https://dotfiles.jozhw.com).

Note that the documentation is what I use to keep track of any details/information that I find useful, hence the purpose is not for others to use, meaning you will not find the documentation to be "organized" nor "intuitive".
