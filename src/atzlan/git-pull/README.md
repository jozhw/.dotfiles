# pull.sh

The following script is self-explanatory. It runs a git pull origin for each
repository listed in the **directories** variable.

Note: make sure that the the directory of interest has its relative path from
where the pull.sh script is as an item in the driectories array.

## Improved Usecase

For the pull.sh, I usually keep the pull.sh script as superfical as possible
to the **User** directory. That way I can keep track of where all of the pull
scripts are.

### Executor

For the pull.sh, I recommend using a rust cli tool that I developed
called **executor**. Executor recursively executes or searches files. I found it
useful to keep track of where my scripts are and also to execute all of my pull.sh
scripts at once from the **User** directory or the most superfical
directory to get all of the scripts.

For more details regarding **executor** please see the following link
to the githib repo: [executor](https://github.com/jozhw/executor)
