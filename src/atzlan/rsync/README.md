# rsync

## Getting Started

```
rsync [option] [source] [destination]
```

## Flags

### -P (partial + progress)

It keeps partially transferred files (if transmission is aborted, you don’t
need to start over) and also shows you progress.

### -z

Compress the files before sending.

This is ideal for transfering over low bandwidth links, but be wary of
CPU usage

### -a (archive)

Preserves everying such as permissions, ownership, file modification, and etc.

### -r (recursive)

Copies data recursively, but does not preserve timestamps and permissions when
transferring data.

### -n (dry run)

Dry run of copying. Does not actually copy anything, but it is good to use
as a test before offical run.

### -h (human readable)

Output in human readable output.

### -v (verbose)

Prints the files.

### --delete

Remove all files that are not in the destination directory that is not
in the source directory.

### --progress

Prints a progress bar.

### --exclude

Excludes a file or directory.

## How To

### Exclude all instances of a file or directory

To exclude all instances of a directory:

```
--exclude='**/<dir>'
```

To exclude all instances of a file:

```
--exclude='**/<file_name>'
```

To exclude all instances of a file type:

```
--exclude='*.<file_type>'
```
