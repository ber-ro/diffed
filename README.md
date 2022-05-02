[![MELPA](https://melpa.org/packages/diffsync-badge.svg)](https://melpa.org/#/diffsync)

# diffsync
Emacs major mode which uses recursive diff output as a base for synchronization.
Can be used somewhat like ediff-directories, but without the need to switch to another buffer to view a subdirectory or to copy a file to the opposite directory.

Only the list of files is visible on startup, so each line corresponds to one file on the left and/or the right side. Detailed file differences are hidden, but can be toggled.

Operations work on the file(s) in the current line (where the cursor is).
There are shortcuts for the following operations: copy/move file to opposite directory, delete file(s), ediff-files, find-file.

![Screenshot](screenshot.png)
