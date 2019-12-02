## repo.el - Repo integration with emacs

This package provides integration of the Google Repo tool with
emacs. Its main features are:

* Display output of the repo status command in a buffer,

* Launches magit from the status buffer for project under point.

## Usage

Make sur the repo tool is in your path. Call `M-x repo-status`, it
will prompt for a repo workspace and display the workspace status in a
new buffer.

In the status buffer, hitting `g` will update the buffer. Hitting `RET` on the
"Workspace:" line will open dired in the workspace directory. Hitting `RET` on
the "Branch ...:" line will run `magit-status` in the ".repo/manifests"
directory. On a project line, `RET` will run `magit-status` for that
project. Finally, `RET` on a file will open the file at point.

## Todo

I suppose there could be much more things to do with this and repo but
for now it's enough for my usage. If you have patches or ideas, they
are welcome.

## Development

Development is done on
[GitHub](https://github.com/canatella/repo-el). Any bugs or patches
are welcome.

## License

This emacs extension is distributed under the terms of the GPLv3. See
COPYING file for details.
