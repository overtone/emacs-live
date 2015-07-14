# actionscript-mode

This repository contains an actionscript-mode for Emacs and a config
file that includes some small optional utilities and config options.

Please note that I rarely hack on Actionscript anymore, so don't
expect a lot of updates or fixes.

Feel free to fork this repo and make it your own!

You may also want to check here for alternatives:
http://www.emacswiki.org/emacs/ActionScriptMode

## Functions

### as-print-func-info

Insert a print statement immediately after the nearest function
definition before point.

This is bound to `F5`.

### as-insert-trace

Insert an empty trace call at point. If point is inside a word, then
trace that word on the next line.

This is bound to `C-c C-t`.

### update-etags

Update the etags table. (You might need to customize this depending
on where you store your `TAGS` file.)

### insert-flash-boilerplate

When you open a new file, this function is automatically called to
insert some boilerplate code. You might want to customize that.

## License

Copyright (c) 2011 Austin Haas.

Licensed under the [GNU General Public License, version 2](blob/master/LICENSE.txt).
