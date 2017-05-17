A tool for managing manually sorted INI files.

Sometimes (part of) a configuration is backed up as INI file, that is manually
sorted to ease its understanding. Keeping track of changes when a new INI file
is generating can be hard, since the generated file might be in a completely
different order.

This program helps by sorting the generated file according to the manually
sorted file. Combined with a regular diff/merge tool, this greatly eases the
task of managing configuration backups.
