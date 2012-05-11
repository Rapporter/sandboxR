# Major tasks

 * ~~Redesign sandboxR to run in an environment with dummy forbidden functions loaded.~~
 * ~~Tidy up unnecessary regexps and checks from `sandbox` thanks to the above!~~

# Check up at least twice!

 * masked fn of `latticeParseFormula` is really needed? sandboxR environment seems to handle malicious calls hidden inside without updating that in `lattice` namespace

# Allow (and create) masked functions for the followings

## Base

 * ~~get~~, mget
 * ~~assign~~
 * ~~ls~~
 * attach, detach (_wontfix_)
 * parse, deparse
 * ~~eval~~
 * do.call (safe.call from ggplot2) etc.
 * exists
 * is.call, as.call, call
 * ~~library, require~~
 * rm, remove (_wontfix_: expressions run in a temporary environment, `gc` will arrange that automatically)

## Stats, graphics, grDevices, datasets

All done, no further functions will be permitted.

## Methods

All functions to be revised (nothing permitted ATM).

## Utils

 * lsf.str, ls.str

# Possible hacks to sort out

 * ~~Check for `::` in calls not to allow running commands from unloaded packages.~~
 * ~~base::body~~
 * ~~quoted function calls~~
 * ~~base::rawToChar~~
 * ~~unexposed function calls~~
 * ~~functions as symbol~~

# Further packages to permit (backlist)

 * ~~grid~~
 * ~~lattice~~
 * parralel (_wontfix_ for RApache environment ATM)
 * reshape, reshape2, plyr
 * ~~ggplot2~~
 * wordcloud, treemap, scatterplot3d etc.
 * ~~nortest~~
 * ~~outliers~~
 * ~~descr~~
 * ~~rapport~~
 * ...
