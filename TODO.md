Allow and create masked functions for the followings:

 * get, mget
 * assign
 * attach, detach

~~Redesign sandboxR to run in an environment with dummy forbidden functions loaded.~~
**Tidy up unnecessary regexps and checks from `sandbox` thanks to the above!**

Possible hacks:

 * ~~Check for `::` in calls not to allow running commands from unloaded packages.~~
 * ~~base::body~~
 * ~~quoted function calls~~
 * ~~base::rawToChar~~
 * ~~unexposed function calls~~
 * ~~functions as symbol~~ 