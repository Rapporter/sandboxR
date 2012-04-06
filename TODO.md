**Major tasks:**

 * ~~Redesign sandboxR to run in an environment with dummy forbidden functions loaded.~~
 * _Tidy up unnecessary regexps and checks from `sandbox` thanks to the above!_

**Allow and create masked functions for the followings:**

 * ~~get~~, mget
 * ~~assign~~
 * ~~ls~~
 * attach, detach

I never planned, but _eval_, _evalq_ and _local_ are also enabled now.

**Possible hacks:**

 * ~~Check for `::` in calls not to allow running commands from unloaded packages.~~
 * ~~base::body~~
 * ~~quoted function calls~~
 * ~~base::rawToChar~~
 * ~~unexposed function calls~~
 * ~~functions as symbol~~ 