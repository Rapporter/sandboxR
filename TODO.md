Allow and create masked functions for the followings:

 * get, mget
 * assign
 * attach, detach

Possible hacks:

 * ~~Check for `::` in calls not to allow running commands from unloaded packages.~~
 * ~~base::body~~
 * ~~quoted function calls~~
 * ~~base::rawToChar~~
