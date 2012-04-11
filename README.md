# sandboxR: *filtering "malicious" calls*

## Preface

This **POC** [R](http://www.r-project.org/) package tries to filter "malicious" calls in R expressions based on a blacklist to let shared R instances **be safe from file and system calls**.

*If you are not the kind of person who likes to read much in the morning about a $n+1^{th}$ R package's theory and background, then please strike out for [testdriving the package in a browser](http://sandboxr.no-ip.org/) and **try to hack my system** with some guidance (see below)!*

Please note that I am aware of [Apparmor](http://wiki.apparmor.net/index.php/Main_Page), [SELinux](http://selinuxproject.org/page/Main_Page), [Tomoyo Linux](http://tomoyo.sourceforge.jp/index.html.en) and other Mandatory Access Control based filters **and** this package does not intend to be used instead of those implementations!

But there are some situations when a MAC based, kernel-level (mostly path based) filter cannot secure a system from a point of view. Just think of logs and other commonly writable files, not to mention the executable/memory mappable libraries. For example you might create a web application with the really great tool of @Jeff ([RApache](http://rapache.net/)) or @Jeroen's similarly handy [Opencpu](http://opencpu.org/) and would leave the `tempdir` system-wide writable to store generated images, uploaded files etc.

### Questions, motivations behind this package

Is it a good practice to set some MAC based filter not to allow users to reach other files on the server besides e.g. `/tmp`? Would not the users mess up each others files on purpose or by chance?

Are you sure some executable files in `lib` would not harm your system somehow?

How do you know what kind of diabolic actions could happen to your server by installing some random package from Github with the help of `devtools` by some of your users? Of course MAC filter would stop all (most) of the tries, but just imagine if someone would package some nice root exploit :)

Well, this latter is rather sci-fi, but the above questions do stand in some situations. This package is and idea for those, who are interested in such environments.

## Guidelines 

The main idea for this little package was to behave as a wrapper in **web applications** - where file and system calls are not needed based on the followings:

 * images generated in some R code are saved to disk usually by some internal ways (users should not try to issue some `pdf` or `png` calls on the disk),
 * datasets are usually uploaded by some internal ways after some checks (users should never try to run e.g. `read.table` agains a file on the disk or a remote URL),
 * users should never touch filesystem outside of their little world (which is mostly manageable with Apparmor - but with limitations, e.g.: you cannot secure the `tempdir` or any other common directory, or even your logs!),
 * and users should not deal with R environments as the web applications would prepare and set all those for them,
 * users would not use R internal and deprecated functions.

Besides these I kept the following guidelines in my head too to make an even **stricter sandbox-like environment**:

 * users should not use the web application for testing/development purposes (by-by `debug`, most of `utils` and `methods`, profiling etc.),
 * users might create some small functions in their files but would not deal with namespaces, `.Fortran` calls etc. If someone needs some more complex functions and methods, it should end up in a package hopefully on Github or even CRAN :)
 * users should not call R packages directly, the server would load all required/available packages on startup,
 * and no need for user enabled character encoding functions - as a web application would store everything in the same encoding,
 * users would not want to run spell check and other strange stuff from R on the server,
 * and of course no interactive terminal is supposed.

Based on these I compiled a quite long list of functions that should be **blacklisted**.

The blacklisted functions are checked in the passed R sources:

 * if they are called (e.g.: `system('cat /etc/passwd')`),
 * if those are attempted to be forked (e.g.: `foo <- system`),
 * if those could be found as symbol (e.g.: `(system)`),
 * if those are called in any formula to be evaluated outside of sandbox (e.g.: `lm("system('ls')")`).  

## Apologetics

*Bear in mind that this package is still in development and is not (**might not ever will be**) ready for production!*

As being a *pre-alpha* release you would find too much restrictions in this approach ATM, as for example the following functions are also blacklisted (for simplicity - **later will be enabled** for sure):

 * ~~get~~, mget
 * ~~assign~~
 * ~~ls~~, ~~objects~~
 * ~~library~~, ~~require~~
 * ~~eval~~
 * etc.

For a detailed workflow plan, please check out my [TODO file](https://github.com/daroczig/sandboxR/blob/master/TODO.md)!

Also as I am not sure in this package's success, only base packages (`base`, `utils`, `methods`, `stats`, `graphics` and `grDevices`) are addressed.

## Testdrive!

Anyway, please feel free to **try** *and* **test** a [live (simple) web application which was build to test *sandboxR*](http://sandboxr.no-ip.org/)!

There I would **ask you to your best at trying to hack the server**, like:

 * reading the system-wide readable `/sandbox/secret` file from R,
 * try to write something in the system-wide writable `/sandbox/hello` file from R,
 * or simply try to figure out the root password on the machine :)

Please do [send me feedback](https://github.com/daroczig/sandboxR/issues/new) if you'd succeed or you are tired of the too sharp restrictions!

## Frequently asked questions

Please see in dedicated file ([FAQ.md](https://github.com/daroczig/sandboxR/blob/master/FAQ.md)).

## License

In short: this pseudo-package is licensed under **AGPL**.

More about this (and if I would misinterpret AGPL than this applies): please feel free to copy, use or modify/extend the sources for any open-sourced project. **But**: nor the sources, nor my simple ideas expressed on this site are allowed to use without my permission in any application which does not let users download its sources :)

## Special thanks

I would like to express my gratitude towards:

 * Aleksandar Blagotić (@aL3xa) for working together
 * Jeroen Ooms (@jeroenooms) for security related discussions, for his hints and for his unbelief :)
 * [@DWin and Hadley Wickham (@hadley)](http://stackoverflow.com/questions/8379570/get-functions-title-from-documentation) for teaching me how to parse helpfiles
 * all those who tried (and succeeded) to by-pass `sandboxR`'s security checks
 * especially Hadley Wickham (@hadley) again for his inspiring comments on twitter
 * my wife and the smartest little guy in the world (@Botond) for their tolerance and support
 * and for a handful flu which got me some "spare" time to implement this
