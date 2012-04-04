**This file is heavily under development (just like other parts of the package)!**

## Why do not you allow `foo` function from package `bar`?

**In short**: as I do not need it :)

**Longer answer**: I do think that users should ever touch filesystem on a shared, web-based environment as those functions should be addressed by the hosting application. E.g. if a user would create a plot in a script then that should be saved to an image on the disk automatically without calling `png` or other R functions in the user-driven R session. Similarly data upload should be done in the web application, not in the user-driven R session. Also users should not use complex functions and package-like environments with S4 classes (not that I hate S4 I promise!), as those should be compiled to a package and submitted to CRAN. After that the package could be whitelisted :) 

**Workaround**: the package uses a static list of blacklisted functions, but feel free to use your own list in your custom environment.

## Filtering resource-hungry calls

There are some function calls which are "malicious", but are not addressed by *sandboxR*. These are usually tries to take a lot of resources for nothing, so not modifying files, but "just" wasting server resources.

A great example of @Jeroen (http://stackoverflow.com/a/9145930/564164):

```
library(multicore);
forkbomb <- function(){
  repeat{
    parallel(forkbomb());
  }
}
forkbomb();
```

~~These problems would not ever be addressed by *sandboxR* as e.g. Apparmor can do a handy job here with ease.~~ Besides the fact that Apparmor/SELinux etc. can address this problem easily, `sandbox` has a timeout option and would stop executing user defined R calls after given period of time (default set to 10 seconds).

