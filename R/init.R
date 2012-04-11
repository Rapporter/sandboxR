.onLoad <- function(libname, pkgname)
{
    
    message(sprintf("\nTo use all power of sandboxR and to really secure your environment, please run the following commands:\n\n\t * assignInNamespace('formula.character', sandboxR:::formula.character.masked, ns='stats')\n\nWARNING: These calls would update some _base_ functions and might have some unwanted side-effects!", system.file('startup.R', package = 'sandboxR')))
    
}
