eval <- function(expr, envir, enclos) {
    
    if (!missing(envir) | !missing(enclos))
        stop('Tried to leave sandboxed environment.')
    
    e <- parent.frame()
    sandbox(deparse(substitute(expr)), e)
        
}


get <- function(x, pos, envir, ...) {

    mc <- match.call()
    
    if (!is.null(mc$envir)| !is.null(mc$pos))
        stop('Tried to leave sandboxed environment.')
    
    e <- parent.frame()
    sandbox(x, e)
    
    mc[[1]] <- quote(base::get)
    mc$pos <- parent.frame()
    base::eval(mc)
    
}


assign <- function(x, value, ...) {
    
    mc <- match.call()
    
    if (!is.null(mc$envir) | !is.null(mc$pos))
        stop('Tried to leave sandboxed environment.')
    
    e <- parent.frame()
    sandbox(deparse(substitute(value)), e)
    
    mc[[1]] <- quote(base::assign)
    mc$pos <- parent.frame()
    base::eval(mc)
    
}


objects <- ls <- function(...) {
    
    mc <- match.call(base::ls)
    
    if (!is.null(mc$envir) | !is.null(mc$pos) | !is.null(mc$name))
        stop('Tried to leave sandboxed environment.')
    
    mc[[1]] <- quote(base::ls)
    mc$pos <- parent.frame()
    res <- base::eval(mc)
    
    setdiff(res, c(as.character(unlist(commands.blacklist())), sub('\\.masked$', '', base::ls(pattern = ".*\\.masked", envir = getNamespace("sandboxR")))))

}


library <- function(...) {
    
    mc <- match.call(base::library)
    
    if (!is.null(mc$pos) | !is.null(mc$lib.loc))
        stop('Tried to leave sandboxed environment.')
    
    if (!is.null(mc$help))
        stop('Sorry, read docs on localhost.')
    
    if (!is.null(mc$package)) {
        if (!is.character(mc$package))
            mc$package <- deparse(mc$package)
        if (!mc$package %in% names(commands.blacklist()))
            stop('Tried to load a forbidden package.')
    } else {
        return(names(commands.blacklist()))
    }
    
    mc[[1]] <- quote(base::library)
    res <- base::eval(mc)
    
    return(invisible(res))
    
}


require <- function(...) {
    
    mc <- match.call(base::require)
    
    if (!is.null(mc$lib.loc))
        stop('Tried to leave sandboxed environment.')
     if (!is.null(mc$package)) {
        if (!is.character(mc$package))
            mc$package <- deparse(mc$package)
        if (!mc$package %in% names(commands.blacklist()))
            stop('Tried to load a forbidden package.')
    } else {
        return(names(commands.blacklist()))
    }
    
    mc[[1]] <- quote(base::require)
    res <- base::eval(mc)
    
    return(invisible(res))
    
}


formula.character <- function(x, env = parent.frame(), ...)
{
    sandbox(x)
    ff <- formula(base::eval(base::parse(text = x)[[1L]]))
    environment(ff) <- env
    ff
}
