#' Masked eval
#' @param expr see \code{eval}
#' @param envir see \code{eval}
#' @param enclos see \code{eval}
eval <- function(expr, envir, enclos) {
    
    if (!missing(envir) | !missing(enclos))
        stop('Tried to leave sandboxed environment.')
    
    mc <- match.call()
    sandbox(deparse(substitute(expr)))
        
}


#' Masked get
#' @param x see \code{get}
#' @param pos see \code{get}
#' @param envir see \code{get}
#' @param ... see \code{get}
get <- function(x, pos, envir, ...) {

    mc <- match.call()
    
    if (!is.null(mc$envir)| !is.null(mc$pos))
        stop('Tried to leave sandboxed environment.')
    
    e <- parent.frame()
    sandbox(mc$x, e)
    
    mc[[1]] <- quote(base::get)
    mc$pos <- parent.frame()
    base::eval(mc)
    
}


#' Masked assign
#' @param x see \code{assign}
#' @param value see \code{assign}
#' @param ... see \code{assign}
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


#' Masked ls
#' @param ... see \code{ls}
#' @aliases ls.masked
objects <- ls <- function(...) {
    
    mc <- match.call(base::ls)
    
    if (!is.null(mc$envir) | !is.null(mc$pos) | !is.null(mc$name))
        stop('Tried to leave sandboxed environment.')
    
    mc[[1]] <- quote(base::ls)
    mc$pos <- parent.frame()
    res <- base::eval(mc)
    
    setdiff(res, c(as.character(unlist(commands.blacklist())), sub('\\.masked$', '', base::ls(pattern = ".*\\.masked", envir = getNamespace("sandboxR")))))

}


#' Masked library
#' @param ... see \code{library}
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


#' Masked require
#' @param ... see \code{require}
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


#' Masked formula.character
#' @param x see \code{formula.character}
#' @param env see \code{formula.character}
#' @param ... see \code{formula.character}
#' @export
formula.character <- function(x, env = parent.frame(), ...)
{
    sandbox(x)
    ff <- formula(base::eval(base::parse(text = x)[[1L]]))
    environment(ff) <- env
    ff
}
