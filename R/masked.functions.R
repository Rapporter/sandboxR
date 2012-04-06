#' Masked paste
#' 
#' Checks for forbidden function calls in constructed character vector before returning result. 
#' @param ... see \code{paste}
#' @param sep see \code{paste} 
#' @param collapse  see \code{paste}
#' @seealso commands.blacklist
#' @examples \dontrun{
#' paste.masked('sys', 'tem', '(', sep = '')
#' paste.masked('xsx sax s system( dasf asf as url(')
#' paste.masked(c(letters[c(19, 25, 19, 20, 5, 13)], '('), collapse = "")
#' }
paste.masked  <- function(..., sep = '', collapse = NULL) {
    
    res <- base::paste(..., sep = sep, collapse = collapse)
    
    blacklist <- as.character(unlist(commands.blacklist()))
    blacklist.found <- sapply(sprintf('%s[ \t]*\\(', blacklist), grepl, res)
    blacklist.found <- which(blacklist.found == TRUE)
    
    if (length(blacklist.found) > 0)
        stop(sprintf('Forbidden function%s name build: %s.', ifelse(length(blacklist.found) == 1, '\'s', 's\''), paste0(blacklist[blacklist.found], collapse = ', ')))
    
    return(res)
    
}


#' Masked paste0
#' @param ... see \code{paste0}
#' @param collapse  see \code{paste0}
#' @examples \dontrun{
#' paste0.masked('sys', 'tem', '(')
#' }
paste0.masked  <- function(..., collapse = NULL) {
    
    sandboxR::paste.masked(..., sep = '', collapse = collapse)
    
}


#' Masked sprintf
#' @param fmt see \code{sprintf}
#' @param ... see \code{sprintf}
sprintf.masked  <- function(fmt, ...) {
    
    res <- base::sprintf(fmt, ...)
    
    blacklist <- as.character(unlist(commands.blacklist()))
    blacklist.found <- sapply(sprintf('%s[ \t]*\\(', blacklist), grepl, res)
    blacklist.found <- which(blacklist.found == TRUE)
    
    if (length(blacklist.found) > 0)
        stop(sprintf('Forbidden function%s name build: %s.', ifelse(length(blacklist.found) == 1, '\'s', 's\''), paste0(blacklist[blacklist.found], collapse = ', ')))
    
    return(res)
    
}


#' Masked model.frame
#' @param formula see \code{model.frame}
#' @param ... see \code{model.frame}
model.frame.masked <- function(formula, ...) {
    
    if (is.character(formula))
        sandbox(formula)
    stats::model.frame(formula, ...)
    
}


#' Masked formula
#' @param x see \code{formula}
#' @param ... see \code{formula}
formula.masked <- function(x, ...) {
    
    if (is.character(x))
        sandbox(x)
    stats::formula(x, ...)

}


#' Masked as.formula
#' @param object see \code{as.formula}
#' @param env see \code{as.formula}
as.formula.masked <- function(object, env = parent.frame()) {
    
    if (is.character(object))
        sandbox(object)
    stats::as.formula(object, env)
    
}


#' Masked eval
#' @param expr see \code{eval}
#' @param ... see \code{eval}
eval.masked <- evalq.masked <- local.masked <- function(expr, envir, enclos) {
    if (!missing(envir) | !missing(enclos))
        stop('Tried to leave sandboxed environment.')
    mc <- match.call()
    sandbox(deparse(substitute(expr)))
    mc[[1]] <- as.name(sub('\\.masked$', '', mc[[1]]))
    eval(mc)
}


#' Masked get
#' @param x see \code{get}
#' @param envir see \code{get}
#' @param ... see \code{get}
get.masked <- function(x, envir, ...) {
    if (!missing(envir))
        stop('Tried to leave sandboxed environment.')
    if (x %in% as.character(unlist(commands.blacklist())))
        stop(sprintf('Tried to get a forbidden function: %s.', x))
    mc <- match.call()
    mc[[1]] <- quote(get)
    eval(mc)
}
