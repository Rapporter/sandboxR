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
#' @export
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
#' @export
paste0.masked  <- function(..., collapse = NULL) {
    
    sandboxR::paste.masked(..., sep = '', collapse = collapse)
    
}


#' Masked sprintf
#' @param fmt see \code{sprintf}
#' @param ... see \code{sprintf}
#' @export
sprintf.masked  <- function(fmt, ...) {
    
    res <- base::sprintf(fmt, ...)
    
    blacklist <- as.character(unlist(commands.blacklist()))
    blacklist.found <- sapply(sprintf('%s[ \t]*\\(', blacklist), grepl, res)
    blacklist.found <- which(blacklist.found == TRUE)
    
    if (length(blacklist.found) > 0)
        stop(sprintf('Forbidden function%s name build: %s.', ifelse(length(blacklist.found) == 1, '\'s', 's\''), paste0(blacklist[blacklist.found], collapse = ', ')))
    
    return(res)
    
}
