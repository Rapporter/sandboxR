#' Sandboxed environment
#'
#' This function returns a special environment pre-loaded with bunch of forked functions from \code{base}, \code{stats}, \code{graphics} etc. to act as a jail for later evaluation.
#'
#' Some of the forked functions \code{stop}s by default (to prevent using those inside the environment), for a full list see \code{\link{commands.blacklist}}. Other functions (found in package's namespace) behaves differently then usual: some parameters are forbidden (like \code{eval}'s \code{env}) to prevent breaking out from the sandbox, some parameters and returned values are checked for "malicious" signs.
#' @param blacklist character vector of function names which should be banned
#' @return environment
#' @export
sandbox.env <- function(blacklist = as.character(unlist(commands.blacklist()))) {

    ## TODO: check if sandboxed env was created before and return that instead of regenerating

    ## prepare a custom environment with dummy functions
    sandboxed.env <- new.env()
    for (cmd in blacklist) {

        ## disarm forbidden function
        cmd.update <- sprintf("%s <- function(x) stop('Forbidden function called: %s')", cmd, cmd)

        base::eval(base::parse(text = cmd.update), envir = sandboxed.env)

    }

    return(sandboxed.env)

}


#' Checking expressions for malicious code
#'
#' This function tests a character vector of R commands agains a list of banned functions and \code{stop}s if any found.
#' @param src character vector of R commands
#' @param blacklist character vector of function names which should be banned
#' @return invisibly \code{TRUE} if tests passed
#' @export
sandbox.pretest <- function(src, blacklist = as.character(unlist(commands.blacklist()))) {

    ## dummy checks
    if (missing(src))
        stop('Nothing provided to check.')

    ## parse elements of src
    src.r <- suppressWarnings(tryCatch(parse(text = src), error = function(e) e))

    ## syntax error
    if (inherits(src.r, 'error'))
        stop(paste0('Parsing command (`', src, '`) failed, possible syntax error: `', paste(src.r$message, collapse = ';') ,'`'))

    ## get details on the parts of src
    p       <- getParseData(src.r)
    calls   <- sort(unique(p$text[which(p$token == 'SYMBOL_FUNCTION_CALL')]))
    strings <- sort(unique(p$text[which(p$token == 'STR_CONST')]))
    vars    <- sort(unique(p$text[which(p$token == 'SYMBOL')]))
    pkgs    <- sort(unique(p$text[which(p$token == 'SYMBOL_PACKAGE')]))

    ## filtering foreign calls
    if (length(pkgs) > 0)
        stop(sprintf('Tried to call at least one function outside of active namespace from package%s: %s', ifelse(length(pkgs) == 1, '', 's'), paste0(pkgs, collapse = ', ')))

    ## filtering forbidden function calls: e.g. get()
    calls.forbidden <- calls %in% blacklist
    if (any(calls.forbidden))
        stop(sprintf('Forbidden function%s called: %s.', ifelse(sum(calls.forbidden) == 1, '', 's'), paste0(calls[which(calls.forbidden)], collapse = ', ')))

    ## filtering forbidden functions used as symbol: e.g. lappy(foo, get)
    calls.forbidden <- vars %in% blacklist
    if (any(calls.forbidden))
        stop(sprintf('Forbidden function%s used as symbol: %s.', ifelse(length(calls.forbidden) == 1, '', 's'), paste0(vars[which(calls.forbidden)], collapse = ', ')))

    return(invisible(TRUE))

}


#' Eval in sandbox
#' @param src character vector of R commands
#' @param envir the environment where the calls would be tested. This should be omitted or preset with \code{\link{sandbox.env}}.
#' @param time.limit limit on the elapsed time while running \code{src}
#' @examples \dontrun{
#' sandbox('paste(rev(c(")", "whatever", "(", "m", "e", "t", "s", "y", "s")), sep = "", collapse = "")')
#' sandbox('get(paste("","y", "tem", sep="s"))("whoami")')
#' sandbox(c("x1 <- 's'", "x2 <- 'y'", "x3 <- 't'", "x4 <- 'e'", "x5 <- 'm'", "x <- paste(x1, x2, x1, x3, x4, x5, sep = '')", "lm(sprintf(\"%s('echo hello > /tmp/xxx') ~ 1\", x))"))
#' sandbox('paste("as.numeric(system(\'ls -la | wc -l\', intern=T)) ~ 1")')
#' sandbox(c("x <- system", "x('ls')"))
#' sandbox('lm("as.numeric(system(\'ls -la | wc -l\', intern=T)) ~ 1")')
#' }
#' @export
sandbox <- function(src, envir, time.limit = 10) {

    if (missing(envir))
        envir <- sandbox.env()

    ## saving global options
    opts.bak <- options()

    ## pre-checking source for malicious code
    sandbox.pretest(src)

    ## check elapsed time
    setTimeLimit(elapsed = time.limit)

    ## evaluate
    res <- tryCatch(base::eval(base::parse(text = src), envir = envir), error = function(e) e)

    ## setting back global options and removing time limit
    options(opts.bak)
    setTimeLimit(elapsed = Inf)

    ## return
    if (any(class(res) == 'error'))
        stop(res[[1]])
    return(res)

}
