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

    sandbox.pretest(x)

    mc[[1]] <- quote(base::get)
    mc$pos <- parent.frame()
    base::eval(mc)

}


assign <- function(x, value, ...) {

    mc <- match.call()

    if (!is.null(mc$envir) | !is.null(mc$pos))
        stop('Tried to leave sandboxed environment.')

    sandbox.pretest(deparse(substitute(value)))

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
    sandbox.pretest(x)
    ff <- formula(base::eval(base::parse(text = x)[[1L]]))
    environment(ff) <- env
    ff
}

latticeParseFormula <- lattice:::latticeParseFormula
body(latticeParseFormula) <- as.call(c(as.symbol("{"), c(substitute(if (inherits(groups, "formula")) sandbox.pretest(as.character(groups)[2])), as.list(body(latticeParseFormula))[-1])))


rapport <- function(...) {

    mc <- match.call(rapport::rapport)

    if (!is.null(mc$reproducible) | !is.null(mc$env) | !is.null(mc$header.levels.offset) | !is.null(mc$rapport.mode) | !is.null(mc$graph.output) | !is.null(mc$file.name) | !is.null(mc$file.path) | !is.null(mc$graph.replay) | !is.null(mc$graph.hi.res))
        stop('Forbidden parameters provided!')

    res      <- do.call(rapport::rapport, list(...))
    return(invisible(res))

}


options <- function(...) {

    l <- list(...)

    disabled.options <- base::getOption('sandboxR.disabled.options')

    if (length(l) == 0) {

        o <- base::options()
        return(o[setdiff(names(o), disabled.options)])

    }

    if (length(names(l)) == 0) {

        l <- unlist(l)
        if (any(l %in% disabled.options))
            stop('Not available option(s) queried.')
        return(base::getOption(l))

    }

    if (any(names(l) %in% disabled.options))
        stop('Not available option(s) queried.')

    mc <- match.call(base::options)
    mc[[1]] <- quote(base::options)
    mc[[2]] <- sandbox(deparse(mc[[2]]), parent.frame())
    res <- base::eval(mc, parent.frame())

    return(invisible(res))

}


getOption <- function(x, default = NULL) {

    if (x %in% base::getOption('sandboxR.disabled.options'))
        stop('Not available option(s) queried.')

    return(base::getOption(x, default))

}


evalsOptions <- function(o, value) {
    if (!o %in% c('graph.unify', 'width', 'height'))
        stop('Forbidden parameter queried. You can set: `graph.unify`, `width`, `height` only.')
    if (missing(value))
        pander::evalsOptions(o)
    else
        pander::evalsOptions(o, value)
}


panderOptions <- function(o, value) {
    if (o %in% c('table.style'))
        stop('Forbidden parameter queried.')
    if (missing(value))
        pander::panderOptions(o)
    else
        pander::panderOptions(o, value)
}


Pandoc.brew <- function(...) {

    mc <- match.call(pander::Pandoc.brew)
    if (!is.null(mc$file) | !is.null(mc$envir))
        stop('Forbidden arguments passed.')
    mc[[1]] <- quote(pander::Pandoc.brew)
    base::eval(mc, parent.frame())

}
