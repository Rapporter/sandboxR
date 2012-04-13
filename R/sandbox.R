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


#' Blacklisted functions
#' @param pkg package name(s) where to look for blacklisted functions. All packages' functions will be returned in a list if not set. 
#' @return vector or list of function names
#' @note Only base is added ATM.
#' @examples \dontrun{
#' commands.blacklist()
#' commands.blacklist('base')
#' }
#' @export
commands.blacklist <- function(pkg) {
    
    ## blacklisted packages which would never be added:
    ##  * from base packages: compiler, parralel, splines, stats4, rcltk, tools
    ##  * foreign, knitr, base64, brew etc.: file operations
    ##  * xtable: no need for HTML/TeX output in templates
    ##  * rook, rredis etc. as no network connection should be allowed 
    ##  * devtools, roxygen(2): no dev package needed
    ##  * RJSONIO, opencpu.encode etc.
    ##  * microbenchmark, logging, profiling etc.
    
    ## TODO:
    ##  * look over if really all functions are to be blacklisted
    ##  * free up: mget
    
    blacklist <- list(
        base        = c('.__H__.cbind', '.__H__.rbind', '.amatch_bounds', '.amatch_costs', '.C', '.cache_class', '.Call', '.Call.graphics', '.colMeans', '.colSums', '.decode_numeric_version', '.Defunct', '.deparseOpts', '.Deprecated', '.difftime', '.doTrace', '.dynLibs', '.encode_numeric_version', '.expand_R_libs_env_var', '.Export', '.External', '.External.graphics', '.find.package', '.First.sys', '.Fortran', '.getRequiredPackages', '.getRequiredPackages2', '.gt', '.gtn', '.handleSimpleError', '.Import', '.ImportFrom', '.Internal', '.isMethodsDispatchOn', '.isOpen', '.kronecker', '.libPaths', '.make_numeric_version', '.makeMessage', '.mergeExportMethods', '.mergeImportMethods', '.NotYetImplemented', '.NotYetUsed', '.OptRequireMethods', '.packages', '.packageStartupMessage', '.path.package', '.POSIXct', '.POSIXlt', '.Primitive', '.primTrace', '.primUntrace', '.readRDS', '.row_names_info', '.rowMeans', '.rowSums', '.S3method', '.saveRDS', '.Script', '.set_row_names', '.signalSimpleWarning', '.standard_regexps', '.subset', '.subset2', '.TAOCP1997init', 'as.call', 'asNamespace', 'asS3', 'asS4', 'attach', 'attachNamespace', 'autoload', 'autoloader', 'baseenv', 'bindingIsActive', 'bindingIsLocked', 'bindtextdomain', 'body', 'browser', 'browserCondition', 'browserSetDebug', 'browserText', 'builtins', 'bzfile', 'call', 'cat', 'chartr', 'close', 'close.connection', 'close.srcfile', 'close.srcfilealias', 'closeAllConnections', 'Cstack_info', 'debug', 'debugonce', 'deparse', 'detach', 'dget', 'dir', 'dir.create', 'do.call', 'dput', 'dump', 'dyn.load', 'dyn.unload', 'enc2native', 'enc2utf8', 'encodeString', 'Encoding', 'env.profile', 'environment', 'environmentIsLocked', 'environmentName', 'eval.parent', 'evalq', 'exists', 'fifo', 'file', 'file.access', 'file.append', 'file.choose', 'file.copy', 'file.create', 'file.exists', 'file.info', 'file.link', 'file.path', 'file.remove', 'file.rename', 'file.show', 'file.symlink', 'find.package', 'findPackageEnv', 'flush', 'flush.connection', 'force', 'formals', 'gc', 'gc.time', 'gcinfo', 'gctorture', 'gctorture2', 'getAllConnections', 'getCallingDLL', 'getCallingDLLe', 'getCConverterDescriptions', 'getCConverterStatus', 'getConnection', 'getDLLRegisteredRoutines', 'getDLLRegisteredRoutines.character', 'getDLLRegisteredRoutines.DLLInfo', 'getElement', 'geterrmessage', 'getExportedValue', 'getHook', 'getLoadedDLLs', 'getNamespace', 'getNamespaceExports', 'getNamespaceImports', 'getNamespaceInfo', 'getNamespaceName', 'getNamespaceUsers', 'getNamespaceVersion', 'getNativeSymbolInfo', 'getNumCConverters', 'getRversion', 'getSrcLines', 'getTaskCallbackNames', 'gettext', 'gettextf', 'getwd', 'globalenv', 'gzcon', 'gzfile', 'iconv', 'iconvlist', 'importIntoEnv', 'interactive', 'intToUtf8', 'invokeRestart', 'invokeRestartInteractively', 'is.call', 'is.loaded', 'isBaseNamespace', 'isdebugged', 'isIncomplete', 'isNamespace', 'isSeekable', 'l10n_info', 'lazyLoad', 'lazyLoadDBexec', 'lazyLoadDBfetch', 'library.dynam', 'library.dynam.unload', 'licence', 'license', 'list.dirs', 'list2env', 'load', 'loadedNamespaces', 'loadingNamespaceInfo', 'loadNamespace', 'lockBinding', 'lockEnvironment', 'local', 'makeActiveBinding', 'manglePackageName', 'mem.limits', 'memCompress', 'memDecompress', 'memory.profile', 'mget', 'namespaceExport', 'namespaceImport', 'namespaceImportClasses', 'namespaceImportFrom', 'namespaceImportMethods', 'new.env', 'NextMethod', 'ngettext', 'on.exit', 'open', 'open.connection', 'open.srcfile', 'open.srcfilealias', 'open.srcfilecopy', 'package_version', 'packageEvent', 'packageHasNamespace', 'packageStartupMessage', 'parent.env', 'parent.frame', 'parse', 'parseNamespaceFile', 'path.expand', 'path.package', 'pipe', 'pos.to.env', 'pushBack', 'pushBackLength', 'q', 'quit', 'R_system_version', 'R.home', 'R.Version', 'rawConnection', 'rawConnectionValue', 'read.dcf', 'readBin', 'readChar', 'readline', 'readLines', 'readRDS', 'readRenviron', 'Recall', 'registerS3method', 'registerS3methods', 'remove', 'removeCConverter', 'removeTaskCallback', 'requireNamespace', 'restartDescription', 'restartFormals', 'retracemem', 'rm', 'RNGkind', 'RNGversion', 'save', 'save.image', 'saveRDS', 'scan', 'search', 'searchpaths', 'seek', 'serialize', 'setHook', 'setNamespaceInfo', 'setSessionTimeLimit', 'setTimeLimit', 'setwd', 'showConnections', 'sink', 'sink.number', 'socketConnection', 'socketSelect', 'source', 'srcfile', 'srcfilealias', 'srcfilecopy', 'srcref', 'sys.call', 'sys.calls', 'Sys.chmod', 'Sys.Date', 'sys.frame', 'sys.frames', 'sys.function', 'Sys.getenv', 'Sys.getlocale', 'Sys.getpid', 'Sys.glob', 'Sys.info', 'sys.load.image', 'Sys.localeconv', 'sys.nframe', 'sys.on.exit', 'sys.parent', 'sys.parents', 'Sys.readlink', 'sys.save.image', 'Sys.setenv', 'Sys.setFileTime', 'Sys.setlocale', 'Sys.sleep', 'sys.source', 'sys.status', 'Sys.time', 'Sys.timezone', 'Sys.umask', 'Sys.unsetenv', 'Sys.which', 'system', 'system.file', 'system.time', 'system2', 'taskCallbackManager', 'tempdir', 'tempfile', 'testPlatformEquivalence', 'textConnection', 'textConnectionValue', 'topenv', 'trace', 'traceback', 'tracemem', 'truncate', 'truncate.connection', 'undebug', 'unloadNamespace', 'unlockBinding', 'unserialize', 'untrace', 'untracemem', 'unz', 'url', 'UseMethod', 'utf8ToInt', 'warnings', 'withCallingHandlers', 'write', 'write.dcf', 'writeBin', 'writeChar', 'writeLines', 'xzfile', 'unlink', 'list.files', 'charToRaw', 'rawToChar', 'rawShift', 'rawToBits', 'intToBits', 'packBits'),
        utils       = c('alarm', 'apropos', 'argsAnywhere', 'aspell', 'aspell_package_Rd_files', 'aspell_package_vignettes', 'aspell_write_personal_dictionary_file', 'assignInMyNamespace', 'assignInNamespace', 'available.packages', 'browseEnv', 'browseURL', 'browseVignettes', 'bug.report', 'capture.output', 'checkCRAN', 'chooseBioCmirror', 'chooseCRANmirror', 'close.socket', 'compareVersion', 'contrib.url', 'count.fields', 'CRAN.packages', 'create.post', 'dataentry', 'data.entry', 'de', 'debugger', 'demo', 'de.ncols', 'de.restore', 'de.setup', '.DollarNames', 'download.file', 'download.packages', 'dump.frames', 'edit', 'emacs', 'example', 'file.edit', 'file_test', 'find', 'findLineNum', 'fix', 'fixInNamespace', 'flush.console', 'getAnywhere', 'getCRANmirrors', 'getFromNamespace', 'getS3method', 'getSrcDirectory', 'getSrcFilename', 'getSrcLocation', 'getSrcref', 'getTxtProgressBar', 'help', 'help.request', 'help.search', 'help.start', 'history', 'installed.packages', 'install.packages', 'loadhistory', 'localeToCharset', 'lsf.str', 'ls.str', 'maintainer', 'make.packages.html', 'makeRweaveLatexCodeRunner', 'make.socket', 'memory.limit', 'memory.size', 'menu', 'methods', 'mirror2html', 'new.packages', 'news', 'nsl', 'object.size', 'old.packages', 'package.contents', 'packageDescription', 'package.skeleton', 'packageStatus', 'packageVersion', 'page', 'pico', 'prompt', 'promptData', 'promptPackage', 'rc.getOption', 'rc.options', 'rc.settings', 'rc.status', 'readCitationFile', 'read.csv', 'read.csv2', 'read.delim', 'read.delim2', 'read.DIF', 'read.fortran', 'read.fwf', 'read.socket', 'read.table', 'recover', 'remove.packages', 'removeSource', 'Rprof', 'Rprofmem', 'RShowDoc', 'RSiteSearch', 'rtags', 'Rtangle', 'RtangleSetup', 'RtangleWritedoc', 'RweaveChunkPrefix', 'RweaveEvalWithOpt', 'RweaveLatex', 'RweaveLatexFinish', 'RweaveLatexOptions', 'RweaveLatexSetup', 'RweaveLatexWritedoc', 'RweaveTryStop', 'savehistory', 'sessionInfo', 'setBreakpoint', 'setRepositories', 'setTxtProgressBar', 'Stangle', 'summaryRprof', 'Sweave', 'SweaveHooks', 'SweaveSyntConv', 'tar', 'timestamp', 'toBibtex', 'toLatex', 'txtProgressBar', 'untar', 'unzip', 'update.packages', 'update.packageStatus', 'upgrade', 'url.show', 'vi', 'View', 'vignette', 'write.csv', 'write.csv2', 'write.socket', 'write.table', 'wsbrowser', 'xedit', 'xemacs', 'zip', 'zip.file.extract'),
        stats       = c('write.ftable', 'read.ftable'),
        graphics    = NULL,
        grDevices   = c('bitmap', 'bmp', 'cairo_pdf', 'cairo_ps', 'CIDFont', 'dev2bitmap', 'devAskNewPage', 'dev.capabilities', 'dev.capture', 'dev.control', 'dev.copy', 'dev.copy2eps', 'dev.copy2pdf', 'dev.cur', 'dev.flush', 'dev.hold', 'deviceIsInteractive', 'dev.interactive', 'dev.list', 'dev.new', 'dev.next', 'dev.off', 'dev.prev', 'dev.print', 'dev.set', 'dev.size', 'embedFonts', 'getGraphicsEvent', 'getGraphicsEventEnv', 'graphics.off', 'jpeg', 'pdf', 'pdfFonts', 'pdf.options', 'pictex', 'png', 'postscript', 'postscriptFont', 'postscriptFonts', 'ps.options', 'quartz', 'quartzFont', 'quartzFonts', 'quartz.options', 'recordGraphics', 'recordPlot', 'replayPlot', 'savePlot', 'setEPS', 'setGraphicsEventEnv', 'setGraphicsEventHandlers', 'setPS', 'svg', 'tiff', 'Type1Font', 'x11', 'X11', 'X11Font', 'X11Fonts', 'X11.options', 'xfig'),
        methods     = c('addNextMethod', 'allGenerics', 'allNames', 'asMethodDefinition', 'assignClassDef', 'assignMethodsMetaData', 'balanceMethodsList', 'cacheGenericsMetaData', 'cacheMetaData', 'cacheMethod', 'callGeneric', 'callNextMethod', 'canCoerce', 'checkSlotAssignment', '.classEnv', 'classesToAM', 'classLabel', 'classMetaName', 'className', 'completeClassDefinition', 'completeExtends', 'completeSubclasses', 'conformMethod', 'defaultDumpName', 'defaultPrototype', 'doPrimitiveMethod', '.doTracePrint', 'dumpMethod', 'dumpMethods', 'el', 'elNamed', 'empty.dump', 'emptyMethodsList', 'evalOnLoad', 'evalqOnLoad', 'evalSource', 'existsFunction', 'existsMethod', 'finalDefaultMethod', 'findClass', 'findFunction', 'findMethod', 'findMethods', 'findMethodSignatures', 'findUnique', 'fixPre1.8', 'formalArgs', 'functionBody', 'generic.skeleton', 'getAccess', 'getAllMethods', 'getAllSuperClasses', 'getClass', 'getClassDef', 'getClasses', 'getClassName', 'getClassPackage', 'getDataPart', 'getExtends', 'getFunction', 'getGeneric', 'getGenerics', 'getGroup', 'getGroupMembers', 'getLoadActions', 'getMethod', 'getMethods', 'getMethodsForDispatch', 'getMethodsMetaData', 'getPackageName', 'getProperties', 'getPrototype', 'getRefClass', 'getSlots', 'getSubclasses', 'getValidity', 'getVirtual', 'hasArg', 'hasLoadAction', 'hasMethod', 'hasMethods', '.hasSlot', 'implicitGeneric', 'inheritedSlotNames', 'initFieldArgs', 'initialize', 'initRefFields', 'insertMethod', 'insertSource', 'isClass', 'isClassDef', 'isClassUnion', 'isGeneric', 'isGrammarSymbol', 'isGroup', 'isSealedClass', 'isSealedMethod', 'isVirtualClass', 'isXS3Class', 'languageEl', '.Last.lib', 'linearizeMlist', 'listFromMethods', 'listFromMlist', 'loadMethod', 'Logic', 'makeClassRepresentation', 'makeExtends', 'makeGeneric', 'makeMethodsList', 'makePrototypeFromClassDef', 'makeStandardGeneric', 'matchSignature', 'mergeMethods', 'metaNameUndo', 'MethodAddCoerce', 'methodSignatureMatrix', 'method.skeleton', 'MethodsList', 'MethodsListSelect', 'methodsPackageMetaName', 'missingArg', 'mlistMetaName', 'multipleClasses', 'new', 'newBasic', 'newClassRepresentation', 'newEmptyObject', 'Ops', 'packageSlot', 'possibleExtends', 'prohibitGeneric', 'promptClass', 'promptMethods', 'prototype', 'Quote', 'reconcilePropertiesAndPrototype', 'registerImplicitGenerics', 'rematchDefinition', 'removeClass', 'removeGeneric', 'removeMethod', 'removeMethods', 'removeMethodsObject', 'representation', 'requireMethods', 'resetClass', 'resetGeneric', 'S3Class', 'S3Part', 'sealClass', 'seemsS4Object', 'selectMethod', 'selectSuperClasses', '.selectSuperClasses', 'sessionData', 'setAs', 'setClass', 'setClassUnion', 'setDataPart', 'setGeneric', 'setGenericImplicit', 'setGroupGeneric', 'setIs', 'setLoadAction', 'setLoadActions', 'setMethod', 'setOldClass', 'setPackageName', 'setPrimitiveMethods', 'setRefClass', 'setReplaceMethod', 'setValidity', 'showClass', 'showDefault', 'showExtends', 'showMethods', 'showMlist', 'signature', 'SignatureMethod', 'sigToEnv', 'slot', 'slotNames', '.slotNames', 'slotsFromS3', 'substituteDirect', 'substituteFunctionArgs', 'superClassDepth', 'testInheritedMethods', 'testVirtual', 'traceOff', 'traceOn', '.TraceWithMethods', 'tryNew', 'trySilent', 'unRematchDefinition', '.untracedFunction', 'validObject', 'validSlotNames', '.valueClassTest'),
        datasets    = NULL
        #grid        = c()
    )
    
    if (missing(pkg))
        pkg <- names(blacklist)
    
    if (!all(pkg %in% names(blacklist)))
        stop('Unknown package specified!')
    
    return(blacklist[pkg])
    
}


#' Sandboxed environment
#' 
#' This function returns a special environment pre-loaded with bunch of forked functions from \code{base}, \code{stats}, \code{graphics} etc. to act as a jail for later evaluation.
#' 
#' Some of the forked functions \code{stop}s by default (to prevent using those inside the environment), for a full list see \code{\link{commands.blacklist}}. Other functions (found in package's namespace) behaves differently then usual: some parameters are forbidden (like \code{eval}'s \code{env}) to prevent breaking out from the sandbox, some parameters and returned values are checked for "malicious" signs.
#' @param blacklist character vector of function names which should be banned 
#' @return environment 
#' @export
sandbox.env <- function(blacklist = as.character(unlist(commands.blacklist()))) {

    ## prepare a custom environment with dummy functions
    sandboxed.env <- new.env()
    for (cmd in blacklist)
        base::eval(base::parse(text = sprintf("%s <- function(x) stop('You have successfully by-passed my regexps but after all calling a still forbidden function: %s')", cmd, cmd)), envir = sandboxed.env)

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
    f <- textConnection(src)
    src.r <- suppressWarnings(tryCatch(parser(f), error = function(e) NULL))
    close(f)
    if (is.null(nrow(attr(src.r, 'data'))))
        stop('Syntax error.')
    p       <- attr(src.r, 'data')
    calls   <- sort(unique(p$text[which(p$token.desc == 'SYMBOL_FUNCTION_CALL')]))
    strings <- sort(unique(p$text[which(p$token.desc == 'STR_CONST')]))
    vars    <- sort(unique(p$text[which(p$token.desc == 'SYMBOL')]))
    pkgs    <- sort(unique(p$text[which(p$token.desc == 'SYMBOL_PACKAGE')]))

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
