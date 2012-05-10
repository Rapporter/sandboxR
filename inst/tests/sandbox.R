context('normal behavior (no filtering should happen)')

test_that('called functions', {
    expect_output(sandbox("as.formula('1~1')"), '.*')
    expect_output(sandbox("lm(mtcars)"), '.*')
    expect_output(sandbox("(get)('mtcars')"), '.*')
    expect_output(sandbox("(`get`)('mtcars')"), '.*')
    expect_output(sandbox("x <- (get)"), '.*')
    expect_output(sandbox("a <- 2; a"), '2')
    expect_output(sandbox(c('x <- eval', 'x(runif(10))')), '.*')
    expect_output(sandbox(c('x <- runif', 'x(10)')), '.*')
})


context('filtering blacklisted functions')

test_that('called functions', {
    expect_error(sandbox('system("cat /etc/passwd")'))
    expect_error(sandbox('get(paste("","y", "tem", sep="s"))("whoami")'))
    expect_error(sandbox(c('f<-function(x) sin(x)', 'f(10)', 'body(f)[[1]] <- quote(readLines)', 'print(f("/sandbox/hello"))')))
    expect_error(sandbox('rawToChar(as.raw(c(115, 121, 115, 116, 101, 109, 40)))'))
})

test_that('paste/sprintf created functions', {
    expect_error(sandbox(c("x1 <- 's'", "x2 <- 'y'", "x3 <- 't'", "x4 <- 'e'", "x5 <- 'm'", "x <- paste(x1, x2, x1, x3, x4, x5, sep = '')", "lm(sprintf(\"%s('echo hello > /tmp/xxx') ~ 1\", x))")))
    expect_output(sandbox('paste("as.numeric(system(\'ls -la | wc -l\', intern=T)) ~ 1")'), '.*')
})

test_that('forked functions', {
    expect_error(sandbox(c('x <- `eval.parent`', 'x(runif(10))')))
    expect_error(sandbox("x <- (system)"))
    expect_error(sandbox(c("x <- system", "x('ls -la')")))
})

test_that('unexposed functions', {
    expect_error(sandbox("`system`('cat /etc/passwd')"))
    expect_error(sandbox("x <- (system)"))
})

test_that('quoted functions', {
    expect_error(sandbox(c('x <- "get"("eval")', 'y <- "get"("parse")', 'x(y(text = \'mean(1:10)\'))')))
})

test_that('functions as symbols', {
    expect_error(sandbox('lapply("/etc/passwd", readLines)'))
    expect_error(sandbox('lapply("whoami", system, intern = TRUE)'))
})

context('defusing forkbomb')

test_that('check elapsed time', {
    expect_error(sandbox("while(TRUE) mean(1:10)", time.limit = 1))
})

context('masked functions')

test_that('eval', {
    expect_error(sandbox("eval(mtcars, envir = .GlobalEnv)"))
    expect_output(sandbox("eval(mtcars)"), '.*')
    expect_output(sandbox(c("x <- paste(\"as.numeric(system('ls -la | wc -l', intern=T)) ~ 1\")", "eval(x)")), '.*')
})

test_that('assign', {
    expect_error(sandbox("assign('a', system)"))
    expect_error(sandbox("assign('a', base::system)"))
    expect_output(sandbox("assign('a', get); a"), '.*')
})

test_that('get', {
    expect_output(sandbox("get('mtcars')"), '.*')
    expect_output(sandbox("x<-1; get('x')"), '1')
    expect_output(sandbox(c("x<-1", "get('x')")), '1')
    expect_error(sandbox("get('system')"))
    expect_error(sandbox("get('base::system')"))
    expect_error(sandbox("get(paste('', 'y', 'tem', sep = 's'))('whoami')"))
})

test_that('ls', {
    expect_output(sandbox('ls()'), '.*')
    expect_error(sandbox('ls(pos = 11)'))
    expect_output(sandbox('x<-1;ls()'), '.*')
    expect_output(sandbox('x<-runif;y<-1:20;ls()'), '.*')
})

test_that('library/require', {
    expect_output(sandbox('library()'), '.*')
    expect_output(sandbox('library(base)'), '.*')
    expect_output(sandbox('library(base, verbose = TRUE)'), '.*')
    expect_output(sandbox('library("base")'), '.*')
    expect_error(sandbox('library(RCurl)'))
    expect_error(sandbox('library("RCurl")'))
    expect_output(sandbox('require(stats)'), '.*')
    expect_error(sandbox('require(RCurl)'))
})

context('modified internals')

test_that('lm hacks', {
    expect_error(sandbox('lm("as.numeric(system(\'ls -la | wc -l\', intern=T)) ~ 1")'))
    expect_error(sandbox("lm(read.table('/etc/passwd'))"))
    expect_error(sandbox("eval(parse(text = \"lm(read.table('/etc/passwd'))\"))"))
    expect_error(sandbox("out <- paste(\"1 ~ system\", \" x\");out <- gsub(\"x\", \"('echo 1')\", out);lm(out)"))
    expect_error(sandbox("out <- paste(\"1 ~ print(system\", \" x)\");out <- gsub(\"x\", \"('echo HA!')\", out);lm(out)"))
    expect_error(sandbox("out <- paste(\"1 ~ print(read.table\", \" x)\");out <- gsub(\"x\", \"('/etc/passwd')\", out);lm(out)"))
    expect_error(sandbox("out <- paste(\"1 ~ system\", \" x\");out <- gsub(\"x\", \"('echo 1')\", out);glm(out)"))
    expect_error(sandbox("out <- paste(\"1 ~ system\", \" x\");out <- gsub(\"x\", \"('echo 1')\", out);plot(as.formula(out))"))
    expect_error(sandbox("out <- paste(\"1 ~ print(system\", \" x)\");out <- gsub(\"x\", \"('echo 1')\", out);t.test(formula = as.formula(out))"))
    expect_error(sandbox(c('out <- paste("1 ~ print(read.table", " x)");', "out <- gsub(\"x\", \"('/etc/passwd')\", out);", "lm(out)")))
    expect_output(sandbox("lm(mtcars)"), '.*')
})


test_that('latticeParseFormula', {
    expect_error(sandbox(c('x <- c(\'1\', \'readLines("/etc/passwd")\'', "class(x) <- 'formula'", "latticeParseFormula(data=mtcars, model=hp~wt, groups=x)")))
})
