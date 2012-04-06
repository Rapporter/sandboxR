context('filtering blacklisted functions')

test_that('called functions', {
            expect_error(sandbox('system("cat /etc/passwd")'))
            expect_error(sandbox('get(paste("","y", "tem", sep="s"))("whoami")'))
            expect_error(sandbox(c('f<-function(x) sin(x)', 'f(10)', 'body(f)[[1]] <- quote(readLines)', 'print(f("/sandbox/hello"))')))
            expect_error(sandbox('rawToChar(as.raw(c(115, 121, 115, 116, 101, 109, 40)))'))
        })

test_that('paste/sprintf created functions', {
            expect_error(sandbox(c("x1 <- 's'", "x2 <- 'y'", "x3 <- 't'", "x4 <- 'e'", "x5 <- 'm'", "x <- paste(x1, x2, x1, x3, x4, x5, sep = '')", "lm(sprintf(\"%s('echo hello > /tmp/xxx') ~ 1\", x))")))
            expect_error(sandbox('paste("as.numeric(system(\'ls -la | wc -l\', intern=T)) ~ 1")'))
        })

test_that('paste/sprintf created functions', {
            expect_error(sandbox(c("x <- system", "x('ls')")))
        })

test_that('lm', {
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
        })

test_that('forked functions', {
            expect_error(sandbox(c('x <- `eval`', 'x(runif(10))')))
            expect_error(sandbox("x <- (system)"))
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
        })

test_that('check elapsed time', {
            expect_error(sandbox("while(TRUE) mean(1:10)", 1))
        })

test_that('eval checks', {
            expect_error(sandbox("eval(mtcars, envir = .GlobalEnv)"))
        })

test_that('assign checks', {
            expect_error(sandbox("assign('a', system)"))
            expect_error(sandbox("assign('a', base::system)"))
        })

test_that('get checks', {
            expect_error(sandbox("get('system')"))
            expect_error(sandbox("get('base::system')"))
        })


context('checking normal behavior')

test_that('called functions', {
            expect_output(sandbox("as.formula('1~1')"), '.*')
            expect_output(sandbox("lm(mtcars)"), '.*')
            expect_output(sandboxR:::model.frame.masked(mtcars), '.*')
            expect_output(sandboxR:::as.formula.masked('1~1'), '.*')
            expect_output(sandboxR:::as.formula.masked(1~1), '.*')
            expect_output(sandboxR:::formula.masked('1~1'), '.*')
            expect_output(sandboxR:::formula.masked(1~1), '.*')
            expect_output(sandboxR:::paste.masked(letters), '.*')
            expect_output(sandbox("(get)('mtcars')"), '.*')
            expect_output(sandbox("(`get`)('mtcars')"), '.*')
            expect_output(sandbox("x <- (get)"), '.*')
            expect_output(sandbox("eval(mtcars)"), '.*')
            expect_output(sandbox("assign('a', get); a"), '.*')
            expect_output(sandbox("a <- 2; a"), '2')
        })
