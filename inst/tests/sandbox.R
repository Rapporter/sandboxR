context('filtering blacklisted functions')

test_that('called functions', {
            expect_error(sandbox('system("cat /etc/passwd")'))
            expect_error(sandbox('get(paste("","y", "tem", sep="s"))("whoami")'))
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
        })
