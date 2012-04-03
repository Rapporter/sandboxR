\name{sandbox}
\alias{sandbox}
\title{Eval in sandbox}
\usage{
  sandbox(src)
}
\arguments{
  \item{src}{character vector of R commands}
}
\description{
  Eval in sandbox
}
\examples{
\dontrun{
sandbox('paste(rev(c(")", "whatever", "(", "m", "e", "t", "s", "y", "s")), sep = "", collapse = "")')
sandbox('get(paste("","y", "tem", sep="s"))("whoami")')
sandbox(c("x1 <- 's'", "x2 <- 'y'", "x3 <- 't'", "x4 <- 'e'", "x5 <- 'm'", "x <- paste(x1, x2, x1, x3, x4, x5, sep = '')", "lm(sprintf(\\"\%s('echo hello > /tmp/xxx') ~ 1\\", x))"))
sandbox('paste("as.numeric(system(\\'ls -la | wc -l\\', intern=T)) ~ 1")')
sandbox(c("x <- system", "x('ls')"))
sandbox('lm("as.numeric(system(\\'ls -la | wc -l\\', intern=T)) ~ 1")')
}
}

