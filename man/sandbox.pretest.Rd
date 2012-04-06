\name{sandbox.pretest}
\alias{sandbox.pretest}
\title{Checking expressions for malicious code}
\usage{
  sandbox.pretest(src,
    blacklist = as.character(unlist(commands.blacklist())))
}
\arguments{
  \item{src}{character vector of R commands}

  \item{blacklist}{character vector of function names which
  should be banned}
}
\value{
  invisibly \code{TRUE} if tests passed
}
\description{
  This function tests a character vector of R commands
  agains a list of banned functions and \code{stop}s if any
  found.
}

