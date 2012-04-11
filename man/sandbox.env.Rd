\name{sandbox.env}
\alias{sandbox.env}
\title{Sandboxed environment}
\usage{
  sandbox.env(blacklist = as.character(unlist(commands.blacklist())))
}
\arguments{
  \item{blacklist}{character vector of function names which
  should be banned}
}
\value{
  environment
}
\description{
  This function returns a special environment pre-loaded
  with bunch of forked functions from \code{base},
  \code{stats}, \code{graphics} etc. to act as a jail for
  later evaluation.
}
\details{
  Some of the forked functions \code{stop}s by default (to
  prevent using those inside the environment), for a full
  list see \code{\link{commands.blacklist}}. Other
  functions (found in package's namespace) behaves
  differently then usual: some parameters are forbidden
  (like \code{eval}'s \code{env}) to prevent breaking out
  from the sandbox, some parameters and returned values are
  checked for "malicious" signs.
}

