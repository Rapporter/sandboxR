\name{commands.blacklist}
\alias{commands.blacklist}
\title{Blacklisted functions}
\usage{
  commands.blacklist(pkg)
}
\arguments{
  \item{pkg}{package name(s) where to look for blacklisted
  functions. All packages' functions will be returned in a
  list if not set.}
}
\value{
  vector or list of function names
}
\description{
  Blacklisted functions
}
\note{
  Only base is added ATM.
}
\examples{
\dontrun{
commands.blacklist()
commands.blacklist('base')
}
}

