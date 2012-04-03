\name{paste.masked}
\alias{paste.masked}
\title{Masked paste}
\usage{
  paste.masked(..., sep = "", collapse = NULL)
}
\arguments{
  \item{...}{see \code{paste}}

  \item{sep}{see \code{paste}}

  \item{collapse}{see \code{paste}}
}
\description{
  Checks for forbidden function calls in constructed
  character vector before returning result.
}
\examples{
\dontrun{
paste.masked('sys', 'tem', '(', sep = '')
paste.masked('xsx sax s system( dasf asf as url(')
paste.masked(c(letters[c(19, 25, 19, 20, 5, 13)], '('), collapse = "")
}
}
\seealso{
  commands.blacklist
}

