\name{format_signif}
\alias{format_signif}
\title{format_signif}
\usage{
  format_signif(x, digits)
}
\arguments{
  \item{x}{numeric vector}

  \item{digits}{number of significant digits}
}
\value{
  character vector
}
\description{
  Format numbers with a given number of significant digits
}
\examples{
x <- c(0.800, 1704.1, 43.5e6, 10, NaN, NA, Inf, 0.00)
format_signif(x, digits=3)
}

